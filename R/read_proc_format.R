# ---- Helpers to strip leading/trailing elements from strings ----
trim_leading <- function(x, regex = "\\s+") {
  sub(paste0("^", regex), "", x)
}
trim_trailing <- function(x, regex = "\\s+") {
  sub(paste0(regex, "$"), "", x)
}

trim_both <- function(x, regex = "\\s+") trim_leading(trim_trailing(x, regex), regex)

# ---- Regexes ----
rx_format <- "\\*\\* FORMAT: (?<format>[\\S\\s]+?) \\*\\*"
rx_varname <- "\\*\\* FOR VARIABLE: (?<varname>[\\S\\s]+?) \\*\\*"
rx_vartype <- "\\*\\* (?<vartype>[A-Z ]+) VARIABLE"
rx_value <- "(^|\n)(?<value>(value|VALUE)[\\S\\s]+)"
rx_sas_name <- "\\$?[a-zA-Z_][a-zA-Z0-9_]{0,31}"
rx_value_var <- paste("^\\s*(VALUE|value)", rx_sas_name)

# ---- Helpers to read proc format statements ----
read_proc_format_statements <- function(file) {
  pf_raw <- readLines(file)
  pf_raw <- paste(pf_raw, collapse = "\n")

  # Only lines inside `proc format ...;` and `run;`
  regex_format_inner <- "(proc format|PROC FORMAT).*;\\s*(?<pf>[\\S\\s]+?)\\s*(run|RUN);"
  pf_statements <- rematch2::re_match(pf_raw, regex_format_inner)$pf

  # Split into statements
  pf_statements <- strsplit(pf_statements, ";")[[1]]
  pf_statements <- trim_leading(pf_statements)

  # Add semicolons to group format and value statements
  pf_statements <- sub("^(value.+)", "\\1;", pf_statements)
  pf_statements <- paste(pf_statements, collapse = "\n")
  pf_statements <- strsplit(pf_statements, ";")[[1]]
  trim_leading(pf_statements)
}


extract_statement <- function(pf_statement) {
  pf_format  <- rematch2::re_match(pf_statement, rx_format)[, 'format']
  pf_varname <- rematch2::re_match(pf_statement, rx_varname)[, 'varname']
  pf_vartype <- rematch2::re_match(sub(rx_varname, "", pf_statement, perl = TRUE), rx_vartype)[, 'vartype']
  pf_value   <- rematch2::re_match(pf_statement, rx_value)[, 'value']
  dplyr::bind_cols(
    pf_format,
    pf_varname,
    pf_vartype,
    pf_value
  ) %>%
    dplyr::mutate(vartype = guess_vartype(vartype))
}

guess_vartype <- function(vartype) {
  vartype[grepl("character", tolower(vartype))] <- "character"
  vartype[grepl("numeric", tolower(vartype))] <- "numeric"
  vartype[grepl("date", tolower(vartype))] <- "datetime"
  vartype[grepl("iso", tolower(vartype))] <- "datetime"
  vartype[is.na(vartype)] <- "factor"
  vartype
}

expand_varnames <- function(pfdf) {
  # takes output from expand_statement where varnames may be like `x0-5`
  # expands varname to be `x0`, `x1`, ..., `x5`
  #
  # Does variable expansion by format name because this will be unique (1 row)

  pfdf_names <- names(pfdf)

  pfdf %>%
    split(.$format) %>%
    purrr::map_df(~ cbind(
      .[, setdiff(pfdf_names, "varname")],
      dplyr::data_frame(varname = expand_varname(.$varname))
    )) %>%
    dplyr::as_tibble() %>%
    dplyr::select(pfdf_names)
}

expand_varname <- function(varname) {
  # Expands a string like `"x0-1"` to `c("x0", "x1", "x2")`
  if (!grepl("\\d-\\d", varname)) return(varname)

  idx_num <- regexec("\\d+-\\d", varname)[[1]][1]
  var_base    <- substring(varname, 1, idx_num - 1)
  var_indexes <- strsplit(substring(varname, idx_num), "-")[[1]]
  var_indexes <- try({
    as.integer(var_indexes)
  })
  if (!is.integer(var_indexes) || length(var_indexes) != 2) {
    message("Check: ", varname)
    return(varname)
  }
  paste0(var_base, var_indexes[1]:var_indexes[2])
}

null_vartype <- function(vartype) {
  switch(
    vartype,
    character = character(0),
    numeric = double(0),
    datetime = as.POSIXct(character(0)),
    factor(0)
  )
}

labelize_values <- function(
  pf_value, pf_vartype,
  missing_values = paste0(".", c("", 1:9, LETTERS))
) {
  # "value $plco_idf\n  " -> c()
  # "value assaydaysf\n  " -> c()
  # "value pctfpsaf\n    .N = \"N/A\"\n  " -> c("N/A" = NA)
  # 'value draw_syv\n    0 = \"(0) T0\"\n    1 = \"(1) T1\"' -> c("(0) T0" = 0, "(1) T1" = 1)
  # " VALUE $formato2 'RED'='Rosso'\n 'YELLOW'='Giallo'\n 'BLUE'='Blu';" -> c("Rosso" = "RED", ...)
  pfv <- trim_leading(pf_value, rx_value_var)
  if (trim_both(pfv) == "") return(null_vartype(pf_vartype))

  pfv <- pfv %>%
    strsplit(split = "\n") %>%
    .[[1]] %>%
    purrr::map_chr(trim_both) %>%
    purrr::map(~ strsplit(., "=")[[1]]) %>%
    purrr::keep(~ length(.) > 0) %>%
    purrr::map(insert_quoted_equals) %>%
    purrr::transpose() %>%
    purrr::imap(~ safe_value(.x, .y == 2)) %>%
    purrr::reduce_right(paste, collapse = ", ", sep = " = ") %>%
    paste0("c(", ., ")")

  eval(parse(text = pfv))
}

# Fixes issue where an equals character might be inside double quotes, in
# which case when splitting on "=" there may be an extra (or multiple splits).
# This function puts equals back inside *double quotes*. Doesn't work with
# single quotes.
insert_quoted_equals <- function(x) {
  q_count <- stringr::str_count(x, "\"")
  if (sum(q_count) == 0) return(x)
  if (sum(q_count) %% 2 != 0) {
    rlang::warn(paste0("Mismatched quotes in string `", x, "`"))
  }

  q_idx <- which(stringr::str_count(x, "\"") == 1)
  while (length(q_idx) >= 2) {
    x <- c(x[0:(q_idx[1] - 1)],
           paste(x[q_idx[1]:q_idx[2]], collapse = "="),
           if (q_idx[2] < length(x)) x[(q_idx[2] + 1):length(x)]
    )
    q_idx <- which(stringr::str_count(x, "\"") == 1)
  }
  x
}

convert_to_missing <- function(values, missing_values = paste0(".", c("", 1:9, LETTERS))) {
  values[values %in% missing_values] <- NA
  values
}

safe_value <- function(x, force_wrap = FALSE, missing_values = paste0(".", c("", 1:9, LETTERS))) {
  x <- trim_both(x)
  x <- convert_to_missing(x, missing_values)

  # Already wrapped in quotes
  if (all(grepl("^\"|\"$", x))) return(x)

  # Some are wrapped in quotes -> wrap all
  if (force_wrap || any(grepl("^\"|\"$", x))) {
    x[!grepl("^\"|\"$", x)] <- paste0('"', x[!grepl("^\"|\"$", x)], '"')
    return(x)
  }

  # Not wrapped in quotes, are there non-numeric?
  if (any(grepl("[a-zA-Z_$%^&*@!+=, ]", x))) {
    paste0('"', x, '"')
  } else x
}


#' Read Proc Format File
#'
#' @param file Path to proc format file.
#' @param verbose Writes out file path being read. May be removed in the future.
#' @param missing_values Possible SAS values that should be treated as missing.
#' @examples
#' \dontrun{
#' read_proc_format("freepsa.sas_formats.feb16.d080516.sas")
#' }
#'
#' @family SAS helper functions
#' @export
read_proc_format <- function(
  file, verbose = FALSE,
  missing_values = paste0(".", c("", 1:9, LETTERS))
) {
  if (verbose) cli::cat_bullet("Reading proc format: ", file)
  read_proc_format_statements(file) %>%
    purrr::map_df(extract_statement) %>%
    dplyr::mutate(label = purrr::map2(value, vartype, labelize_values, missing_values = missing_values)) %>%
    expand_varnames()
}

#' Add labels from proc_format file to SAS data file
#'
#' @param df Input data frame read from [haven::read_sas]
#' @param proc_format Either path to SAS file with `proc format` statements or
#'   the results of [read_proc_format].
#' @param varname_case SAS variable names are not case-sensitive, but R variable
#'   names are. Choose one of `"lower"` or `"upper"` case to normalize the
#'   variable names in `df` and the `varnames` column of `proc_format`. Or set
#'   equal to NULL to leave both as-is.
#' @param as_factor Convert labelled variables to factor with [haven::as_factor]?
#' @param debug_level Default is `0`. Level `1` prints confirmation of reading
#'   `proc_format` file. Level `2` prints confirmation that the labels were
#'   applied to each variable in `df`.
#' @inheritDotParams read_proc_format
#' @examples
#' \dontrun{
#' bdat <- haven::read_sas("freepsa_data_feb16_d080516.sas7bdat")
#' bdat2 <- add_proc_format_labels(bdat, "freepsa.sas_formats.feb16.d080516.sas")
#' }
#'
#' @family SAS helper functions
#' @export
add_proc_format_labels <- function(
  df,
  proc_format,
  as_factor = FALSE,
  varname_case = c("lower", "upper"),
  debug_level = 0,
  ...
) {
  if (is.character(proc_format)) {
    if (length(proc_format) == 1) {
      proc_format <- read_proc_format(proc_format, debug_level > 0, ...)
    } else {
      rlang::abort(paste("`proc_format` must be a path to SAS proc format file,",
                   "or the result of reading such a file from `read_proc_format()`"))
    }
  }
  verbose <- debug_level > 1

  if (any(duplicated(proc_format$varname))) {
    rlang::warn("Duplicate variables found in `proc_format`, using first defined.")
    proc_format <- filter(proc_format, !duplicated(varname))
  }

  varname_case <- if (!is.null(varname_case)) match.arg(varname_case) else "default"

  varname_transformer <- switch(
    varname_case,
    "lower" = tolower,
    "upper" = toupper,
    I
  )

  names(df) <- varname_transformer(names(df))
  proc_format$varname <- varname_transformer(proc_format$varname)

  pf <- purrr::set_names(
    proc_format$label,
    proc_format$varname
  ) %>%
    # Only apply labels if non-missing
    purrr::keep(~ length(.[!is.na(.)]) > 0)

  safely_label <- purrr::safely(labelled::labelled)

  for (var in names(pf)) {
    var_labelled <- safely_label(df[[var]], pf[[var]])
    if (is.null(var_labelled$error)) {
      if (verbose) cli::cat_bullet("Applying labels to variable ", var, bullet = "continue")
      df[[var]] <- if (as_factor) {
        haven::as_factor(var_labelled$result)
      } else var_labelled$result
    } else {
      if (verbose) cli::cat_bullet(
        "Unable to apply label to ",
        crayon::bold(var), ": ",
        trim_both(var_labelled$error),
        bullet = "cross", bullet_col = "red")
    }
  }

  df
}


#' Read SAS file and label according to PROC FORMAT
#'
#' Reads a SAS `sas7bdat` file using [haven::read_sas] and then applies the
#' labels given in the formats file (containing `PROC FORMAT` statements).
#' The labels use [labelled::labelled] and may be coerced to factors if desired.
#'
#' @param file_bdat Path to the `sas7bdat` file to be read by [haven::read_sas]
#' @param file_format Path to the SAS formats file to be read by [read_proc_format]
#' @inheritParams read_proc_format
#' @inheritParams add_proc_format_labels
#' @inheritDotParams haven::read_sas
#' @examples
#' \dontrun{
#' read_sas_with_format("freepsa_data_feb16_d080516.sas7bdat", "freepsa.sas_formats.feb16.d080516.sas")
#' }
#'
#' @family SAS helper functions
#' @export
read_sas_with_format <- function(
  file_bdat,
  file_format,
  as_factor = FALSE,
  missing_values = paste0(".", c("", 1:9, LETTERS)),
  debug_level = 0,
  ...
) {
  if (debug_level > 0) cli::cat_bullet("Reading SAS file: ", file_bdat)
  bdat <- haven::read_sas(file_bdat, ...)

  add_proc_format_labels(
    bdat, file_format,
    as_factor = as_factor, debug_level = debug_level, missing_values = missing_values
  )
}
