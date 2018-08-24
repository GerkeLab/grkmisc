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
  pf_format <- rematch2::re_match(pf_statement, rx_format)[, 'format']
  pf_varname <- rematch2::re_match(pf_statement, rx_varname)[, 'varname']
  pf_vartype <- rematch2::re_match(sub(rx_varname, "", pf_statement, perl = TRUE), rx_vartype)[, 'vartype']
  pf_value <- rematch2::re_match(pf_statement, rx_value)[, 'value']
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

null_vartype <- function(vartype) {
  switch(
    vartype,
    character = character(0),
    numeric = double(0),
    datetime = as.POSIXct(character(0)),
    factor(0)
  )
}

labelize_values <- function(pf_value, pf_vartype, missing_values = c(".N")) {
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

  convert_to_missing(eval(parse(text = pfv)), missing_values)
}

#' Fixes issue where an equals character might be inside double quotes, in
#' which case when splitting on "=" there may be an extra (or multiple splits).
#' This function puts equals back inside *double quotes*. Doesn't work with
#' single quotes.
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

convert_to_missing <- function(values, missing_values = c(".N")) {
  values[values %in% missing_values] <- NA
  values
}

safe_value <- function(x, force_wrap = FALSE) {
  x <- trim_both(x)

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
#' @examples
#' read_proc_format("/Volumes/Lab_Gerke/PLCO/Free PSA/freepsa.sas_formats.feb16.d080516.sas")
#'
#' @export
read_proc_format <- function(file, verbose = FALSE) {
  if (verbose) cli::cat_bullet("Reading proc format: ", file)
  read_proc_format_statements(file) %>%
    purrr::map_df(extract_statement) %>%
    dplyr::mutate(label = purrr::map2(value, vartype, labelize_values))
}

#' Add labels from proc_format file to SAS data file
#'
#' @param df Input data frame read from [haven::read_sas]
#' @param proc_format Either path to SAS file with `proc format` statements or
#'   the results of [read_proc_format].
#' @inheritDotParams read_proc_format
#' @examples
#' bdat <- haven::read_sas("/Volumes/Lab_Gerke/PLCO/Free PSA/freepsa_data_feb16_d080516.sas7bdat")
#' bdat2 <- add_proc_format_labels(bdat, "/Volumes/Lab_Gerke/PLCO/Free PSA/freepsa.sas_formats.feb16.d080516.sas")
#'
#' @export
add_proc_format_labels <- function(df, proc_format, ...) {
  if (is.character(proc_format)) {
    if (length(proc_format) == 1) {
      proc_format <- read_proc_format(proc_format, ...)
    } else {
      rlang::abort(paste("`proc_format` must be a path to SAS proc format file,",
                   "or the result of reading such a file from `read_proc_format()`"))
    }
  }

  pf <- purrr::set_names(
    proc_format$label,
    proc_format$varname
  ) %>%
    # Only apply labels if non-missing
    purrr::keep(~ length(.[!is.na(.)]) > 0)

  for (var in names(pf)) {
    df <- apply_label_to_var(df, var, pf[[var]])
  }

  df
}

apply_label_to_var <- function(df, var, labels) {
  df[[var]] <- labelled::labelled(df[[var]], labels)
  df
}
