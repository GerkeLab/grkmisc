trim_leading <- function(x, regex = "\\s+") {
  sub(paste0("^", regex), "", x)
}
trim_trailing <- function(x, regex = "\\s+") {
  sub(paste0(regex, "$"), "", x)
}

trim_both <- function(x, regex = "\\s+") trim_leading(trim_trailing(x, regex), regex)

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
  rx_format <- "\\*\\* FORMAT: (?<format>[\\S\\s]+?) \\*\\*"
  rx_varname <- "\\*\\* FOR VARIABLE: (?<varname>[\\S\\s]+?) \\*\\*"
  rx_vartype <- "\\*\\* (?<vartype>[A-Z ]+) VARIABLE"
  rx_value <- "(^|\n)(?<value>(value|VALUE)[\\S\\s]+)"

  pf_format <- rematch2::re_match(pf_statement, rx_format)[, 'format']
  pf_varname <- rematch2::re_match(pf_statement, rx_varname)[, 'varname']
  pf_vartype <- rematch2::re_match(sub(rx_varname, "", pf_statement, perl = TRUE), rx_vartype)[, 'vartype']
  pf_value <- rematch2::re_match(pf_statement, rx_value)[, 'value']
  dplyr::bind_cols(
    pf_format,
    pf_varname,
    pf_vartype,
    pf_value
  )
}

labelize_values <- function(pf_value) {
  # "value $plco_idf\n  " -> c()
  # "value assaydaysf\n  " -> c()
  # "value pctfpsaf\n    .N = \"N/A\"\n  " -> c("N/A" = NA)
  # 'value draw_syv\n    0 = \"(0) T0\"\n    1 = \"(1) T1\"' -> c("(0) T0" = 0, "(1) T1" = 1)
  # " VALUE $formato2 'RED'='Rosso'\n 'YELLOW'='Giallo'\n 'BLUE'='Blu';" -> c("Rosso" = "RED", ...)
  rx_value_var <- paste("^\\s*(VALUE|value)", rx_sas_name)
  pfv <- trim_leading(pf_value, rx_value_var)
  if (trim_both(pfv) == "") return(character(0))

  pfv <- pfv %>%
    strsplit(split = "\n") %>%
    .[[1]] %>%
    purrr::map_chr(trim_both) %>%
    purrr::map(~ strsplit(., "=")[[1]]) %>%
    purrr::keep(~ length(.) > 0) %>%
    purrr::transpose() %>%
    purrr::imap(~ safe_value(.x, .y == 2)) %>%
    purrr::reduce_right(paste, collapse = ", ", sep = " = ") %>%
    paste0("c(", ., ")")

  eval(parse(text = pfv))
}


rx_sas_name <- "\\$?[a-zA-Z_][a-zA-Z0-9_]{0,31}"

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
#' @examples
#' read_proc_format("/Volumes/Lab_Gerke/PLCO/Free PSA/freepsa.sas_formats.feb16.d080516.sas")
#'
#' @export
read_proc_format <- function(file) {
  read_proc_format_statements(file) %>%
    purrr::map_df(extract_statement) %>%
    dplyr::mutate(label = purrr::map(value, labelize_values))
}
