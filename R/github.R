#' Create Shortlink for a GitHub URL using git.io
#'
#' @param url The GitHub URL to convert to a shortlink
#' @param vanity_code Request a specific shortcode instead of the randomly
#'   generated name. Helpful for easily sharing a link verbally.
#' @return A (hopefully) shorter URL
#' @export
shorten_github_url <- function(url, vanity_code = NULL) {
  if (!requireNamespace("curl", quietly = TRUE)) return(url)
  h <- curl::new_handle()
  if (!is.null(vanity_code)) {
    curl::handle_setform(h, url = url, code = vanity_code)
  } else curl::handle_setform(h, url = url)
  r <- curl::curl_fetch_memory("https://git.io", h)
  if (!r$status %in% 200:203) {
    if (requireNamespace("httr", quietly = TRUE)) httr::warn_for_status(r$status)
    return(url)
  }
  short_url <- curl::parse_headers_list(r$headers)$location
  if (!is.null(short_url) && grepl("git\\.io", short_url)) short_url else url
}
