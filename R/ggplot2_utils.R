#' Save and/or Print Plot
#'
#' A thin wrapper around [ggplot2::ggsave()] that takes a plot as the first
#' argument and `filename` as the second (the reverse of [ggplot2::ggsave()]),
#' but that doesn't save the plot when called interactively. Works best inside R
#' Markdown documents, allowing you to view the figure when running the code
#' chunk interactively but ensure that a specific copy of the figure is saved to
#' a specific directory during the markdown rendering.
#'
#' @examples
#' \dontrun{
#' iris %>%
#' {
#'   ggplot(.) +
#'     aes(Sepal.Length, Sepal.Width, color = Species) +
#'     geom_point()
#' } %>%
#' ggsave_and_print("iris_plot.png", width = 10, height = 6)
#' }
#'
#' @param include Mimics the [knitr] `include` chunk option. If `FALSE` then the
#'   plot is saved but not printed. If `TRUE`, then the plot is saved and
#'   printed.
#' @param force_save If `TRUE` the plot is saved, even if inside an interactive
#'   session. The default is `FALSE`, meaning that plots are not saved when
#'   in interactive mode.
#' @param base_dir Save figures into a specified directory
#' @inheritParams ggplot2::ggsave
#' @inheritDotParams ggplot2::ggsave
#' @export
ggsave_and_print <- function(plot,
                                filename,
                                width = 20,
                                height = 15,
                                device = fs::path_ext(filename),
                                include = knitr::opts_current$get("include") %||% TRUE,
                                ...,
                                force_save = FALSE,
                                base_dir = getwd()
) {
  if (interactive() && !force_save) return(plot)

  filename <- fs::path(base_dir, filename)
  filename <- fs::path_ext_set(filename, device)

  fs::dir_create(fs::path_dir(filename))

  ggsave(plot, filename = filename, width = width, height = height, device = device, ...)

  if (include) print(plot)

  invisible(plot)
}
