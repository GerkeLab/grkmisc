#' A Clean Theme with Moffitt Style
#'
#' A clean theme based on [hrbrthemes::theme_ispum()] that uses
#' [Overpass](https://fonts.google.com/specimen/Overpass), a free Google Font
#' that resembles Freeway, the (proprietary) font chosen by MCC branding. This
#' theme works best when set globally using [ggplot2::theme_set()] as it will
#' automatically download and register the correct fonts from Google Font using
#' [sysfonts] and [showtext] (if installed) and will change the default
#' [ggplot2::geom_text()] related fonts as well. For an alternative look that I
#' prefer, use `theme_grk()`, which simply uses "Fira Sans" as the base font.
#'
#' @examples
#' \dontrun{
#' theme_set(theme_moffitt())
#' theme_set(theme_grk())
#' # Set base theme without changing geom defaults
#' theme_set(theme_moffitt(default_geom_font = NULL, default_geom_color = NULL))
#' }
#'
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   geom_text(aes(label = cyl), vjust = 0.5, hjust = 1.65) +
#'   labs(x = "Fuel efficiency (mpg)",
#'        y = "Weight (tons)",
#'        title = "Seminal ggplot2 scatterplot example",
#'        subtitle = "A plot that is only useful for demonstration purposes",
#'        caption = "Brought to you by the letter 'g'")
#' g + theme_moffitt()
#' g + theme_moffitt(default_geom_color = moffitt_colors$green)
#' g + theme_grk()
#'
#' @inheritParams hrbrthemes::theme_ipsum
#' @inheritDotParams hrbrthemes::theme_ipsum
#' @param default_color Changes default colors of bars and points to
#'   the value given. Set to `NULL` to avoid changing these colors. Note that
#'   these colors are used only when color or fill is not mapped to the data.
#' @param default_geom_font Change the default ggplot2 geom fonts to the
#'   specified font. The default is "Fira Sans Condensed", which tends to look
#'   good in constrained space.
#' @param axis_text_family The font family for axis ticks text labels.
#'   Passed to `family` in `axis.text` in [ggplot2::theme()].
#' @param axis_title_bold If `TRUE`, the axis title's will be bold.
#' @param axis_text_color Color of axis text
#' @param plot_caption_color Color of the plot caption text
#' @param panel_border_color Color of the panel border
#' @export
theme_moffitt <- function(
  base_family        = "Overpass",
  axis_text_family   = "Overpass Mono",
  axis_title_family  = base_family,
  axis_title_bold    = FALSE,
  axis_title_just    = "cc",
  axis_title_size    = 13,
  subtitle_size      = 13,
  default_geom_font  = "Fira Sans Condensed",
  default_geom_color = grkmisc::moffitt_colors$blue,
  axis_text_color    = "#6e6e6e",
  plot_caption_color = axis_text_color,
  panel_border_color = axis_text_color,
  ...
) {
  theme_grk(
    base_family        = base_family,
    axis_text_family   = axis_text_family,
    axis_title_family  = axis_title_family,
    axis_title_bold    = axis_title_bold,
    axis_title_just    = axis_title_just,
    axis_title_size    = axis_title_size,
    subtitle_size      = subtitle_size,
    default_geom_font  = default_geom_font,
    default_geom_color = default_geom_color,
    axis_text_color    = axis_text_color,
    plot_caption_color = plot_caption_color,
    panel_border_color = panel_border_color,
    ...
  )
}

#' @rdname theme_moffitt
#' @export
theme_grk <- function(
  base_family        = "Fira Sans",
  axis_text_family   = paste(sub(" ?(Mono|Sans|Serif)", "", base_family), "Mono"),
  axis_title_family  = base_family,
  axis_title_bold    = FALSE,
  axis_title_just    = "cc",
  axis_title_size    = 13,
  subtitle_size      = 13,
  default_geom_font  = "Fira Sans Condensed",
  default_geom_color = grkmisc::moffitt_colors$blue,
  axis_text_color    = "#6e6e6e",
  plot_caption_color = axis_text_color,
  panel_border_color = axis_text_color,
  ...
) {
  axis_title_family_name <- if (axis_title_bold) {
    paste(sub(" ?([bB]old|[sS]emi-?[bB]old)", "", axis_title_family), "Bold")
  } else axis_title_family

  # Get and set fonts - this works across all devices
  has_showtext <- requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)
  if (has_showtext) {
    sysfonts::font_add_google(base_family, regular.wt = 200, bold.wt = 400)
    if (axis_title_family_name != base_family)
      sysfonts::font_add_google(axis_title_family, axis_title_family_name, regular.wt = 600)
    sysfonts::font_add_google(default_geom_font)
    sysfonts::font_add_google(axis_text_family)
    showtext::showtext_auto()
  } else {
    if (!isTRUE(getOption("grkmisc.warned_install_showtext"))) {
      rlang::warn("For consistent font display, `install.packages(c(\"sysfonts\", \"showtext\")")
      options("grkmisc.warned_install_showtext" = TRUE)
    }
  }

  if (!is.null(default_geom_font)) hrbrthemes::update_geom_font_defaults(default_geom_font)
  if (!is.null(default_geom_color)) update_geom_moffitt_defaults(default = default_geom_color)

  hrbrthemes::theme_ipsum(
    base_family = base_family,
    subtitle_size = subtitle_size,
    axis_title_size = axis_title_size,
    axis_title_family = axis_title_family_name,
    axis_title_just = axis_title_just,
    ...
  ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        family = axis_text_family, color = axis_text_color),
      plot.caption = ggplot2::element_text(
        family = base_family, face = "plain", color = plot_caption_color),
      panel.background = ggplot2::element_rect(fill = NA, color = panel_border_color)
    )
}

update_geom_moffitt_defaults <- function(
  geom = c("bar" = "fill", "col" = "fill", "dotplot" = "color", "point" = "color"),
  default = grkmisc::moffitt_colors$blue
) {
  purrr::iwalk(geom, ~ .update_geom_default(.y, .x, value = default))
}

.update_geom_default <- function(geom, attr, value) {
  ggplot2::update_geom_defaults(geom, setNames(list(value), attr))
}



# ---- Documents ----

#' Create New Document
#'
#' Creates a new R Markdown document of the requested type. There are currently
#' two templates, one for reports where the default is HTML based on the
#' HTML vignette template, and another with a Moffitt-styled xaringan theme.
#' In both cases, the document and supporting files are added to a directory
#' with the name given by the file.
#'
#' @examples
#' \dontrun{
#' doc_new("my_report.Rmd", "doc")
#' doc_new("my_slides.Rmd", "slides")
#' }
#'
#' @param path Path to the location of the new document
#' @param type Type of document to create
#' @export
doc_new <- function(path, type = c("doc", "slides")) {
  type <- match.arg(type)
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  file_path <- switch(
    type,
    "doc" = rmarkdown::draft(path, template = "default", package = "grkmisc", edit = FALSE),
    "slides" = rmarkdown::draft(path, template = "moffitt-xaringan", package = "grkmisc", edit = FALSE)
  )
  if (requireNamespace("rstudioapi", quietly = TRUE))
    rstudioapi::navigateToFile(file_path) else file_path
}
