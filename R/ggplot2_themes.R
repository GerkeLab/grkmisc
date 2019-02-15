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
#' @param use_showtext Should [showtext] and [sysfonts] be used to register
#'   font families from Google? Default is `TRUE`.
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
  panel_background_color = "#FFFFFF",
  ...,
  use_showtext = TRUE,
  hide_panel_grid_minor = TRUE
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
    use_showtext       = use_showtext,
    panel_background_color = panel_background_color,
    hide_panel_grid_minor  = hide_panel_grid_minor,
    ...
  )
}

#' @rdname theme_moffitt
#' @export
theme_grk <- function(
  base_family        = "PT Sans",
  axis_text_family   = "PT Mono",
  axis_title_family  = base_family,
  axis_title_bold    = FALSE,
  axis_title_just    = "cc",
  axis_title_size    = 13,
  subtitle_size      = 13,
  default_geom_font  = "PT Sans Narrow",
  default_geom_color = grkmisc::moffitt_colors$blue,
  axis_text_color    = "#6e6e6e",
  plot_caption_color = axis_text_color,
  panel_border_color = axis_text_color,
  panel_background_color = "grey96",
  ...,
  use_showtext = TRUE,
  hide_panel_grid_minor = TRUE
) {
  axis_title_family_name <- axis_title_family
  # Get and set fonts - this works across all devices
  has_showtext <- requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)
  if (use_showtext && has_showtext) {
    axis_title_family_name <- if (axis_title_bold) {
      paste(sub(" ?([bB]old|[sS]emi-?[bB]old)", "", axis_title_family), "Bold")
    } else axis_title_family

    try_font_add_google(base_family, regular.wt = 400, bold.wt = 600)
    if (axis_title_family_name != base_family)
      try_font_add_google(axis_title_family, axis_title_family_name, regular.wt = 600)
    try_font_add_google(default_geom_font)
    try_font_add_google(axis_text_family)
    showtext::showtext_auto()
  } else {
    if (!isTRUE(getOption("grkmisc.warned_install_showtext"))) {
      rlang::warn("For consistent font display, `install.packages(c(\"sysfonts\", \"showtext\")")
      options("grkmisc.warned_install_showtext" = TRUE)
    }
  }

  if (!is.null(default_geom_font)) hrbrthemes::update_geom_font_defaults(default_geom_font)
  if (!is.null(default_geom_color)) update_geom_moffitt_defaults(default = default_geom_color)

  theme <-
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
        family = axis_text_family, color = axis_text_color, inherit.blank = TRUE),
      plot.caption = ggplot2::element_text(
        family = base_family, face = "plain", color = plot_caption_color, inherit.blank = TRUE),
      panel.background = ggplot2::element_rect(fill = panel_background_color),
      panel.border = ggplot2::element_rect(fill = NA, color = panel_border_color, inherit.blank = TRUE)
    )

  if (hide_panel_grid_minor) theme <- theme + theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

  theme
}

# Try to get font from Google, otherwise returns null
try_font_add_google <- function(font, ...) {
  x <- purrr::possibly(sysfonts::font_add_google, NULL)(font, ...)
  if (is.null(x)) rlang::warn(
    glue::glue('Font "{font}" wasn\'t found on Google Fonts, but may be installed locally.')
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

# ---- Scales ----

moffitt_pal <- function(color_other = "grey", direction = 1) {
  function(n) {
    if (n > 7) rlang::warn("Moffitt Color Palette only has 7 colors.")

    x <- if (n == 2) {
      color_other <- if (!color_other %in% names(grkmisc::moffitt_colors)) {
        color_other
      } else grkmisc::moffitt_colors[color_other]
      c(grkmisc::moffitt_colors["blue"], color_other)
    } else grkmisc::moffitt_colors[1:n]

    x <- unname(unlist(x))
    if (direction > 0) x else rev(x)
  }
}

#' Moffitt Color Scales for ggplot2
#'
#' Color scales based on the Moffitt Branding Guidelines, 2014.
#'
#' @seealso [moffitt_colors] [theme_moffitt()]
#' @inheritDotParams ggplot2::discrete_scale
#' @param color_other When the data contains two values, the second value takes
#'   this color. Can be any of the colors in [moffitt_colors] other than blue:
#'   green, red, orange, light_blue, yellow, or grey (default).
#' @param direction Reverses the direction of the color scale when `direction`
#'   is less than 0, i.e. -1.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(vs)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt()
#'
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(vs)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt(color_other = "green")
#'
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(carb)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt()
#'
#' dplyr::count(mpg, class, sort = TRUE) %>%
#'   dplyr::mutate(class = factor(class, levels = class)) %>%
#'   ggplot() +
#'   aes(class, n, fill = class) +
#'   geom_col() +
#'   coord_flip() +
#'   theme_moffitt() +
#'   scale_fill_moffitt()
#'
#' @name scale_moffitt
#' @export
scale_colour_moffitt <- function(color_other = "grey", direction = 1, ...) {
  ggplot2::discrete_scale("colour", "moffitt", moffitt_pal(color_other, direction), ...)
}

#' @name scale_moffitt
#' @export
scale_color_moffitt <- scale_colour_moffitt

#' @name scale_moffitt
#' @export
scale_fill_moffitt <- function(color_other = "grey", direction = 1, ...) {
  ggplot2::discrete_scale("fill", "moffitt", moffitt_pal(color_other, direction), ...)
}

# ---- Documents ----

#' Create New Document
#'
#' Creates a new R Markdown document of the requested type. There are currently
#' three templates, one for reports where the default is HTML based on the HTML
#' vignette template, and another with a Moffitt-styled [xaringan] theme. There
#' is also a [radix] template for stunning HTML-only reports. In all cases, the
#' document and supporting files are added to a directory with the name given by
#' the file.
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
doc_new <- function(path, type = c("doc", "slides", "radix")) {
  type <- match.arg(type)
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  file_path <- switch(
    type,
    "doc"    = rmarkdown::draft(path, template = "default", package = "grkmisc", edit = FALSE),
    "slides" = rmarkdown::draft(path, template = "moffitt-xaringan", package = "grkmisc", edit = FALSE),
    "radix"  = rmarkdown::draft(path, template = "grk-radix", package = "grkmisc", edit = FALSE)
  )
  if (type == "radix" && !requireNamespace("radix", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      rlang::abort("Please install 'remotes' with `install.packages(\"remotes\")`")
    }
    message("Installing radix from rstudio/radix...")
    remotes::install_github("radix/rstudio")
  }
  if (requireNamespace("rstudioapi", quietly = TRUE))
    rstudioapi::navigateToFile(file_path) else file_path
}
