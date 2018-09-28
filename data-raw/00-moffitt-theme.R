library(xaringanthemer)
duo_accent(
  outfile = here::here("inst/rmarkdown/templates/moffitt-xaringan/skeleton/moffitt-xaringan.css"),
  primary_color        = "#00589A",
  secondary_color      = "#82c878",
  inverse_header_color = "white",
  inverse_text_color   = "white",
  text_font_size       = "24px",
  text_font_google     = google_font("Overpass", "400", "300"),
  header_font_google   = google_font("Roboto Slab", "400"),
  code_font_google     = google_font("IBM Plex Mono", "300", "400"),
  text_font_weight     = "400",
  text_slide_number_font_size = "15px",
  table_row_even_background_color = "#f0f0f0",
  extra_fonts = list(
    google_font("Lora", "300", "300i", "400", "400i")
  ),
  extra_css = list(
    ".remark-slide-content" = list(padding = "0px 70px 0 50px"),
    '.title-slide' = list(
      "text-align" = "left"
    ),
    ".title-slide h1" = list(
      "margin-bottom" = "4em"
    ),
    ".pkg" = list(
      "color"             = "#53804d",
      "font-weight"       = 300,
      "font-size"         = "95%",
      "font-family"       = "IBM Plex Mono",
      padding             = "1px 4px",
      "background-color"  = "#eff4ef",
      "border-radius"     = "4px",
      "border"            = "1px solid #82c878"
    ),
    ".muted" = list(
      color = "#777"
    ),
    ".hl" = list(
      "background-color" = "rgba(255, 255, 0, 0.5)",
      padding            = "1px 4px"
    ),
    ".footer" = list(
      position = "absolute",
      bottom = "3%",
      left = "5%",
      opacity = "75%"
    ),
    ".bordered" = list(
      border = "#777 solid 2px"
    ),
    ".top" = list(
      "vertical-align" = "top"
    ),
    blockquote = list(
      "font-family" = "Lora",
      "font-weight" = 400,
      "font-style" = "italic",
      color = "#777"
    ),
    ".large" = list(
      "font-size" = "1.5em"
    ),
    ".big" = list(
      "font-size" = "2em"
    ),
    ".third" = list(
      width = "33%"
    ),
    ".two-third" = list(
      width = "66%"
    ),
    ".right-column img" = list(
      "max-height" = "35vh",
      "margin-top" = "-2em"
    ),
    "::-mox-selection" = list(
      "background-color" = "rgba(255, 255, 0, 0.5)"
    ),
    "::selection" = list(
      "background-color" = "rgba(255, 255, 0, 0.5)"
    ),
    "kbd" = list(
      "padding"               = "0.1em 0.6em",
      "border"                = "1px solid #ccc",
      "font-family"           = "Arial,Helvetica,sans-serif",
      "font-family"           = "'IBM Plex Mono',monospace",
      "background-color"      = "#f7f7f7",
      "color"                 = "#333",
      "-moz-box-shadow"       = "0 1px 0px rgba(0, 0, 0, 0.2),0 0 0 2px #ffffff inset",
      "-webkit-box-shadow"    = "0 1px 0px rgba(0, 0, 0, 0.2),0 0 0 2px #ffffff inset",
      "box-shadow"            = "0 1px 0px rgba(0, 0, 0, 0.2),0 0 0 2px #ffffff inset",
      "-moz-border-radius"    = "3px",
      "-webkit-border-radius" = "3px",
      "border-radius"         = "3px",
      "display"               = "inline-block",
      "margin"                = "0 0.1em",
      "text-shadow"           = "0 1px 0 #fff",
      "line-height"           = "1.4",
      "white-space"           = "nowrap"
    )
  )
)
