# grkmisc 0.1.2

- Added `use_grk_github_labels()` with standard labels for use in GitHub repos.

- Renamed all package helpers based on `usethis` to use the prefix `use_grk_`.
  For example, `use_grk_starter_package()` replaced `use_starter_package()`.
  
- Fixed `use_grk_gitignore()` so that template is correctly found internally.

- Replaced `hide_panel_grid_minor` with `panel_grid` having options "major",
  "minor", "both", or "none" in `theme_moffitt()`.
  
- Setting `plot_caption_color` or `panel_border_color` to `NULL` in
  `theme_moffitt()` skips setting those colors.

# grkmisc 0.1.0

- A nice default R Markdown template available through RStudio new markdown file
  "From Template" dialogue called "Garrick Default".

- A Moffitt color palette and Moffitt colors CSV file.

- A Moffitt-styled xaringan presentation template, also in the "From Template"
  pane of RStudio's new markdown file dialogue as "Moffitt Xaringan".
  
- Copy code from the console and insert in "tidy" style using the "Insert Tidy-
  Styled Code" RStudio addin. Breaks pipe and ggplot steps into individual lines.
  
- Shorten GitHub URLs with `shorten_github_url()`.

- Create a brand new package with all the bells and whistles with
  `use_starter_package()`.
  
- A couple helpers to install a default `.gitignore` file with `use_gitignore()`
  and to install a helpful git pre-commit hook with `use_git_hook_precommit()`.

- Create a new project (not package) with `use_starter_project()`.

- Import SAS format data files with `read_sas_format()`. Import SAS data files
  and format files and apply labels directly to the data with 
  `read_sas_with_format()`. Or apply the labels from a SAS format file using
  `add_proc_format_labels()`.

- Insert the relative or absolute path to the directory containing the active
  document with the RStudio Addins "Insert Relative Directory Path" or
  "Insert Absolute Directory Path".

- New ggplot2 themes: `theme_moffitt()` and `theme_grk()`

- New ggplot2 scales: `scale_color_moffitt()` and `scale_fill_moffitt()`. When
  given two values, both scales allow the user to pick a secondary color from
  `moffitt_colors` (green, red, orange, light_blue, yellow, or grey (default))
  to contrast the primary blue color.

- A function to help replace missing values in data frame columns: `if_na()`

- A pipe-friendly logging functions: `logger()`

- A pipe- and Rmd-friendly function to save ggplot objects: `ggsave_and_print()`.
  In comparison with `ggsave()`, the first argument is the plot object and the
  second argument is the filename, making this function pipe-friendly. By
  default, the plot is printed but not saved when used interactively.
  
- A function to truncate and wrap long strings: `pretty_string()`

- `format_pretty_string()` and `format_pretty_number()` return formatting
  functions that can be used as labellers in ggplot2 scales.
