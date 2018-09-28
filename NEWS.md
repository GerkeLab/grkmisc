# grkmisc 0.0.1

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
