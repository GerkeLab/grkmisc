% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addins.R
\name{insert_path_dir_active}
\alias{insert_path_dir_active}
\alias{insert_path_dir_active_abs}
\title{Insert Path Containing Active File}
\usage{
insert_path_dir_active(relative = TRUE)

insert_path_dir_active_abs()
}
\arguments{
\item{relative}{Should the path be relative to the current working directory?}
}
\description{
For the "Insert Relative Directory Path" RStudio Addin. Inserts the relative
path to the directory containing the active source document. If text is
selected, this text is assumed to be a file inside that directory and is
appended to the path. The "Insert Absolute Directory Path" Rstudio Addin does
the same but provides the absolute path to the active source document.
}
