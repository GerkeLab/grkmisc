#' Auto Extract Zipped Files
#'
#' @param path Path to a single zipped file or a folder containing files to be
#'   unzipped.
#' @param ... Arguments passed on to [untar] or [unzip] (or both for
#'   `list` and `extdir`). The `files` argument will be ignored.
#' @export
auto_extract <- function(path, ...) {
  is_dir <- file.info(path)$isdir == TRUE

  files <- NULL
  files <- if (is_dir && dir.exists(path)) {
    dir(path, full.names = TRUE, pattern = "(tar|gz|zip)$")
  } else if (file.exists(path)) {
    path
  }
  if (is.null(files)) stop(path, " does not exist or does not contain zipped files.")

  # files <- check_gunzip_reqs(files)

  dots <- list(...)
  dots <- dots[setdiff(names(dots), "files")] # ignore files args if specified
  untar_args <- dots[intersect(names(formals(utils::untar)), names(dots))]
  unzip_args <- dots[intersect(names(formals(utils::unzip)), names(dots))]

  for (file in files) {
    file_args <- switch(
      file_ext(file),
      "zip" = unzip_args,
      untar_args
    )
    file_args$file <- file
    output <- switch(
      file_ext(file),
      "zip" = do.call("unzip_in_dir", file_args),
      "tar" = do.call("untar_in_dir", file_args),
      "gz"  = do.call("untar_in_dir", file_args),
      "tgz" = do.call("untar_in_dir", file_args),
      message("Nothing to do for ", file)
    )
    if (!is.null(output)) print(output)
  }

  if (is_dir) {
    new_files <- dir(path, full.names = TRUE, pattern = "(tar|gz|zip)$")
    new_compressed <- grep("(tar|gz|zip)$", setdiff(new_files, files), value = TRUE)
    message("Some extracted files in", path, "may still be compressed:",
            paste(basename(new_compressed), collapse = ", "))
  }
}

untar_in_dir <- function(file, ...) {
  assertthat::is.readable(file)
  message("Untarring file: ", file)
  in_dir(dirname(file), utils::untar(basename(file), ...))
}

unzip_in_dir <- function(file, ...) {
  assertthat::is.readable(file)
  message("Unzipping file: ", file)
  in_dir(dirname(file), utils::unzip(basename(file), ...))
}

file_ext <- function(x) {
  # tools::file_ext
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

in_dir <- function (dir, expr) {
    owd = setwd(dir)
    on.exit(setwd(owd))
    expr
}
