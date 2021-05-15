#' Find package
#'
#' @param path character with path to directory with R code files
#'
#' @return package path
find_package <- function(path = getwd()) {
  start_wd <- getwd()
  on.exit(setwd(start_wd))
  setwd(path)
  prev_path <- ""
  while (!file.exists(file.path(prev_path, "DESCRIPTION"))) {
    if (prev_path == getwd()) {
      return(NULL)
    }
    prev_path <- getwd()
    setwd("..")
  }
  prev_path
}

#' Process file
#'
#' It calls \code{find_pattern} on given file and return detected markers or NULL.
#'
#' @param filepath character with
#' @param patterns vector of characters with given patterns to detect,
#' e.g. c("TODO", "BUG")
#'
#' @return list of markers (which are lists describing properties
#' of detected item from find_pattern)
process_file <- function(filepath, patterns) {
  con <- file(filepath, "r")
  n <- 1
  markers <- list()
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    pattern_check <- find_pattern(line, patterns = patterns)
    if (!is.null(pattern_check))
      markers[[length(markers) + 1]] <- list(nr = n,
                                             type = pattern_check,
                                             text = stringr::str_replace(
                                               line, pattern_check, "")
      )
    n <- n + 1
  }
  close(con)
  markers
}

#' List files with given extension
#'
#' It lists recursively with full path names.
#'
#' @param extension character with extension
#' @param search_path character with path to start searching from
#'
#' @return list of files with specified extension
#' @import rex
list_files_with_extension <- function(extension, search_path) {
  dir(
    path = search_path,
    pattern = rex::rex(".", extension, end),
    recursive = TRUE,
    full.names = TRUE
  )
}

#' Write detected markers to file
#'
#' Takes the output from \code{process_file} (named with file names) and writtes it to a file.
#' Matched patterns will be grouped by file and type (e.g. TODO, BUG, etc.)
#'
#' @param todo.list named output from \code{process_file}. list of files with lists of items detected in it
#' @param file_output character string; file to write to
#'
#' @return output of formatted markers into file
todolist_to_file <- function(todo.list, file_output) {

  nonempty <- Filter(function(k) length(k) > 0, todo.list)
  vec <- lapply(nonempty, function(x) {
    # combinde all matches per file in one dataframe
    df_bind <- do.call(
      rbind,
      lapply(x, function(y) {
        df <- as.data.frame(y)
        # clean text
        df$text <- stringr::str_trim(df$text, side = "both")
        df$text <- stringr::str_remove(df$text, "#")
        # add indentation for formatting in text-file
        df$type <- paste0("\t", df$type)
        df$text <- paste0("\t", df$text)
        df$nr <- NULL
        df
      })
    )
    # split into groups per type
    split_type <- split(df_bind, df_bind$type)

    # make character vector with type + text
    combined <- lapply(split_type, function(y) {
      c(unique(y$type), y$text)
    })
    unlist(combined)
  })
  # add name of file to character vector
  filename_vec <- unlist(mapply(c, names(vec), vec))
  cat(filename_vec, file = file_output, sep = "\n")
}
