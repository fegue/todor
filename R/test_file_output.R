library(todor)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
files <- c("tests/testthat/demo.R", "tests/testthat/demo2.R")
filepath <- files
todo_types <- c("TODO", "BUG")
patterns <- todo_types



processed <- lapply(files, function(x) process_file(x, todo_types))
names(processed) <- files
processed

str(processed)

bla <- lapply(processed, function(x) {

  # combinde all matches per file in one dataframe
  # clean text
  # add indentation for formatting in text file
  df_bind <- do.call(
    rbind,
    lapply(x, function(y) {
      df <- as.data.frame(y)
      df$text <- stringr::str_remove(df$text, "#")
      df$text <- stringr::str_trim(df$text, side = "both")
      df$type <- paste0("\t", df$type)
      df$text <- paste0("\t\t", df$text)
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
blop <- mapply(c, names(bla), bla) %>% unlist()

cat(blop, file = "TODO.txt", sep = "\n")

#
# foo <- tibble::enframe(processed) %>%
#   tidyr::unnest(value) %>%
#   tidyr::unnest_wider(value) %>%
#   arrange(name, type, nr) %>%
#   # select(-nr) %>%
#   mutate(text = stringr::str_remove(text, "#"),
#          text = stringr::str_trim(text, side = "both"))
#
# brew <- foo %>%
#   mutate(data = select(., type, text) %>% deframe() %>% as.list()) %>%
#   select(name, data) %>%
#   group_by(name) %>%
#   nest()
# # nested in filename and type
#
# for(i in seq_len(nrow(brew))) {
#   cat(brew$name[i], "\n")
#   for( i in seq_len(length(brew$data[[1]]$data))) {
#     cat("\t",names(brew$data[[1]]$data[i]), "\n")
#     cat("\t\t", paste(brew$data[[1]]$data[i], collapse = "\n"), "\n")
#   }
# }
#
# todo_output <- c()
# for(i in seq_len(nrow(brew))) {
#   todo_output <- paste0(todo_output, brew$name[i], "\n")
#
#   for( i in seq_len(length(brew$data[[1]]$data))) {
#     todo_output <- paste0(todo_output, "\t", names(brew$data[[1]]$data[i]), "\n")
#     todo_output <- paste0(todo_output, "\t\t", paste(brew$data[[1]]$data[i], collapse = "\n"), "\n")
#   }
# }
# todo_output
# cat(todo_output, file = "TODO.txt")
# writeLines()
