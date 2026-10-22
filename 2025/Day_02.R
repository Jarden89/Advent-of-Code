# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(tictoc)
}))


## 0.2 Location ----

suppressMessages({
  i_am("2025/Day_02.R")
})


## 0.3 Options ----

log_layout(layout_glue_colors)
log_threshold(TRACE)
tic()





# Read Input ----

tic()

all_input <- .get_todays_input()

log_info("Input read and parsed: {toc(quiet = TRUE)$callback_msg}")





# Part 1 ----

tic()

# The only tricky thing here is figuring out the regular expression needed to
# match the (in)correct strings.
sum_incorrect_ids_part_1 <- function(input) {
  
  incorrect_ids_count <- strsplit(input, split = ",") |>
    unlist() |>
    # Gives us a list of pairs of numeric values that we can turn into a range.
    strsplit(split = "-") |>
    # We want strings here so we can use grep.
    sapply(\(x) as.character(seq.int(from = x[1], to = x[2]))) |>
    # unlist() |>
    # There might not be any overlap but this ensures that, if there is, no
    # double counting of incorrect ids occurs.
    unique() |>
    grepv(pattern = "^([[:digit:]]+)\\1$") |>
    as.numeric() |>
    sum()
  
  return(incorrect_ids_count)
}


part_one <- sum_incorrect_ids_part_1(input = all_input)
log_info("Part One Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part One: {part_one}")
#





# Part 2 ----

tic()




part_two <- NULL
log_info("Part Two Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part Two: {part_two}")
#





# End ----

log_info("Total time elapsed: {toc(quiet = TRUE)$callback_msg}")
