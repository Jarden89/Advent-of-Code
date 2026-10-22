# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_02.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_02_input.txt"))





# Part 1 ----

# The only tricky thing here is figuring out the regular expression needed to
# match the (in)correct strings.
sum_incorrect_ids_part_1 <- function(input) {

  incorrect_ids_count <- strsplit(input, split = ",") |>
    unlist() |>
    # Gives us a list of pairs of numeric values that we can turn into a range.
    strsplit(split = "-") |>
    # We want strings here so we can use grep.
    lapply(\(x) as.character(seq.int(from = x[1], to = x[2]))) |>
    unlist() |>
    # There might not be any overlap but this ensures that, if there is, no
    # double counting of incorrect ids occurs.
    unique() |>
    grepv(pattern = "^([[:digit:]]+)\\1$") |>
    as.numeric() |>
    sum()

  return(incorrect_ids_count)
}


part_one <- sum_incorrect_ids_part_1(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 19574776074





# Part 2 ----

# Same as above but with slightly different regular expression.
sum_incorrect_ids_part_2 <- function(input) {

  incorrect_ids_count <- strsplit(input, split = ",") |>
    unlist() |>
    strsplit(split = "-") |>
    lapply(\(x) as.character(seq.int(from = x[1], to = x[2]))) |>
    unlist() |>
    unique() |>
    grepv(pattern = "^([[:digit:]]+)\\1{1,}$") |>
    as.numeric() |>
    sum()


  return(incorrect_ids_count)
}


part_two <- sum_incorrect_ids_part_2(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 25912654282
