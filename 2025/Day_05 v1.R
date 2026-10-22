# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_05.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()

options(digits = 22)





# Read Input ----

# Function to parse the ranges and remove overlapping ranges. This is somewhat
# useful for part 1, and trivialises part 2.
parse_input_ranges <- function(input) {

  all_input_ranges_list <- input[seq(1, which(input == "") - 1)] |>
    strsplit(split = "-") |>
    lapply(as.numeric)

  condensed_ranges <- list()

  remaining_ranges <- all_input_ranges_list

  while (length(remaining_ranges) > 0) {

    ranges_to_condense <- list(remaining_ranges[[1]])

    remaining_ranges <- remaining_ranges[-1]

    # The repeat loop here essentially starts with one range, checks for any
    # overlapping ranges, then checks for all of the overlaps with those ranges.
    # It keeps going until no more overlapping ranges are found.
    repeat{

      # If there are no more ranges left to check we can stop.
      if (length(remaining_ranges) == 0) {
        break
      }


      all_overlap_indexes <- integer(0)

      for (range_to_check in ranges_to_condense) {
        overlapping_indexes <- sapply(remaining_ranges, function(x) {x[1] <= range_to_check[2] && x[2] >= range_to_check[1]}) |>
          which()

        all_overlap_indexes <- c(all_overlap_indexes, overlapping_indexes)
      }


      # If the ranges we are checking don't overlap with any other ranges we can
      # stop.
      if (length(all_overlap_indexes) == 0) {
        break
      } else {

        matching_ranges <- remaining_ranges[all_overlap_indexes]

        remaining_ranges <- remaining_ranges[-all_overlap_indexes]

        ranges_to_condense <- c(ranges_to_condense, matching_ranges)

      }
    }


    # Then we condense all of the overlapping ranges here and add the new range
    # to our list.
    new_range_start <- ranges_to_condense |>
      lapply(\(x) x[1]) |>
      unlist() |>
      min()

    new_range_end <- ranges_to_condense |>
      lapply(\(x) x[2]) |>
      unlist() |>
      max()

    condensed_ranges[length(condensed_ranges) + 1] <- list(c(new_range_start, new_range_end))
  }


  return(condensed_ranges)
}


all_input <- readLines(here("Data", "2025_05_input.txt"))


input_ranges <- parse_input_ranges(input = all_input)
input_ingredient_ids <- all_input[seq(which(all_input == "") + 1, length(all_input))] |>
  as.numeric()

logger::log_success("Input parsing complete...")
logger::log_elapsed("Input parsing complete...")





# Part 1 ----

# Helper function for checking if an id is fresh or not.
check_if_fresh <- function(ingredient_id, input_ranges_list) {

  fresh_bool <- FALSE

  for (range in input_ranges_list) {
    if (ingredient_id >= range[1] && ingredient_id <= range[2]) {
      fresh_bool <- TRUE
      break
    }
  }

  return(fresh_bool)
}


part_one <- sapply(input_ingredient_ids, check_if_fresh, input_ranges_list = input_ranges) |>
  sum()
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 617





# Part 2 ----

part_two <- input_ranges |>
  lapply(\(x) {(x[2] - x[1]) + 1}) |>
  unlist() |>
  sum()
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 338,258,295,736,104
# 338258295736104
