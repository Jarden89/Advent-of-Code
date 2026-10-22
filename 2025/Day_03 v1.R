# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_03.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_03_input.txt"))





# Part 1 ----

# Generic function to get the maximum 'joltage' from a bank - can be used for
# both parts 1 and 2.
get_max_joltage <- function(battery_bank, digits = 2) {

  remaining_batteries <- as.numeric(battery_bank)

  # We use strings here so that the result can be pasted together then passed to
  # as.numeric to get the final value.
  batteries_selected <- character(0)

  number_of_batteries_to_select <- digits

  while (length(batteries_selected) < digits) {

    # If we only have n batteries left to pick from and we need n digits, we can
    # take these as the remaining digits and stop early.
    if (length(remaining_batteries) == number_of_batteries_to_select) {
      batteries_selected <- c(batteries_selected, remaining_batteries)
      break
    }

    # If all the remaining batteries have the same value, the rest of the digits
    # will be that value and we can stop early.
    if (length(unique(remaining_batteries)) == 1) {
      remaining_digits <- rep(unique(remaining_batteries)[1], number_of_batteries_to_select)
      batteries_selected <- c(batteries_selected, remaining_digits)
      break
    }


    # In most cases we will need to search for the next battery to pick, which
    # this if/else loop deals with.
    if (number_of_batteries_to_select == 1) {

      next_battery <- max(remaining_batteries)

    } else {

      # If we still need to pick n batteries to turn on, we cannot select any of
      # the last (n-1) batteries in the bank to turn on.
      batteries_to_choose_from <- remaining_batteries[1:(length(remaining_batteries) - number_of_batteries_to_select + 1)]

      next_battery <- max(batteries_to_choose_from)

      battery_index <- min(which(batteries_to_choose_from == next_battery))

      remaining_batteries <- remaining_batteries[-(1:battery_index)]

    }

    batteries_selected <- c(batteries_selected, next_battery)

    number_of_batteries_to_select <- number_of_batteries_to_select - 1

  }


  joltage <- paste0(batteries_selected, collapse = "") |>
    as.numeric()

  return(joltage)
}


part_one <- all_input |>
  strsplit(split = "") |>
  lapply(get_max_joltage, digits = 2) |>
  unlist() |>
  sum()
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 17535





# Part 2 ----

part_two <- all_input |>
  strsplit(split = "") |>
  lapply(get_max_joltage, digits = 12) |>
  unlist() |>
  sum()
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 173577199527257
