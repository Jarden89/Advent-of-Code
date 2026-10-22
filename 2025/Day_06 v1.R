# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(dplyr)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_06.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()

options(digits = 22)





# Read Input ----

all_input <- readLines(here("Data", "2025_06_input.txt"))





# Part 1 ----

part_one <- all_input |>
  # Remove leading or trailing white space from each line.
  gsub(pattern = "(^(\\s)+)|((\\s)+$)", replacement = "") |>
  # Replace any remaining spaces with commas.
  gsub(pattern = "(\\s)+", replacement = ",") |>
  # Split by commas on each row. Returns a list of each row of the input.
  strsplit(split = ",") |>
  # Convert to data frame and set the column names to x1, x2, ..., op.
  as.data.frame(col.names = c(paste0("x", 1:(length(all_input) - 1)), "op")) |>
  # Convert all but the op column to numeric.
  mutate(across(!op, as.numeric)) |>
  # Calculate the value based on op.
  # I really wish there was a rowProds function in R right now - then we could
  # use rowSums an rowProds to make this look a bit cleaner. The cases would be
  # rowSums(across(1:(length(all_input) - 1))), and the equivalent for product.
  # Seeing as the function doesn't exist we use this apply trick - 1 is the
  # MARGIN element in apply and indicates that the function should be applied
  # across the rows. MARGIN = 2 indicates columns.
  mutate(value = case_when(
    op == "+" ~ apply(across(1:(length(all_input) - 1)), 1, sum),
    op == "*" ~ apply(across(1:(length(all_input) - 1)), 1, prod)
  )) |>
  select(value) |>
  sum()

logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 4878670269096





# Part 2 ----

# This is a helper function that will get the cephalopod numbers from the
# strings in the input.
get_cephalopod_numbers <- function(input_df) {

  number_strings <- input_df |>
    select(!op)

  all_cephalopod_numbers <- list()

  for (i in 1:nrow(number_strings)) {

    row_to_parse <- as.character(number_strings[i, ])

    number_of_cephalopod_numbers <- row_to_parse |>
      sapply(nchar) |>
      max()

    cephalopod_numbers <- numeric(0)
    for (i in 1:number_of_cephalopod_numbers) {

      new_cephalopod_value <- row_to_parse |>
        lapply(substr, start = i, stop = i) |>
        unlist() |>
        paste0(collapse = "") |>
        as.numeric()

      if (!is.na(new_cephalopod_value)) {
        cephalopod_numbers <- c(cephalopod_numbers, new_cephalopod_value)
      }
    }

    all_cephalopod_numbers <- c(all_cephalopod_numbers, list(cephalopod_numbers))
  }

  return(all_cephalopod_numbers)
}


get_grand_total <- function(input) {

  # We want to put commas into the numbers rows where the actual spaces are.
  # We can find the comma locations mainly by doing some regular expression
  # manipulation on the last element of the input (the row with the operations).

  comma_locations <- input[length(input)] |>
    # Replace all * and + at either the start or the end of the line with _ -
    # our place holder for locations where there are digits in the rows above.
    # The last pattern also replaces all spaces with _ - these do not all
    # necessarily represent digit locations in the row above, but we fix that
    # with the next gsub.
    gsub(pattern = "(^\\*)|(\\*$)|(^\\+)|(\\+$)|( )", replacement = "_") |>
    # Replace any instance of "_*" or "_+" with " _" - this correctly places
    # spaces where actual spaces go in the rows above.
    gsub(pattern = "(_\\*)|(_\\+)", replacement = " _") |>
    # Get the locations of all of the spaces.
    gregexpr(pattern = " ") |>
    unlist()

  # We also want the column that shows the operations as in part 1.
  ops_column <- input[length(input)] |>
    gsub(pattern = " ", replacement = "") |>
    strsplit(split = "") |>
    unlist()

  # We then format the numbers columns using the comma locations.
  numbers_cols <- input[c(1:(length(input) - 1))] |>
    strsplit(split = "") |>
    lapply(\(x) {x[comma_locations] <- ","
      return(x)}) |>
    lapply(paste0, collapse = "") |>
    lapply(strsplit, split = ",") |>
    unlist(recursive = FALSE)

  # Stitch the columns together into one data frame.
  parsed_df <- c(numbers_cols, list(ops_column)) |>
    as.data.frame(col.names = c(paste0("x", 1:(length(input) - 1)), "op"))

  # Use the above helper function to get the cephalopod numbers.
  # This takes a bit longer than is ideal because we have to do the calculations
  # rowwise.
  grand_total <- parsed_df |>
    mutate(c_nums = get_cephalopod_numbers(parsed_df)) |>
    rowwise() |>
    mutate(value = case_when(
      op == "+" ~ sum(c_nums),
      op == "*" ~ prod(c_nums)
    )) |>
    select(value) |>
    sum()

  return(grand_total)
}


part_two <- get_grand_total(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 8674740488592
