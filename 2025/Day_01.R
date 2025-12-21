# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(tictoc)
}))


## 0.2 Location ----

suppressMessages({
  i_am("2025/Day_01.R")
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

count_zeroes_landed_on <- function(start_value, input) {

  starting_int <- as.integer(start_value)

  # Parse the input into an integer vector of positive/negative turns (positive
  # turns to the right).
  # Note that we add the start_value to the beginning of the input.
  instructions <- sub(pattern = "L", replacement = "-", x = c(starting_int, input)) |>
    sub(pattern = "R", replacement = "") |>
    as.integer()

  # How many times do we land on 0?
  zeroes_count <- sum(cumsum(instructions) %% 100 == 0)

  return(zeroes_count)
}


part_one <- count_zeroes_landed_on(start_value = 50L, input = all_input)
log_info("Part One Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part One: {part_one}")
# 1052





# Part 2 ----

tic()

# Helper function to count the number of zeroes a pair of starting values and
# instructions will generate.
count_zeroes_between <- function(pair) {

  starting_int <- as.integer(pair[1])
  addend_int <- as.integer(pair[2])
  ending_int <- starting_int + addend_int

  # Are we starting at 0?
  START_ZERO <- starting_int == 0L
  # Are we turning left?
  ADDEND_NEGATIVE <- addend_int < 0L
  # Are we turning some multiple of 100?
  ADDEND_100 <- addend_int %% 100 == 0
  # Do we end at a multiple of 100?
  ENDING_100 <- ending_int %% 100 == 0

  # We can typically count the number of times that zero is passed or ended on
  # just by looking at the result of adding the new instruction to the starting
  # value. In most cases this would be abs(ending_int %/% 100), but there are
  # some edge cases.

  if (START_ZERO & ADDEND_NEGATIVE & !ADDEND_100) {
    # If we start at zero and subtract a negative (i.e. turn left any amount),
    # an extra pass of 0 is counted as -10 %/% 100 is -1. We counteract this by
    # subtracting one from the number of zeroes in this case. However, we don't
    # want to do this if the addend is divisible by 100 (-100 %/% 100 is also -1)
    # so we ignore these cases for now.

    zero_count <- abs(ending_int %/% 100) - 1

  } else if (ENDING_100 & ADDEND_NEGATIVE) {
    # The other edge case is if the addend is equal to the starting value plus
    # some multiple of 100, and we are turning left (i.e. 24 and -124). In these
    # cases abs(ending_int %/% 100) will return one fewer zeroes than desired,
    # so we add one more.

    zero_count <- abs(ending_int %/% 100) + 1

  } else {
    # Otherwise we can just take the remainder of modulus division.

    zero_count <- abs(ending_int %/% 100)
  }

  return(zero_count)
}


# Generate a list of pairs of starting values and the next instruction, then use
# the above function to calculate the number of zeroes.
count_zeroes_passed <- function(start_value, input) {

  starting_int <- as.integer(start_value)

  all_instructions <- sub(pattern = "L", replacement = "-", x = c(starting_int, input)) |>
    sub(pattern = "R", replacement = "") |>
    as.integer()

  all_starting_values <- cumsum(all_instructions) %% 100

  # Turn the vectors all_starting_values and all_instructions into a list of
  # numeric pairs (minus the last "starting value" and the first instruction -
  # which is the start_value passed to the function).
  all_pairs <- Map(c, all_starting_values[-length(all_starting_values)], all_instructions[-1])

  zeroes_count <- sapply(all_pairs, count_zeroes_between) |>
    sum()

  return(zeroes_count)
}


part_two <- count_zeroes_passed(start_value = 50L, input = all_input)
log_info("Part Two Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part Two: {part_two}")
# 6295





# End ----

log_info("Total time elapsed: {toc(quiet = TRUE)$callback_msg}")
