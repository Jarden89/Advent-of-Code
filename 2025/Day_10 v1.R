# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_10.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_10_input.txt"))

# Should return 7, 33.
sample_input <- c(
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
  "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
  "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
)





# Part 1 ----

# Helper function to calculate the minimum number of button presses for a given
# machine with a bit of linear algebra.
get_minimum_button_presses_part_one <- function(machine) {

  # Get the desired output as a nx1 matrix (1 means a light is on, 0 means it's
  # off).
  desired_light_state <- machine[1] |>
    gsub(pattern = "\\[|\\]", replacement = "") |>
    strsplit(split = "") |>
    unlist() |>
    gsub(pattern = "\\#", replacement = "1") |>
    gsub(pattern = "\\.", replacement = "0") |>
    as.integer() |>
    matrix(ncol = 1)


  # Get the indexes of the lights that each button toggles.
  buttons <- machine[2:(length(machine) - 1)] |>
    gsub(pattern = "\\(|\\)", replacement = "") |>
    strsplit(split = ",") |>
    lapply(as.numeric) |>
    # Note that we have to add 1 as R indexes at 1.
    lapply(\(x) x + 1)


  # Convert the button indexes to a matrix. Each column represents a button.
  buttons_matrix <- matrix(0L, nrow = nrow(desired_light_state), ncol = length(buttons))

  for (i in 1:length(buttons)) {
    buttons_matrix[buttons[[i]], i] <- 1L
  }


  # Solutions will be binary - we will only need to press a button a maximum of
  # 1 times. This gets all possible combinations then sorts them by fewest
  # number of button presses.
  possible_solutions <- expand.grid(rep(list(c(0, 1)), ncol(buttons_matrix)))
  possible_solutions <- possible_solutions[order(rowSums(possible_solutions)), ]


  # Iterate over the possible solutions and stop when we find one that turns on
  # the desired lights.
  for (i in 1:nrow(possible_solutions)) {

    solution <- t(possible_solutions[i, ])

    # We add one each time a button is toggled, so the modulus of 2 for each
    # light gives us a 1 if the button is on, and 0 otherwise.
    output <- (buttons_matrix %*% solution) %% 2

    if (all(output == desired_light_state)) {
      break
    }
  }

  value <- sum(solution)


  return(value)
}


# Wrapper function to parse the input and use the above function to get an
# answer.
get_total_minimum_button_presses_part_one <- function(input) {

  all_machines <- strsplit(input, split = " ")

  value <- all_machines |>
    sapply(get_minimum_button_presses_part_one) |>
    sum()

  return(value)
}


part_one <- get_total_minimum_button_presses_part_one(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 502





# Part 2 ----

# Similar helper function to the one used in part one, except we are using
# lpSolve to calculate the minimum number of button presses for a given machine.
get_minimum_button_presses_part_two <- function(machine) {

  # Get the desired output as a vector.
  desired_joltage_state <- machine[length(machine)] |>
    gsub(pattern = "\\{|\\}", replacement = "") |>
    strsplit(split = ",") |>
    unlist() |>
    as.integer()


  # Get the indexes of the lights that each button toggles.
  buttons <- machine[2:(length(machine) - 1)] |>
    gsub(pattern = "\\(|\\)", replacement = "") |>
    strsplit(split = ",") |>
    lapply(as.numeric) |>
    # Note that we have to add 1 as R indexes at 1.
    lapply(\(x) x + 1)


  # Convert the button indexes to a matrix. Each column represents a button.
  buttons_matrix <- matrix(0L, nrow = length(desired_joltage_state), ncol = length(buttons))

  for (i in 1:length(buttons)) {
    buttons_matrix[buttons[[i]], i] <- 1L
  }


  # Boring to just use lpSolve but it's easy at least - and pretty fast.
  value <- lpSolve::lp(
    direction = "min",
    objective.in = rep(1, ncol(buttons_matrix)),
    const.mat = buttons_matrix,
    const.dir = rep("=", length(desired_joltage_state)),
    const.rhs = desired_joltage_state,
    all.int = TRUE
  )$objval


  return(value)
}


# Wrapper function to parse the input and use the above function to get an
# answer.
get_total_minimum_button_presses_part_two <- function(input) {

  all_machines <- strsplit(input, split = " ")

  value <- all_machines |>
    sapply(get_minimum_button_presses_part_two) |>
    sum()

  return(value)
}


part_two <- get_total_minimum_button_presses_part_two(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 21467
