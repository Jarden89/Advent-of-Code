# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_04.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

# Function to turn the original input into a matrix where 1 represents a roll of
# paper and 0 is an empty space. We add a ring of 0s around the original input
# to make finding available rolls easier.
parse_input_to_matrix <- function(input) {

  # Start with a row of zeroes.
  values_matrix <- matrix(0, nrow = 1, ncol = nchar(input[1]) + 2)

  for (row in input) {

    # Replace "." with a 0 and @ with a 1
    new_row <- gsub(pattern = "\\.", replacement = 0, x = row) |>
      gsub(pattern = "\\@", replacement = 1) |>
      # Convert to numeric.
      strsplit(split = "") |>
      unlist() |>
      as.numeric()

    # Convert to numeric and add a zero on each end.
    new_row <- c(0, new_row, 0)

    # Add to the matrix.
    values_matrix <- rbind(values_matrix, new_row)

  }

  # Add a final row of zeroes.
  values_matrix <- rbind(
    values_matrix,
    matrix(0, nrow = 1, ncol = nchar(input[1]) + 2)
  )


  return(values_matrix)
}


all_input <- readLines(here("Data", "2025_04_input.txt"))

matrix_input <- parse_input_to_matrix(input = all_input)

logger::log_success("Input parsing complete...")
logger::log_elapsed("Input parsing")





# Part 1 ----

count_available_rolls <- function(matrix_values) {

  # We iterate only over the inner matrix to make things simpler.
  rows <- 2:(nrow(matrix_values) - 1)
  cols <- 2:(ncol(matrix_values) - 1)

  available_count <- 0L

  for (i in rows) {
    for (j in cols) {

      # Make sure to only check values that are rolls of paper.
      if (matrix_values[i, j] == 1) {

        # It might be smarter to do sum(matrix_values[i-1:i+1, j-1:j+1]) - 1
        # here, but this also works.
        surrounding_value <- sum(c(
          matrix_values[i - 1, j - 1],
          matrix_values[i - 1, j],
          matrix_values[i - 1, j + 1],
          matrix_values[i, j - 1],
          matrix_values[i, j + 1],
          matrix_values[i + 1, j - 1],
          matrix_values[i + 1, j],
          matrix_values[i + 1, j + 1]
        ))

        if (surrounding_value < 4) {
          available_count <- available_count + 1L
        }
      }
    }
  }


  return(available_count)
}


part_one <- count_available_rolls(matrix_values = matrix_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 1363





# Part 2 ----

# Very similar function to part one but we return a list of pairs of the paper
# instead.
get_available_rolls <- function(matrix_values) {

  rows <- 2:(nrow(matrix_values) - 1)
  cols <- 2:(ncol(matrix_values) - 1)

  available_rolls <- list()

  for (i in rows) {
    for (j in cols) {

      if (matrix_values[i, j] == 1) {

        surrounding_value <- sum(c(
          matrix_values[i - 1, j - 1],
          matrix_values[i - 1, j],
          matrix_values[i - 1, j + 1],
          matrix_values[i, j - 1],
          matrix_values[i, j + 1],
          matrix_values[i + 1, j - 1],
          matrix_values[i + 1, j],
          matrix_values[i + 1, j + 1]
        ))

        if (surrounding_value < 4) {
          available_rolls[[length(available_rolls) + 1]] <- c(i, j)

        }
      }
    }
  }


  return(available_rolls)
}


# We use the above function to iterate over the matrix and convert available
# rolls of paper to 0s, then check how many rolls are still available.
count_remaining_rolls <- function(matrix_values) {

  matrix_of_rolls <- matrix_values

  original_number_of_rolls <- sum(matrix_of_rolls)

  available_rolls <- get_available_rolls(matrix_values = matrix_of_rolls)

  number_of_available_rolls <- length(available_rolls)


  while (number_of_available_rolls > 0) {

    # Replace available rolls with 0.
    for (pair in available_rolls) {
      matrix_of_rolls[pair[1], pair[2]] <- 0L
    }

    # Re-check the number and positions of available rolls.
    available_rolls <- get_available_rolls(matrix_values = matrix_of_rolls)

    number_of_available_rolls <- length(available_rolls)

  }


  # Return the answer - how many rolls have been removed.
  value <- original_number_of_rolls - sum(matrix_of_rolls)

  return(value)
}


part_two <- count_remaining_rolls(matrix_values = matrix_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 8184
