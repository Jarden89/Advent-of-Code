# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(tictoc)
}))


## 0.2 Location ----

suppressMessages({
  i_am("2016/Day_01.R")
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

count_total_distance <- function(input) {
  
  # Transpose left turns into negative numbers, right turns into positive
  # numbers.
  all_instructions <- input |>
    gsub(pattern = ",", replacement = "") |>
    gsub(pattern = "L", replacement = "-") |>
    gsub(pattern = "R", replacement = "") |>
    strsplit(split = " ") |>
    unlist() |>
    as.integer()
  
  # Collect all instructions that move in the direction of the x-axis here.
  x_instructions <- integer(0)
  # Collect all instructions that move in the direction of the y-axis here.
  y_instructions <- integer(0)
  
  # Use 0, 1, 2, 3 to represent the cardinal directions (North, East, South,
  # West).
  current_direction <- 0
  
  for (instruction in all_instructions) {
    # For each instruction we check if we are moving parallel to the x or
    # y-axis, and we check if we are moving in the positive direction or the
    # negative direction. We then save the coordinate transformation in the
    # relevant vector.
    
    new_direction <- (current_direction + sign(instruction)) %% 4
    
    # Are we moving in the negative direction?
    negative_y <- (current_direction == 1 && sign(instruction) > 0) || (current_direction == 3 && sign(instruction) < 0)
    negative_x <- (current_direction == 2 && sign(instruction) > 0) || (current_direction == 0 && sign(instruction) < 0)
    
    if (new_direction %in% c(0, 2)) {
      # If we are moving in the y direction.
      if (negative_y) {
        y_instructions <- c(y_instructions, -abs(instruction))
      } else {
        y_instructions <- c(y_instructions, abs(instruction))
      }
      
    } else {
      # If we are moving in the x direction.
      if (negative_x) {
        x_instructions <- c(x_instructions, -abs(instruction))
      } else {
        x_instructions <- c(x_instructions, abs(instruction))
      }
    }
    
    current_direction <- new_direction
  }
  
  total_distance <- (sum(x_instructions) + sum(y_instructions)) |>
    abs()
  
  return(total_distance)
}


part_one <- count_total_distance(input = all_input)
log_info("Part One Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part One: {part_one}")
# .send_solution(part_one, 1)
# 299


# Part 2 ----

tic()

count_total_distance_until_repeat <- function(input) {
  
  # Transpose left turns into negative numbers, right turns into positive
  # numbers.
  all_instructions <- input |>
    gsub(pattern = ",", replacement = "") |>
    gsub(pattern = "L", replacement = "-") |>
    gsub(pattern = "R", replacement = "") |>
    strsplit(split = " ") |>
    unlist() |>
    as.numeric()
  
  # We store traversed coordinates in a data frame.
  traversed <- data.frame(x = 0, y = 0)
  
  # Use 0, 1, 2, 3 to represent the cardinal directions (North, East, South,
  # West).
  current_direction <- 0
  
  for (instruction in all_instructions) {
    # For each instruction we check if we are moving parallel to the x or
    # y-axis, and we check if we are moving in the positive direction or the
    # negative direction. We then save the coordinate transformation in the
    # relevant vector.
    
    new_direction <- (current_direction + sign(instruction)) %% 4
    
    # Are we moving in the negative direction?
    negative_y <- (current_direction == 1 && sign(instruction) > 0) || (current_direction == 3 && sign(instruction) < 0)
    negative_x <- (current_direction == 2 && sign(instruction) > 0) || (current_direction == 0 && sign(instruction) < 0)
    
    current_x <- traversed[nrow(traversed),][1]
    current_y <- traversed[nrow(traversed),][2]
    
    for (i in 1:abs(instruction)) {
      if (new_direction %in% c(0, 2)) {
        # If we are moving in the y direction.
        if (negative_y) {
          new_coordinate <- data.frame(x = current_x, y = current_y - 1)
        } else {
          new_coordinate <- data.frame(x = current_x, y = current_y + 1)
        }
        
      } else {
        # If we are moving in the x direction.
        if (negative_x) {
          new_coordinate <- data.frame(x = current_x - 1, y = current_y)
        } else {
          new_coordinate <- data.frame(x = current_x + 1, y = current_y)
        }
      }
      
      if (nrow(merge(new_coordinate, traversed)) > 0) {
        traversed <- rbind(traversed, new_coordinate)
        break
      }
      
      traversed <- rbind(traversed, new_coordinate)
      
      current_x <- new_coordinate[1,1]
      current_y <- new_coordinate[1,2]
    }
    
    current_direction <- new_direction
  }
  
  print(new_coordinate)
  total_distance <- abs(new_coordinate[1,1]) + abs(new_coordinate[1,2])
  print(total_distance)
  return(traversed)
}

test <- duplicated(part_two) |>
  which() |>
  min()
part_two[586,]

part_two <- count_total_distance_until_repeat(input = all_input)
log_info("Part Two Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part Two: {part_two}")
# .send_solution(part_two, 2)
# 181 (-176, -5)

# End ----

log_info("Total time elapsed: {toc(quiet = TRUE)$callback_msg}")
