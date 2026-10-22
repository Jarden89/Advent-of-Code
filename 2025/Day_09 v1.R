# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(geos)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_09.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_09_input.txt"))

# Should return: 50, 24.
sample_input <- c(
  "7,1",
  "11,1",
  "11,7",
  "9,7",
  "9,5",
  "2,5",
  "2,3",
  "7,3"
)





# Part 1 ----

# Simple function to calculate all of the possible areas then return the largest
# value.
get_largest_rectangle_part_one <- function(input) {

  coordinate_count <- length(input)
  matrix_of_rectangle_areas <- matrix(NA_integer_, nrow = coordinate_count, ncol = coordinate_count)

  all_coordinates <- input |>
    lapply(strsplit, split = ",") |>
    unlist(recursive = FALSE) |>
    lapply(as.numeric)

  for (i in 1:coordinate_count) {
    for (j in 1:coordinate_count) {
      if (i < j) {
        coord_a <- all_coordinates[[i]]
        coord_b <- all_coordinates[[j]]

        matrix_of_rectangle_areas[i, j] <- (abs(coord_a[1] - coord_b[1]) + 1) * (abs(coord_a[2] - coord_b[2]) + 1)
      }
    }
  }

  largest_area <- sort(matrix_of_rectangle_areas, decreasing = TRUE)[1]

  return(largest_area)
}


part_one <- get_largest_rectangle_part_one(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 4777816465





# Part 2 ----

# A similar function to part one but now we check if the rectangle is in the
# shape (using the geos package) before calculating the area.
# Note that we could use coordinate compression here but I tested it and it
# doesn't seem to make the geos functions any faster.
get_largest_rectangle_part_two <- function(input) {

  coordinate_count <- length(input)
  matrix_of_rectangle_areas <- matrix(NA_integer_, nrow = coordinate_count, ncol = coordinate_count)

  all_coordinates <- input |>
    lapply(strsplit, split = ",") |>
    unlist(recursive = FALSE) |>
    lapply(as.numeric)


  polygon_coordinates <- all_coordinates |>
    purrr::list_transpose()

  full_shape <- geos::geos_make_polygon(
    x = polygon_coordinates[[1]],
    y = polygon_coordinates[[2]]
  )


  for (i in 1:coordinate_count) {
    for (j in 1:coordinate_count) {
      if (i < j) {
        coord_a <- all_coordinates[[i]]
        coord_b <- all_coordinates[[j]]

        x_sorted <- sort(c(coord_a[1], coord_b[1]))

        y_sorted <- sort(c(coord_a[2], coord_b[2]))

        new_shape <- geos_make_polygon(
          x = c(x_sorted[1], x_sorted[1], x_sorted[2], x_sorted[2]),
          y = c(y_sorted[1], y_sorted[2], y_sorted[2], y_sorted[1])
        )

        if (geos_within(new_shape, full_shape)) {
          matrix_of_rectangle_areas[i, j] <- (abs(coord_a[1] - coord_b[1]) + 1) * (abs(coord_a[2] - coord_b[2]) + 1)
        }
      }
    }
  }

  largest_area <- sort(matrix_of_rectangle_areas, decreasing = TRUE)[1]

  return(largest_area)
}


part_two <- get_largest_rectangle_part_two(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 1410501884
