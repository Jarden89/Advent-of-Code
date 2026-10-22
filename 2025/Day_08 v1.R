# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_08.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_08_input.txt"))

# Should return: 40, 25272.
sample_input <- c(
  "162,817,812",
  "57,618,57",
  "906,360,560",
  "592,479,940",
  "352,342,300",
  "466,668,158",
  "542,29,236",
  "431,825,988",
  "739,650,466",
  "52,470,668",
  "216,146,977",
  "819,987,18",
  "117,168,530",
  "805,96,715",
  "346,949,466",
  "970,615,88",
  "941,993,340",
  "862,61,35",
  "984,92,344",
  "425,690,689"
)





# Part 1 ----

get_prod_three_biggest_junctions <- function(input, number_of_connections = 1000) {

  # We store the distances in a matrix, and the coordinates in a list. Note that
  # we use integers  for the distances as we omit taking the square root when
  # calculating the Euclidean distance between points - this is an unnecessary
  # step if we only wish to know what points the shortest distances are between,
  # rather than the value of the distances.
  coordinate_count <- length(input)
  matrix_of_distances <- matrix(NA_integer_, nrow = coordinate_count, ncol = coordinate_count)

  all_coordinates <- input |>
    lapply(strsplit, split = ",") |>
    unlist(recursive = FALSE) |>
    lapply(as.numeric)

  for (i in 1:coordinate_count) {
    for (j in 1:coordinate_count) {
      if (i < j) {
        x <- all_coordinates[[i]]

        y <- all_coordinates[[j]]

        matrix_of_distances[i, j] <- (x[1] - y[1])^2 + (x[2] - y[2])^2 + (x[3] - y[3])^2
      }
    }
  }

  # We want to make sure that the distances we are going to check are unique, as
  # we don't want to match multiple sets of coordinates - this isn't a problem
  # for my input and would require some more complex logic.
  ordered_distances <- sort(matrix_of_distances, decreasing = FALSE) |>
    head(n = number_of_connections)

  if (length(ordered_distances) != length(unique(ordered_distances))) {
    stop("Distances are not unique.")
  }


  # We store the junctions in a list of character vectors that correspond to the
  # indexes of the coordinates stored in all_coordinates (i.e., a vector of the
  # strings "1" and "2" represents a junction between the 1st and 2nd
  # coordinates in the input). We use character vectors as we want to use grep
  # to check which junctions contain coordinates in potential new connections.
  junctions_list <- list()


  # To speed up the for loop slightly, we put the first junction into the above
  # list then iterate over the remaining shortest distances.
  initial_coorinate_indexes <- which(matrix_of_distances == ordered_distances[1], arr.ind = TRUE)
  junctions_list[[1]] <- c(as.character(initial_coorinate_indexes[1]), as.character(initial_coorinate_indexes[2]))


  for (i in 2:number_of_connections) {

    coorinate_indexes <- which(matrix_of_distances == ordered_distances[i], arr.ind = TRUE)

    new_connection_x <- as.character(coorinate_indexes[1])
    new_connection_y <- as.character(coorinate_indexes[2])


    # We want to check if x or y are present in any of the vectors in
    # junctions_list - and if they are we want to know the index of that vector
    # in the list. Grep seems to work in this instance, but we need to format
    # the pattern to match first.
    res_x <- paste0("\"", new_connection_x, "\"") |>
      grep(x = junctions_list)
    res_y <- paste0("\"", new_connection_y, "\"") |>
      grep(x = junctions_list)

    if (length(res_x) > 0 && length(res_y) > 0) {
      if (res_x != res_y) {
        # If x and y both have matches and they are different (nothing happens
        # if the two coordinates are already connected to the same junction),
        # then we want to combine the junctions they match with into one
        # junction, add it to the list, and remove the original two junctions
        # that we have merged.
        new_junction <- c(junctions_list[[res_x]], junctions_list[[res_y]])

        junctions_list[[length(junctions_list) + 1]] <- new_junction

        junctions_list <- junctions_list[-c(res_x, res_y)]
      }

    } else if (length(res_x) > 0 && !(length(res_y) > 0)) {
      # If only x has a match we want to add y to the junction that x matches.
      junctions_list[[res_x]] <- c(junctions_list[[res_x]], new_connection_y)

    } else if (!(length(res_x) > 0) && length(res_y) > 0) {
      # Similarly, if only y has a match we want to add x to the junction that y
      # matches.
      junctions_list[[res_y]] <- c(junctions_list[[res_y]], new_connection_x)

    } else {
      # Finally, if neither x or y have any matches we add a new junction with
      # only x and y.
      new_junction <- c(new_connection_x, new_connection_y)
      junctions_list[[length(junctions_list) + 1]] <- new_junction

    }
  }


  # Return the answer.
  prod_three_biggest_junctions <- junctions_list |>
    lapply(length) |>
    unlist() |>
    sort(decreasing = TRUE) |>
    head(n = 3) |>
    prod()

  return(prod_three_biggest_junctions)
}


part_one <- get_prod_three_biggest_junctions(input = all_input, number_of_connections = 1000)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 175500





# Part 2 ----

# This is a very similar function to part one, except we continue to iterate
# over the shortest distances until we the length of the first junction is the
# same length as the input.
generate_one_big_circuit <- function(input) {

  # We store the distances in a matrix, and the coordinates in a list. Note that
  # we use integers  for the distances as we omit taking the square root when
  # calculating the Euclidean distance between points - this is an unnecessary
  # step if we only wish to know what points the shortest distances are between,
  # rather than the value of the distances.
  coordinate_count <- length(input)
  matrix_of_distances <- matrix(NA_integer_, nrow = coordinate_count, ncol = coordinate_count)

  all_coordinates <- input |>
    lapply(strsplit, split = ",") |>
    unlist(recursive = FALSE) |>
    lapply(as.numeric)

  for (i in 1:coordinate_count) {
    for (j in 1:coordinate_count) {
      if (i < j) {
        x <- all_coordinates[[i]]

        y <- all_coordinates[[j]]

        matrix_of_distances[i, j] <- (x[1] - y[1])^2 + (x[2] - y[2])^2 + (x[3] - y[3])^2
      }
    }
  }

  # We want to make sure that the distances we are going to check are unique, as
  # we don't want to match multiple sets of coordinates - this isn't a problem
  # for my input and would require some more complex logic.
  ordered_distances <- sort(matrix_of_distances, decreasing = FALSE)

  if (length(ordered_distances) != length(unique(ordered_distances))) {
    stop("Distances are not unique.")
  }


  # We store the junctions in a list of character vectors that correspond to the
  # indexes of the coordinates stored in all_coordinates (i.e., a vector of the
  # strings "1" and "2" represents a junction between the 1st and 2nd
  # coordinates in the input). We use character vectors as we want to use grep
  # to check which junctions contain coordinates in potential new connections.
  junctions_list <- list()


  # To speed up the for loop slightly, we put the first junction into the above
  # list then iterate over the remaining shortest distances.
  initial_coorinate_indexes <- which(matrix_of_distances == ordered_distances[1], arr.ind = TRUE)
  junctions_list[[1]] <- c(as.character(initial_coorinate_indexes[1]), as.character(initial_coorinate_indexes[2]))


  for (i in 2:length(ordered_distances)) {

    coorinate_indexes <- which(matrix_of_distances == ordered_distances[i], arr.ind = TRUE)

    new_connection_x <- as.character(coorinate_indexes[1])
    new_connection_y <- as.character(coorinate_indexes[2])


    # We want to check if x or y are present in any of the vectors in
    # junctions_list - and if they are we want to know the index of that vector
    # in the list. Grep seems to work in this instance, but we need to format
    # the pattern to match first.
    res_x <- paste0("\"", new_connection_x, "\"") |>
      grep(x = junctions_list)
    res_y <- paste0("\"", new_connection_y, "\"") |>
      grep(x = junctions_list)

    if (length(res_x) > 0 && length(res_y) > 0) {
      if (res_x != res_y) {
        # If x and y both have matches and they are different (nothing happens
        # if the two coordinates are already connected to the same junction),
        # then we want to combine the junctions they match with into one
        # junction, add it to the list, and remove the original two junctions
        # that we have merged.
        new_junction <- c(junctions_list[[res_x]], junctions_list[[res_y]])

        junctions_list[[length(junctions_list) + 1]] <- new_junction

        junctions_list <- junctions_list[-c(res_x, res_y)]
      }

    } else if (length(res_x) > 0 && !(length(res_y) > 0)) {
      # If only x has a match we want to add y to the junction that x matches.
      junctions_list[[res_x]] <- c(junctions_list[[res_x]], new_connection_y)

    } else if (!(length(res_x) > 0) && length(res_y) > 0) {
      # Similarly, if only y has a match we want to add x to the junction that y
      # matches.
      junctions_list[[res_y]] <- c(junctions_list[[res_y]], new_connection_x)

    } else {
      # Finally, if neither x or y have any matches we add a new junction with
      # only x and y.
      new_junction <- c(new_connection_x, new_connection_y)
      junctions_list[[length(junctions_list) + 1]] <- new_junction

    }

    if (length(junctions_list[[1]]) == coordinate_count) {
      break
    }
  }


  # Return the value of the last two x coordinates multiplied together.
  value <- all_coordinates[[as.numeric(new_connection_x)]][1] * all_coordinates[[as.numeric(new_connection_y)]][1]

  return(value)
}


part_two <- generate_one_big_circuit(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 6934702555
