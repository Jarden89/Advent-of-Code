# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_11.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_11_input.txt"))





# Part 1 ----

# Generic function that can be used for both parts one and two.
count_paths_from_to <- function(input, from = "you", to = "out") {

  start_node <- from
  final_node <- to

  # Vector of strings of the device names.
  device_names <- input |>
    strsplit(split = ":") |>
    sapply(\(x) x[1])

  # List of outputs of each device, which we name with the vector above so that
  # we can get the outputs with device_outputs[["name"]].
  device_outputs <- input |>
    strsplit(split = ": ") |>
    sapply(\(x) x[2]) |>
    strsplit(" ")
  names(device_outputs) <- device_names

  # Starting vector that we loop over. This is a named numeric vector of the
  # number of paths to each node. The basic idea is very similar to Day 7 -
  # rather than looking at the individual paths we keep track of the number of
  # paths to each device.
  nodes_to_follow <- c(1)
  names(nodes_to_follow) <- start_node

  # Number of paths to final_node, which we return.
  final_paths_count <- 0

  while (length(nodes_to_follow) > 0) {

    # Create an empty list to replace nodes_to_follow.
    new_nodes_to_follow <- list()

    for (start in names(nodes_to_follow)) {

      # Get the current number of paths to the starting node.
      path_count <- nodes_to_follow[[start]]

      # If we are at the final node we skip checking the nodes it leads to and
      # add to the final_paths_count.
      if (start == final_node) {
        final_paths_count <- final_paths_count + path_count
        next
      }

      # If the node is "out" we also skip - this node doesn't lead anywhere.
      if (start == "out") {
        next
      }

      # Otherwise we iterate through the output nodes and count the paths to
      # each. Note that if an element is unspecified in a list calling
      # list[["name"]] returns null - for the first start node this will always
      # be true, but we add to existing paths for each node after.
      start_outputs <- device_outputs[[start]]

      for (node in start_outputs) {

        if (is.null(new_nodes_to_follow[[node]])) {
          new_nodes_to_follow[[node]] <- path_count
        } else {
          new_nodes_to_follow[[node]] <- new_nodes_to_follow[[node]] + path_count
        }

      }

    }

    # We unlist here to get another named numeric vector to iterate
    nodes_to_follow <- unlist(new_nodes_to_follow)
  }


  return(final_paths_count)
}


part_one <- count_paths_from_to(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 662





# Part 2 ----

# We can use our function from part one to calculate the number of paths from
# "svr" to "out" that pass through "fft" and "dac" with the following
# calculation.
part_two <-
  (count_paths_from_to(input = all_input, from = "svr", to = "fft") * count_paths_from_to(input = all_input, from = "fft", to = "dac") * count_paths_from_to(input = all_input, from = "dac", to = "out")) +
  (count_paths_from_to(input = all_input, from = "svr", to = "dac") * count_paths_from_to(input = all_input, from = "dac", to = "fft") * count_paths_from_to(input = all_input, from = "fft", to = "out"))
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 429399933071120
