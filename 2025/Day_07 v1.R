# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_07.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_07_input.txt"))





# Part 1 ----

count_beam_splits <- function(input) {

  manifold <- input |>
    strsplit(split = "")

  # We assume all lines are the same length.
  max_beam_index <- length(manifold[[1]])

  # Counter for the number of times a beam has been split.
  split_couter <- 0L

  # Vector to keep track of the locations of the beams.
  beam_indexes <- integer(0)

  for (row in manifold) {

    # Check the rows of the beams that already exist.
    if (length(beam_indexes) > 0) {

      # Columns with a beam and without a splitter are kept.
      beams_continue_in <- beam_indexes[which(row[beam_indexes] != "^")]

      # Columns with a beam that have a splitter produce two beams - one in the
      # index below and one in the index above.
      beams_split_in <- beam_indexes[which(row[beam_indexes] == "^")]

      # May as well nest this logic in an if statement - half the time the beams
      # won't be split.
      if (length(beams_split_in) > 0) {
        beats_split_lower <- beams_split_in - 1
        beats_split_upper <- beams_split_in + 1

        # Here we increase the split counter.
        split_couter <- split_couter + length(beams_split_in)

        # These are the new indexes, but we want to make sure they are all
        # unique, positive, and not larger than the maximum beam index. This
        # problem doesn't occur in my input, but we may as well make sure it's
        # accounted for.
        new_beam_indexes <- unique(c(beams_continue_in, beats_split_lower, beats_split_upper))

        beam_indexes <- new_beam_indexes[which(0 <= new_beam_indexes & new_beam_indexes <= max_beam_index)]
      }
    }


    # Are there any new beams in the row? This should only happen in the first
    # row (it does in my input), but we want to be able to account for it
    # happening anywhere.
    new_beams <- which(row == "S")
    if (length(new_beams) > 0) {
      beam_indexes <- unique(c(beam_indexes, new_beams))
    }
  }

  return(split_couter)
}


part_one <- count_beam_splits(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 1649





# Part 2 ----

# A very similar function to part one but we now no longer need to keep track
# of the number of splits. We do need to keep track of the number of paths that
# have been generated with each row. We do this with the path_counts vector,
# which keeps track of the number of paths to each beam in a row.
count_beam_paths <- function(input) {

  manifold <- input |>
    strsplit(split = "")

  # We assume all lines are the same length.
  max_beam_index <- length(manifold[[1]])

  # This vector is essentially a list of how many paths there are to each index,
  # and is updated for each row with the loop below.
  path_counts <- rep(0, max_beam_index)

  # Vector to keep track of the locations of the beams.
  beam_indexes <- integer(0)

  for (row in manifold) {

    # Check the rows of the beams that already exist.
    if (length(beam_indexes) > 0) {

      # Columns with a beam and without a splitter are kept.
      beams_continue_in <- beam_indexes[which(row[beam_indexes] != "^")]

      # Columns with a beam that have a splitter produce two beams - one in the
      # index below and one in the index above.
      beams_split_in <- beam_indexes[which(row[beam_indexes] == "^")]

      # May as well nest this logic in an if statement - half the time the beams
      # won't be split.
      if (length(beams_split_in) > 0) {

        beats_split_lower <- beams_split_in - 1
        beats_split_upper <- beams_split_in + 1

        # These are the new indexes, but we want to make sure they are all
        # unique, positive, and not larger than the maximum beam index. This
        # problem doesn't occur in my input, but we may as well make sure it's
        # accounted for.
        new_beam_indexes <- unique(c(beams_continue_in, beats_split_lower, beats_split_upper))

        beam_indexes <- new_beam_indexes[which(0 <= new_beam_indexes & new_beam_indexes <= max_beam_index)]

        # Here we do most of the work for getting the answer for part 2. Each
        # time a beam is split, three things happen:
        for (i in beams_split_in) {

          # 1.  At the index below, the number of paths to that index increases
          #     by however many paths there are to the current index.
          if (i - 1 >= 0) {
            path_counts[i - 1] <- path_counts[i - 1] + path_counts[i]
          }

          # 2.  And the same for the index above.
          if (i + 1 <= max_beam_index) {
            path_counts[i + 1] <- path_counts[i + 1] + path_counts[i]
          }

          # 3.  There are now no paths to the current index, as there is a
          #     splitter here.
          path_counts[i] <- 0
        }
      }
    }


    # Are there any new beams in the row? This should only happen in the first
    # row (it does in my input), but we want to be able to account for it
    # happening anywhere.
    new_beams <- which(row == "S")
    if (length(new_beams) > 0) {
      beam_indexes <- unique(c(beam_indexes, new_beams))

      # We also want to add a 1 where there are new beams - this is the start of
      # new paths.
      for (i in new_beams) {
        path_counts[i] <- path_counts[i] + 1
      }
    }
  }


  total_path_counts <- sum(path_counts)

  return(total_path_counts)
}


part_two <- count_beam_paths(input = all_input)
logger::log_success("Part Two: {part_two}")
logger::log_elapsed("Part Two Complete")
# 16937871060075
