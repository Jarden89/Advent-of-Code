# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
}))


## 0.2 Location ----

suppressMessages({
  here::i_am("Day_12.R")
})


## 0.3 Options ----

logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::TRACE)
logger::log_elapsed_start()





# Read Input ----

all_input <- readLines(here("Data", "2025_12_input.txt"))





# Part 1 ----

# Turns out that the naive approach of assuming the worst packing is enough to
# solve today's puzzle. What a troll.
count_regions <- function(input) {

  tree_regions <- input[(max(grep(pattern = "^$", x = input)) + 1):length(input)] |>
    gsub(pattern = ":", replacement = "") |>
    strsplit(split = " ")

  region_areas <- tree_regions |>
    sapply(\(x) x[1]) |>
    strsplit(split = "x") |>
    sapply(\(x) as.numeric(x[1]) * as.numeric(x[2]))

  present_maximum_areas <- tree_regions |>
    lapply(\(x) x[-1]) |>
    lapply(as.numeric) |>
    sapply(\(x) sum(x) * 9)

  count <- sum(region_areas >= present_maximum_areas)

  return(count)
}


part_one <- count_regions(input = all_input)
logger::log_success("Part One: {part_one}")
logger::log_elapsed("Part One Complete")
# 425
