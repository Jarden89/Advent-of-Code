# 0. Location and Packages ----

## 0.1 Packages ----

suppressWarnings(suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(tictoc)
}))


## 0.2 Location ----

suppressMessages({
  i_am("20YY/Day_DD.R")
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




part_one <- NULL
log_info("Part One Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part One: {part_one}")
#





# Part 2 ----

tic()




part_two <- NULL
log_info("Part Two Complete: {toc(quiet = TRUE)$callback_msg}")
log_success("Part Two: {part_two}")
#





# End ----

log_info("Total time elapsed: {toc(quiet = TRUE)$callback_msg}")
