# Styler Options ----

# Silence styler messages about the cache being cleared in x number of days
# when opening the project.
options(styler.cache_root = "styler")





# Custom Styler ----

# Based on the tidyverse styler with a few tweaks.
.custom_styler <- styler::tidyverse_style()
# Can have as many lines between top level expressions as you like.
.custom_styler$line_break$set_line_breaks_between_top_level_exprs <- NULL
# Can have line breaks immediately after the start of a function.
.custom_styler$line_break$style_line_break_around_curly <- NULL
options(
  styler.addins_style_transformer = ".custom_styler"
)





# Advent of Code Functions ----

# Function to load the input data for the day from the local directory. If the
# data does not exist locally the function will attempt to download it. This
# requires saving a copy of the adventofcode.com session cookie in .Renviron as
# AOC_SESSION - this can be done with `usethis::edit_r_environ()`. Note that the
# `input_url` is an optional argument that can be specified if the URL to
# download the input doesn't match the expected URL.
.get_todays_input <- function(input_url = NULL) {

  # This returns the path of the file that is being sourced - from this we can
  # get the year (the folder that the file is in) and day (should be in the
  # name of the file) that we want to load.
  source_path <- scriptloc::scriptloc() |>
    strsplit(split = "/") |>
    unlist()

  year <- source_path[length(source_path) - 1]

  day <- source_path[length(source_path)] |>
    substr(start = 5, stop = 6)

  # This is the URL that we should be able to download the input from.
  if (is.null(input_url)) {
    input_url <- paste0("https://adventofcode.com/", year, "/day/", as.integer(day), "/input")
  }

  # This is the path that the local input should be saved at.
  local_input_path <- here::here("Data", year, paste0(year, "_", day, "_", "input.txt"))


  if (file.exists(local_input_path)) {
    # If the file exists locally we open it and return it.
    todays_input <- readLines(local_input_path, warn = FALSE)

  } else {
    # Otherwise we download the input from the input_url.

    # First we need to make sure there's a folder to save the input in.
    dir.create(path = here::here("Data", year), showWarnings = FALSE)

    # Download using `httr2`.
    todays_input_string <- httr2::request(input_url) |>
      httr2::req_cookies_set(session = Sys.getenv("AOC_SESSION")) |>
      # The user agent is just a link to the github project.
      httr2::req_user_agent(Sys.getenv("AOC_USER_AGENT")) |>
      httr2::req_perform() |>
      httr2::resp_body_string()

    # Then we create the file and write the lines from the input string.
    local_input_con <- file(local_input_path)
    writeLines(text = todays_input_string, con = local_input_con, sep = "")
    close(local_input_con)

    # Finally, we load the input.
    todays_input <- readLines(local_input_path, warn = FALSE)
  }


  return(todays_input)
}
