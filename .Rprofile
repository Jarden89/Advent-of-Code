# 0. renv ----
source("renv/activate.R")




# 1. Styler Options ----

# Silence styler messages about the cache being cleared in x number of days when
# opening the project.
options(styler.cache_root = "styler")


# Custom styletr based on the tidyverse styler with a few tweaks.
.custom_styler <- styler::tidyverse_style()

# Can have as many lines between top level expressions as you like.
.custom_styler$line_break$set_line_breaks_between_top_level_exprs <- NULL

# Can have line breaks immediately after the start of a function.
.custom_styler$line_break$style_line_break_around_curly <- NULL

# Set the custom styler for this project.
options(
  styler.addins_style_transformer = ".custom_styler"
)
