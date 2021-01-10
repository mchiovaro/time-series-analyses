#### Load necessary packages ####
#
# Last updated: 2020_08_11

# list of required packages
required_packages = c(
  'dplyr',
  'ggplot2',
  'crqa',
  'tidyr',
  'modeest',
  'papaja',
  'pander',
  'gtools',
  'knitr',
  'statip',
  'gridExtra',
  'png',
  'data.table',
  'biwavelet',
  'stringr',
  'lubridate',
  'stringi'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))
