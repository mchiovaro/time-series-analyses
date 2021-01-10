#### Install missing packages ####
#
# Last updated: 2020_08_11

# list of required packages as strings
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

# install missing packages (adapted from <http://stackoverflow.com/a/4090208> and a-paxton/perception-memory-coordination)
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
