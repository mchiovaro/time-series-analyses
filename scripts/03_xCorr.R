######## Cross Correlation ########
#
# This script runs cross correlation on
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_02_01

#### 1. Set up ####

# read in data
data <- read.csv("./data/formatted/formatted_data.csv")

# make ts variables
coh <- ts(data$coh_deciles)
all_t <- ts(data$all_deciles_target)

#### 2. ADF test for stationarity ####
(adf_coh <- adf.test(coh)) # non-stationary
(adf_all_t <- adf.test(all_t)) # non-stationary

#### 3. Fix non-stationary time series and check again ####

# take first difference to assure stationarity
coh <- diff(coh, differences=1)
all_t <- diff(all_t, differences=1)

# retest for stationarity
(adf_coh <- adf.test(coh)) 
(adf_all_t <- adf.test(all_t)) 

#### 4. Run cross-correlation and save plot

tiff(file="./results/xcorr/stats_ccf_all_t.tiff")
stats::ccf(coh, all_t, type = "correlation", ylab = "Cross-correlation")
dev.off()

