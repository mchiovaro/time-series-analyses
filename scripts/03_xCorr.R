######## Cross Correlation ########
#
# This script runs cross correlation on
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_01_21

#### 1. Set up ####

# read in data
data <- read.csv("./data/formatted/formatted_data.csv")

# make ts variables
coh <- ts(data$coh_deciles)
all_st <- ts(data$all_deciles_source_target)
pos_st <- ts(data$pos_deciles_source_target)
neg_st <- ts(data$neg_deciles_source_target)
all_t <- ts(data$all_deciles_target)
pos_t <- ts(data$pos_deciles_target)
neg_t <- ts(data$neg_deciles_target)

#### 2. ADF test for stationarity ####
(adf_coh <- adf.test(coh)) # non-stationary
(adf_all_st <- adf.test(all_st)) # non-stationary
(adf_pos_st <- adf.test(pos_st)) # non-stationary
(adf_neg_st <- adf.test(neg_st))
(adf_all_t <- adf.test(all_t)) # non-stationary
(adf_pos_t <- adf.test(pos_t)) # non-stationary
(adf_neg_t <- adf.test(neg_t))

#### 3. Fix non-stationary time series and check again ####

# take first difference to assure stationarity
coh <- diff(coh, differences=1)
all_st <- diff(all_st, differences=1)
pos_st <- diff(pos_st, differences=1)
neg_st <- diff(neg_st, differences=1)
all_t <- diff(all_t, differences=1)
pos_t <- diff(pos_t, differences=1)
neg_t <- diff(neg_t, differences=1)

# retest for stationarity
(adf_coh <- adf.test(coh)) 
(adf_all_st <- adf.test(all_st)) 
(adf_pos_st <- adf.test(pos_st)) 
(adf_neg_st <- adf.test(neg_st)) 
(adf_all_t <- adf.test(all_t)) 
(adf_pos_t <- adf.test(pos_t)) 
(adf_neg_t <- adf.test(neg_t)) 

#### 4. Run cross-correlation and save plots

## target only pairs
tiff(file="./results/figures/ccf_all_t.tiff")
ccf(coh, all_t, type = "correlation", ylab = "Cross-correlation")
dev.off()
tiff(file="./results/figures/ccf_pos_t.tiff")
ccf(coh, pos_t, type = "correlation", ylab = "Cross-correlation")
dev.off()
tiff(file="./results/figures/ccf_neg_t.tiff")
ccf(coh, neg_t, type = "correlation", ylab = "Cross-correlation")
dev.off()

## source-target pairs
tiff(file="./results/figures/ccf_all_st.tiff")
ccf(coh, all_st, type = "correlation", ylab = "Cross-correlation")
dev.off()
tiff(file="./results/figures/ccf_pos_st.tiff")
ccf(coh, pos_st, type = "correlation", ylab = "Cross-correlation")
dev.off()
tiff(file="./results/figures/ccf_neg_st.tiff")
ccf(coh, neg_st, type = "correlation", ylab = "Cross-correlation")
dev.off()
