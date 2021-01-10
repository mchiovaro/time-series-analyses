######## Vector Autoregression ########
#
# This script runs vector autoregression on
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_01_10


#### 1. Set up ####
# install.packages("vars", "tseries", "tidyverse", "stargazer")
# install.packages('vars',repos='http://cran.us.r-project.org')
# install.packages('stargazer',repos='http://cran.us.r-project.org')
# install.packages('tseries',repos='http://cran.us.r-project.org')
# install.packages('tidyverse',repos='http://cran.us.r-project.org')

# load packages
library(vars)
library(tseries)
library(tidyverse)
library(stargazer)

# setwd("./Documents/_github/time-series-analyses/")

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

#### 2. Test for stationarity ####
# ADF tests for stationarity
(adf_coh <- adf.test(coh)) # non-stationary
(adf_all_st <- adf.test(all_st)) # non-stationary
(adf_pos_st <- adf.test(pos_st)) # non-stationary
(adf_neg_st <- adf.test(neg_st))
(adf_all_t <- adf.test(all_t)) # non-stationary
(adf_pos_t <- adf.test(pos_t)) # non-stationary
(adf_neg_t <- adf.test(neg_t))

#### 3. Fix non-stationary time series and check again ####

# make non-stationary time series stationary via first difference
coh <- diff(coh, differences=1)
all_st <- diff(all_st, differences=1)
pos_st <- diff(pos_st, differences=1)
all_t <- diff(all_t, differences=1)
pos_t <- diff(pos_t, differences=1)

# retest for stationarity
(adf_coh <- adf.test(coh)) # non-stationary
(adf_all_st <- adf.test(all_st)) # non-stationary
(adf_pos_st <- adf.test(pos_st)) # non-stationary
(adf_all_t <- adf.test(all_t)) # non-stationary
(adf_pos_t <- adf.test(pos_t)) # non-stationary

# create pairs
data_all_st <- cbind(coh, all_st)
data_pos_st <- cbind(coh, pos_st)
data_neg_st <- cbind(coh, neg_st)
data_all_t <- cbind(coh, all_t)
data_pos_t <- cbind(coh, pos_t)
data_neg_t <- cbind(coh, neg_t)

# trim first row from non-differenced pairs due to NAs from differencing
data_neg_st <- ts(data_neg_st[-1,])
data_neg_t <- ts(data_neg_t[-1,])

#### 4. Determine optimal lag ####
(lag <- VARselect(data_all_st)) # AIC 6
(lag <- VARselect(data_pos_st)) # AIC 2
(lag <- VARselect(data_neg_st)) # AIC 6
(lag <- VARselect(data_all_t)) # AIC 6
(lag <- VARselect(data_pos_t)) # AIC 4
(lag <- VARselect(data_neg_t)) # AIC 8

#### 5. Run VAR, check stability, and Granger causality for target pairs ####

## Run VAR
# all events
VAR_all_t <- VAR(data_all_t, p = 6, type = "none")
stargazer(VAR_all_t[["varresult"]], type = 'text')
# pos events
VAR_pos_t <- VAR(data_pos_t, p = 4, type = "none")
stargazer(VAR_pos_t[["varresult"]], type = 'text')
# neg events
VAR_neg_t <- VAR(data_neg_t, p = 8, type = "none")
stargazer(VAR_neg_t[["varresult"]], type = 'text')

## check for stability (< 1 is stable)
roots(VAR_all_t, modulus = TRUE)
roots(VAR_pos_t, modulus = TRUE)
roots(VAR_neg_t, modulus = TRUE)

## Granger causality (sig p value = Granger-casuality)
# all events
granger_coh <- causality(VAR_all_t, cause = "coh")
granger_coh$Granger
granger_all_t <- causality(VAR_all_t, cause = "all_t")
granger_all_t$Granger
# pos events
granger_coh <- causality(VAR_pos_t, cause = "coh")
granger_coh$Granger
granger_pos_t <- causality(VAR_pos_t, cause = "pos_t")
granger_pos_t$Granger
# neg events
granger_coh <- causality(VAR_neg_t, cause = "coh")
granger_coh$Granger
granger_neg_t <- causality(VAR_neg_t, cause = "neg_t")
granger_neg_t$Granger

#### 5. Run VAR, check stability, and Granger causality for source-target pairs ####

## Run VAR
# all events
VAR_all_st <- VAR(data_all_st, p = 6, type = "none")
stargazer(VAR_all_st[["varresult"]], type = 'text')
# pos events
VAR_pos_st <- VAR(data_pos_st, p = 2, type = "none")
stargazer(VAR_pos_st[["varresult"]], type = 'text')
# neg events
VAR_neg_st <- VAR(data_neg_st, p = 6, type = "none")
stargazer(VAR_neg_st[["varresult"]], type = 'text')

## check for stability (< 1 is stable)
roots(VAR_all_st, modulus = TRUE)
roots(VAR_pos_st, modulus = TRUE)
roots(VAR_neg_st, modulus = TRUE)

## Granger causality (sig p value = Granger-casuality)
# all events
granger_coh <- causality(VAR_all_st, cause = "coh")
granger_coh$Granger
granger_all_st <- causality(VAR_all_st, cause = "all_st")
granger_all_st$Granger
# pos events
granger_coh <- causality(VAR_pos_st, cause = "coh")
granger_coh$Granger
granger_pos_st <- causality(VAR_pos_st, cause = "pos_st")
granger_pos_st$Granger
# neg events
granger_coh <- causality(VAR_neg_st, cause = "coh")
granger_coh$Granger
granger_neg_st <- causality(VAR_neg_st, cause = "neg_st")
granger_neg_st$Granger

