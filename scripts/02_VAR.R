######## Vector Autoregression ########
#
# This script runs vector autoregression on
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_01_21

#### 1. Set up ####

# load packages
library(vars)
library(tseries)
library(tidyverse)
library(stargazer)

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

# create pairs
data_all_st <- cbind(coh, all_st)
data_pos_st <- cbind(coh, pos_st)
data_neg_st <- cbind(coh, neg_st)
data_all_t <- cbind(coh, all_t)
data_pos_t <- cbind(coh, pos_t)
data_neg_t <- cbind(coh, neg_t)

#### 4. Determine optimal lag ####
(lag <- VARselect(data_all_st)) # AIC 6, SC 2
(lag <- VARselect(data_pos_st)) # AIC 2, SC 1
(lag <- VARselect(data_neg_st)) # AIC 2, SC 2 
(lag <- VARselect(data_all_t)) # AIC 6, SC 1
(lag <- VARselect(data_pos_t)) # AIC 4, SC 1
(lag <- VARselect(data_neg_t)) # AIC 8, SC 1

#### 5. Run VAR, check stability, and Granger causality for target pairs ####

### Run VAR

## all events
VAR_all_t_sc <- VAR(data_all_t, p = 2, type = "none")
stargazer(VAR_all_t_sc[["varresult"]], type = 'text')
VAR_all_t_aic <- VAR(data_all_t, p = 6, type = "none")
stargazer(VAR_all_t_aic[["varresult"]], type = 'text')

## pos events
VAR_pos_t_sc <- VAR(data_pos_t, p = 1, type = "none")
stargazer(VAR_pos_t_sc[["varresult"]], type = 'text')
VAR_pos_t_aic <- VAR(data_pos_t, p = 2, type = "none")
stargazer(VAR_pos_t_aic[["varresult"]], type = 'text')

## neg events
VAR_neg_t_sc <- VAR(data_neg_t, p = 2, type = "none")
stargazer(VAR_neg_t_sc[["varresult"]], type = 'text')
VAR_neg_t_aic <- VAR(data_neg_t, p = 2, type = "none")
stargazer(VAR_neg_t_aic[["varresult"]], type = 'text')

## check for stability (< 1 is stable)
roots(VAR_all_t_sc, modulus = TRUE)
roots(VAR_all_t_aic, modulus = TRUE)
roots(VAR_pos_t_sc, modulus = TRUE)
roots(VAR_pos_t_aic, modulus = TRUE)
roots(VAR_neg_t_sc, modulus = TRUE)
roots(VAR_neg_t_aic, modulus = TRUE)

### Granger causality for SC (sig p value = Granger-casuality)

## create data frame for saving Granger causality
granger_t <- as.data.frame(matrix(0, ncol = 12, nrow = 4)) 
colnames(granger_t) <- c("coh_all_sc", "all_sc", 
                         "coh_pos_sc", "pos_sc", 
                         "coh_neg_sc", "neg_sc", 
                         "coh_all_aic", "all_aic", 
                         "coh_pos_aic", "pos_aic", 
                         "coh_neg_aic", "neg_aic")
rownames(granger_t) <- c("F", "df1", "df2", "p-value")

## all events
granger_coh <- causality(VAR_all_t_sc, cause = "coh")
granger_t[1,1] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,1] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,1] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,1] <- granger_coh[["Granger"]][["p.value"]]
granger_all_t <- causality(VAR_all_t_sc, cause = "all_t")
granger_t[1,2] <- granger_all_t[["Granger"]][["statistic"]]
granger_t[2,2] <- granger_all_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,2] <- granger_all_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,2] <- granger_all_t[["Granger"]][["p.value"]]

## pos events
granger_coh <- causality(VAR_pos_t_sc, cause = "coh")
granger_t[1,3] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,3] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,3] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,3] <- granger_coh[["Granger"]][["p.value"]]
granger_pos_t <- causality(VAR_pos_t_sc, cause = "pos_t")
granger_t[1,4] <- granger_pos_t[["Granger"]][["statistic"]]
granger_t[2,4] <- granger_pos_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,4] <- granger_pos_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,4] <- granger_pos_t[["Granger"]][["p.value"]]

## neg events
granger_coh <- causality(VAR_neg_t_sc, cause = "coh")
granger_t[1,5] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,5] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,5] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,5] <- granger_coh[["Granger"]][["p.value"]]
granger_neg_t <- causality(VAR_neg_t_sc, cause = "neg_t")
granger_t[1,6] <- granger_neg_t[["Granger"]][["statistic"]]
granger_t[2,6] <- granger_neg_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,6] <- granger_neg_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,6] <- granger_neg_t[["Granger"]][["p.value"]]

### Granger causality for AIC (sig p value = Granger-casuality)

## all events
granger_coh <- causality(VAR_all_t_aic, cause = "coh")
granger_t[1,7] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,7] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,7] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,7] <- granger_coh[["Granger"]][["p.value"]]
granger_all_t <- causality(VAR_all_t_aic, cause = "all_t")
granger_t[1,8] <- granger_all_t[["Granger"]][["statistic"]]
granger_t[2,8] <- granger_all_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,8] <- granger_all_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,8] <- granger_all_t[["Granger"]][["p.value"]]

## pos events
granger_coh <- causality(VAR_pos_t_aic, cause = "coh")
granger_t[1,9] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,9] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,9] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,9] <- granger_coh[["Granger"]][["p.value"]]
granger_pos_t <- causality(VAR_pos_t_aic, cause = "pos_t")
granger_t[1,10] <- granger_pos_t[["Granger"]][["statistic"]]
granger_t[2,10] <- granger_pos_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,10] <- granger_pos_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,10] <- granger_pos_t[["Granger"]][["p.value"]]

## neg events
granger_coh <- causality(VAR_neg_t_aic, cause = "coh")
granger_t[1,11] <- granger_coh[["Granger"]][["statistic"]]
granger_t[2,11] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_t[3,11] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_t[4,11] <- granger_coh[["Granger"]][["p.value"]]
granger_neg_t <- causality(VAR_neg_t_aic, cause = "neg_t")
granger_t[1,12] <- granger_neg_t[["Granger"]][["statistic"]]
granger_t[2,12] <- granger_neg_t[["Granger"]][["parameter"]][["df1"]]
granger_t[3,12] <- granger_neg_t[["Granger"]][["parameter"]][["df2"]]
granger_t[4,12] <- granger_neg_t[["Granger"]][["p.value"]]

#### 6. Run VAR, check stability, and Granger causality for source-target pairs ####

### Run VAR

## all events
VAR_all_st_sc <- VAR(data_all_st, p = 1, type = "none")
stargazer(VAR_all_st_sc[["varresult"]], type = 'text')
VAR_all_st_aic <- VAR(data_all_st, p = 6, type = "none")
stargazer(VAR_all_t_aic[["varresult"]], type = 'text')

## pos events
VAR_pos_st_sc <- VAR(data_pos_st, p = 1, type = "none")
stargazer(VAR_pos_st_sc[["varresult"]], type = 'text')
VAR_pos_st_aic <- VAR(data_pos_st, p = 4, type = "none")
stargazer(VAR_pos_st_aic[["varresult"]], type = 'text')

## neg events
VAR_neg_st_sc <- VAR(data_neg_st, p = 1, type = "none")
stargazer(VAR_neg_st_sc[["varresult"]], type = 'text')
VAR_neg_st_aic <- VAR(data_neg_st, p = 8, type = "none")
stargazer(VAR_neg_st_aic[["varresult"]], type = 'text')

## check for stability (< 1 is stable)
roots(VAR_all_st_sc, modulus = TRUE)
roots(VAR_all_st_aic, modulus = TRUE)
roots(VAR_pos_st_sc, modulus = TRUE)
roots(VAR_pos_st_aic, modulus = TRUE)
roots(VAR_neg_st_sc, modulus = TRUE)
roots(VAR_neg_st_aic, modulus = TRUE)

### Granger causality for SC (sig p value = Granger-casuality)

## create data frame for saving Granger causality
granger_st <- as.data.frame(matrix(0, ncol = 12, nrow = 4)) 
colnames(granger_st) <- c("coh_all_sc", "all_sc", 
                         "coh_pos_sc", "pos_sc", 
                         "coh_neg_sc", "neg_sc", 
                         "coh_all_aic", "all_aic", 
                         "coh_pos_aic", "pos_aic", 
                         "coh_neg_aic", "neg_aic")
rownames(granger_st) <- c("F", "df1", "df2", "p-value")

## all events
granger_coh <- causality(VAR_all_st_sc, cause = "coh")
granger_st[1,1] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,1] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,1] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,1] <- granger_coh[["Granger"]][["p.value"]]
granger_all_st <- causality(VAR_all_st_sc, cause = "all_st")
granger_st[1,2] <- granger_all_st[["Granger"]][["statistic"]]
granger_st[2,2] <- granger_all_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,2] <- granger_all_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,2] <- granger_all_st[["Granger"]][["p.value"]]

## pos events
granger_coh <- causality(VAR_pos_st_sc, cause = "coh")
granger_st[1,3] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,3] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,3] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,3] <- granger_coh[["Granger"]][["p.value"]]
granger_pos_st <- causality(VAR_pos_st_sc, cause = "pos_st")
granger_st[1,4] <- granger_pos_st[["Granger"]][["statistic"]]
granger_st[2,4] <- granger_pos_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,4] <- granger_pos_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,4] <- granger_pos_st[["Granger"]][["p.value"]]

## neg events
granger_coh <- causality(VAR_neg_st_sc, cause = "coh")
granger_st[1,5] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,5] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,5] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,5] <- granger_coh[["Granger"]][["p.value"]]
granger_neg_st <- causality(VAR_neg_st_sc, cause = "neg_st")
granger_st[1,6] <- granger_neg_st[["Granger"]][["statistic"]]
granger_st[2,6] <- granger_neg_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,6] <- granger_neg_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,6] <- granger_neg_st[["Granger"]][["p.value"]]

### Granger causality for AIC (sig p value = Granger-casuality)

## all events
granger_coh <- causality(VAR_all_st_aic, cause = "coh")
granger_st[1,7] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,7] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,7] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,7] <- granger_coh[["Granger"]][["p.value"]]
granger_all_st <- causality(VAR_all_st_aic, cause = "all_st")
granger_st[1,8] <- granger_all_st[["Granger"]][["statistic"]]
granger_st[2,8] <- granger_all_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,8] <- granger_all_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,8] <- granger_all_st[["Granger"]][["p.value"]]

## pos events
granger_coh <- causality(VAR_pos_st_aic, cause = "coh")
granger_st[1,9] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,9] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,9] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,9] <- granger_coh[["Granger"]][["p.value"]]
granger_pos_st <- causality(VAR_pos_st_aic, cause = "pos_st")
granger_st[1,10] <- granger_pos_st[["Granger"]][["statistic"]]
granger_st[2,10] <- granger_pos_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,10] <- granger_pos_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,10] <- granger_pos_st[["Granger"]][["p.value"]]

## neg events
granger_coh <- causality(VAR_neg_st_aic, cause = "coh")
granger_st[1,11] <- granger_coh[["Granger"]][["statistic"]]
granger_st[2,11] <- granger_coh[["Granger"]][["parameter"]][["df1"]]
granger_st[3,11] <- granger_coh[["Granger"]][["parameter"]][["df2"]]
granger_st[4,11] <- granger_coh[["Granger"]][["p.value"]]
granger_neg_st <- causality(VAR_neg_st_aic, cause = "neg_st")
granger_st[1,12] <- granger_neg_st[["Granger"]][["statistic"]]
granger_st[2,12] <- granger_neg_st[["Granger"]][["parameter"]][["df1"]]
granger_st[3,12] <- granger_neg_st[["Granger"]][["parameter"]][["df2"]]
granger_st[4,12] <- granger_neg_st[["Granger"]][["p.value"]]
