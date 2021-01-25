######## Formatting Data ########
#
# This script formats data related to the Arab Spring.
# Daily Twitter social cohesion and event data are processed 
# and transformed into deciles for CRQA and windowed CRQA
# analyses.
#
# Code by: M. Chiovaro (@mchiovaro) and A. Paxton (@a-paxton)
# University of Connecticut
# Last updated: 2021_01_08

#### 1. Set up ####

# clear environment
rm(list=ls())

# read in the data 
cohesion_df_original <- read.csv("./data/raw/syria_coherence_sort_5.csv")
ICEWS_df <- read.delim("./data/raw/events.2012.20150313084811.tab", 
                       header = TRUE, sep = "\t", quote = "")

# specify necessary libraries
library(dplyr)

#### 2. Filter and format the time series ####

### prep cohesion data ###
cohesion_df <- cohesion_df_original %>%
  
  mutate(Start.Year = 2012) %>%
  
  mutate(full_date <- paste(Start.Year, 
                       Start.Month, 
                       Start.Day, 
                       sep="-") 
         %>% ymd() 
         %>% as.Date()) %>%
  
  rename(Date=19) %>%
  
  group_by(Date) %>%
  
  summarise(n = n(),
            MeanCohesion = mean(Cohesion..avg., na.rm=T))

### filter for source and target ###

# filter full 2012 ICEWS data for correct dates and parameters
ICEWS_filtered <- ICEWS_df %>% 
  
  # fill blanks with NAs
  na_if("") %>%

  # turn factor to character for using grepl
  mutate(Event.Date = as.character(Event.Date)) %>%
  
  # filter out unusable dates
  filter(grepl("^(2012)[-](0[1-9]|1[012])[-](0[1-9]|[12][0-9]|3[01])$", 
               Event.Date)) %>%
  
  # turn to date format to use select for date range
  mutate_at(vars(Event.Date), as.Date, format = "%Y-%m-%d") %>%
  
  # grab the dates that align with old data
  filter(Event.Date >= as.Date("2012-03-30") 
         & Event.Date <= as.Date("2012-06-15")) %>%
  
  # change search params to characters for searching
  mutate_if(is.factor, as.character)  %>%
  
  # grab observations where either source or target is Syria
  filter(grepl("Syria", Source.Country) |
           grepl("Syria", Target.Country)) %>%
  
  # filter out invalid intensity formats
  filter(grepl("\\-*\\d+\\.*\\d*", Intensity)) %>%
  
  # convert intensity to numeric
  mutate(Intensity = as.numeric(Intensity)) %>%
  
  # filter out invalid intensity values
  filter(Intensity >= -10 & Intensity <= 10)

# look at the event types percentages for source and target
source_targ_positives <- sum(ICEWS_filtered$Intensity > 0)
source_targ_positives_perc <- sum(ICEWS_filtered$Intensity > 0)/nrow(ICEWS_filtered)
source_targ_negatives <- sum(ICEWS_filtered$Intensity < 0)
source_targ_negatives_perc <- sum(ICEWS_filtered$Intensity < 0)/nrow(ICEWS_filtered)

### prep the event count time series ###

# create new dataframe with counts of different events
ICEWS_formatted_source_target <- ICEWS_filtered %>% 
  
  # group by date
  group_by(Event.Date) %>%
  
  # count total number of events
  mutate(all_events_source_target = n()) %>%
  
  # count positive events
  mutate(pos_events_source_target = sum(Intensity > 0)) %>%
  
  # count negative events
  mutate(neg_events_source_target = sum(Intensity < 0)) %>%
  
  # sort descending so the mode will the be the more positive event
  mutate(Intensity = sort(Intensity, decreasing = TRUE)) %>%
  
  # calculate mode
  mutate(mode_source_target = statip::mfv1(Intensity, method = "mfv")) %>%
  
  # keep one unique row per day
  distinct(Event.Date, 
           all_events_source_target,
           pos_events_source_target, 
           neg_events_source_target, 
           mode_source_target) %>%
  
  # make date variable name match cohesion_df
  rename(Date = Event.Date) %>%
  
  # undo grouping by date
  ungroup()

### filter again with just target and prep count variables ###

# grab observations where target is Syria
ICEWS_formatted_target <- ICEWS_filtered %>% 
  
  # filter to just target (removing source)
  filter(grepl("Syria", Target.Country)) 

# look at the event types percentages for source and target
targ_positives <- sum(ICEWS_formatted_target$Intensity > 0)
targ_positives_perc <- sum(ICEWS_formatted_target$Intensity > 0)/nrow(ICEWS_formatted_target)
targ_negatives <- sum(ICEWS_formatted_target$Intensity < 0)
targ_negatives_perc <- sum(ICEWS_formatted_target$Intensity < 0)/nrow(ICEWS_formatted_target)

# grab observations where target is Syria
ICEWS_formatted_target <- ICEWS_formatted_target %>%
  
  # group by date
  group_by(Event.Date) %>%
  
  # count total number of events
  mutate(all_events_target = n()) %>%
  
  # count positive events
  mutate(pos_events_target = sum(Intensity > 0)) %>%
  
  # count negative events
  mutate(neg_events_target = sum(Intensity < 0)) %>%
  
  # sort descending so the mode will the be the more positive event
  mutate(Intensity = sort(Intensity, decreasing = TRUE)) %>%
  
  # calculate mode
  mutate(mode_target = statip::mfv1(Intensity, method = "mfv")) %>%
  
  # keep one unique row per day
  distinct(Event.Date, 
           all_events_target, 
           pos_events_target, 
           neg_events_target, 
           mode_target) %>%
  
  # make date variable name match cohesion_df
  rename(Date = Event.Date) %>%
  
  # undo grouping by date
  ungroup()

### combine all dataframes ###

# bind the event and social cohesion data frames
ICEWS_df_formatted <- dplyr::full_join(ICEWS_formatted_source_target,
                                       ICEWS_formatted_target,
                                       by=c("Date")) %>%
  dplyr::full_join(., cohesion_df,
                   by=c("Date")) %>%
  
  # remove 2020_03_30 for incomplete twitter data
  slice(2:n())

#### 3. Create the deciles for analyses ####

ICEWS_df_formatted <- ICEWS_df_formatted %>% ungroup() %>%
  
  # social cohesion
  mutate(coh_deciles = ntile(MeanCohesion, 10)) %>%
  
  # source and target: all events
  mutate(all_deciles_source_target = ntile(all_events_source_target, 10)) %>%
  
  # source and target: positive events
  mutate(pos_deciles_source_target = ntile(pos_events_source_target, 10)) %>%
  
  # source and target: negative events
  mutate(neg_deciles_source_target = ntile(neg_events_source_target, 10)) %>%
  
  # target only: all events
  mutate(all_deciles_target = ntile(all_events_target, 10)) %>%
  
  # target only: positive events
  mutate(pos_deciles_target = ntile(pos_events_target, 10)) %>%
  
  # target only: negative events
  mutate(neg_deciles_target = ntile(neg_events_target, 10))

### save to file ###

# write data to file
write.table(x = ICEWS_df_formatted,
            file='./data/formatted/formatted_data.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 4. Create randomized time series for permutation testing for CRQA ####

# set seed for reproducibility
set.seed(123)

## social cohesion ##

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  coh_shuffled <- sample(ICEWS_df_formatted$coh_deciles, replace = FALSE)
  sample <- t(as.data.frame(coh_shuffled))
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(ICEWS_df_formatted$coh_deciles)
original <- as.data.frame(t(original))
shuffled_coh <- rbind(shuffled_coh, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for CRQA: ",
             nrow(distinct(shuffled_coh))))
if(nrow(distinct(shuffled_coh)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
shuffled_coh <- as.data.frame(t(shuffled_coh))

# remove real time series from shuffled dataframe
shuffled_coh <- shuffled_coh[c(1:1000)]

### source and target ###

## count of events ##

# create empty data frame
shuffled_all_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled <- sample(ICEWS_df_formatted$all_deciles_source_target, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled))
  shuffled_all_source_target <- rbind(shuffled_all_source_target, sample)
}

# take the original time series and add it as a row
original_all <- as.data.frame(ICEWS_df_formatted$all_deciles_source_target)
original_all <- as.data.frame(t(original_all))
shuffled_all_source_target <- rbind(shuffled_all_source_target, original_all)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled all-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_all_source_target))))
if(nrow(distinct(shuffled_all_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_source_target <- as.data.frame(t(shuffled_all_source_target))

# remove real time series from shuffled dataframe
shuffled_all_source_target <- shuffled_all_source_target[c(1:1000)]

## count of positive events ##

# create empty data frame
shuffled_pos_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled <- sample(ICEWS_df_formatted$pos_deciles_source_target, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled))
  shuffled_pos_source_target <- rbind(shuffled_pos_source_target, sample)
}

# take the original time series and add it as a row
original_pos <- as.data.frame(ICEWS_df_formatted$pos_deciles_source_target)
original_pos <- as.data.frame(t(original_pos))
shuffled_pos_source_target <- rbind(shuffled_pos_source_target, original_pos)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled positive-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_pos_source_target))))
if(nrow(distinct(shuffled_pos_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_source_target <- as.data.frame(t(shuffled_pos_source_target))

# remove real time series from shuffled dataframe
shuffled_pos_source_target <- shuffled_pos_source_target[c(1:1000)]

## count of negative events ##

# create empty data frame
shuffled_neg_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled <- sample(ICEWS_df_formatted$neg_deciles_source_target, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled))
  shuffled_neg_source_target <- rbind(shuffled_neg_source_target, sample)
}

# take the original time series and add it as a row
original_neg <- as.data.frame(ICEWS_df_formatted$neg_deciles_source_target)
original_neg <- as.data.frame(t(original_neg))
shuffled_neg_source_target <- rbind(shuffled_neg_source_target, original_neg)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled negative-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_neg_source_target))))
if(nrow(distinct(shuffled_neg_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_source_target <- as.data.frame(t(shuffled_neg_source_target))

# remove real time series from shuffled dataframe
shuffled_neg_source_target <- shuffled_neg_source_target[c(1:1000)]

### target only ###

## count of events ##

# create empty data frame
shuffled_all_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled <- sample(ICEWS_df_formatted$all_deciles_target, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled))
  shuffled_all_target <- rbind(shuffled_all_target, sample)
}

# take the original time series and add it as a row
original_all <- as.data.frame(ICEWS_df_formatted$all_deciles_target)
original_all <- as.data.frame(t(original_all))
shuffled_all_target <- rbind(shuffled_all_target, original_all)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled all-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_all_target))))
if(nrow(distinct(shuffled_all_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_target <- as.data.frame(t(shuffled_all_target))

# remove real time series from shuffled dataframe
shuffled_all_target <- shuffled_all_target[c(1:1000)]

## count of positive events ##

# create empty data frame
shuffled_pos_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled <- sample(ICEWS_df_formatted$pos_deciles_target, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled))
  shuffled_pos_target <- rbind(shuffled_pos_target, sample)
}

# take the original time series and add it as a row
original_pos <- as.data.frame(ICEWS_df_formatted$pos_deciles_source_target)
original_pos <- as.data.frame(t(original_pos))
shuffled_pos_target <- rbind(shuffled_pos_target, original_pos)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled positive-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_pos_target))))
if(nrow(distinct(shuffled_pos_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_target <- as.data.frame(t(shuffled_pos_target))

# remove real time series from shuffled dataframe
shuffled_pos_target <- shuffled_pos_target[c(1:1000)]

## count of negative events ##

# create empty data frame
shuffled_neg_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled <- sample(ICEWS_df_formatted$neg_deciles_target, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled))
  shuffled_neg_target <- rbind(shuffled_neg_target, sample)
}

# take the original time series and add it as a row
original_neg <- as.data.frame(ICEWS_df_formatted$neg_deciles_target)
original_neg <- as.data.frame(t(original_neg))
shuffled_neg_target <- rbind(shuffled_neg_target, original_neg)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled negative-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_neg_target))))
if(nrow(distinct(shuffled_neg_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_target <- as.data.frame(t(shuffled_neg_target))

# remove real time series from shuffled dataframe
shuffled_neg_target <- shuffled_neg_target[c(1:1000)]

### save to file ###

# bind shuffled data together to save as one file
shuffled_full <- cbind(shuffled_coh, 
                       shuffled_all_source_target, 
                       shuffled_pos_source_target, 
                       shuffled_neg_source_target, 
                       shuffled_all_target, 
                       shuffled_pos_target, 
                       shuffled_neg_target)

# write shuffled data to file
write.table(x = shuffled_full,
            file='./data/formatted/shuffled_data_full.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 5. Create randomized time series for permutation testing for windowed CRQA ####

## social cohesion ##

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  coh_shuffled <- sample(ICEWS_df_formatted$coh_deciles, size = 14, replace = FALSE)
  sample <- t(as.data.frame(coh_shuffled))
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for windowed CRQA: ",
             nrow(distinct(shuffled_coh))))
if(nrow(distinct(shuffled_coh)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_coh <- as.data.frame(t(shuffled_coh))

### source and target ###

## count of all events ##

# create empty data frame
shuffled_all_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled <- sample(ICEWS_df_formatted$all_deciles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled))
  shuffled_all_source_target <- rbind(shuffled_all_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled all-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_all_source_target))))
if(nrow(distinct(shuffled_all_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_source_target <- as.data.frame(t(shuffled_all_source_target))

## count of positive events ##

# create empty data frame
shuffled_pos_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled <- sample(ICEWS_df_formatted$pos_deciles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled))
  shuffled_pos_source_target <- rbind(shuffled_pos_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled positive-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_pos_source_target))))
if(nrow(distinct(shuffled_pos_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_source_target <- as.data.frame(t(shuffled_pos_source_target))

## count of negative events ##

# create empty data frame
shuffled_neg_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled <- sample(ICEWS_df_formatted$neg_deciles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled))
  shuffled_neg_source_target <- rbind(shuffled_neg_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled negative-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_neg_source_target))))
if(nrow(distinct(shuffled_neg_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_source_target <- as.data.frame(t(shuffled_neg_source_target))

### target only ###

## count of all events ##

# create empty data frame
shuffled_all_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled <- sample(ICEWS_df_formatted$all_deciles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled))
  shuffled_all_target <- rbind(shuffled_all_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled all-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_all_target))))
if(nrow(distinct(shuffled_all_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_target <- as.data.frame(t(shuffled_all_target))

## count of positive events ##

# create empty data frame
shuffled_pos_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled <- sample(ICEWS_df_formatted$pos_deciles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled))
  shuffled_pos_target <- rbind(shuffled_pos_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled positive-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_pos_target))))
if(nrow(distinct(shuffled_pos_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_target <- as.data.frame(t(shuffled_pos_target))

## count of negative events ##

# create empty data frame
shuffled_neg_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled <- sample(ICEWS_df_formatted$neg_deciles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled))
  shuffled_neg_target <- rbind(shuffled_neg_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled negative-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_neg_target))))
if(nrow(distinct(shuffled_neg_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_target <- as.data.frame(t(shuffled_neg_target))

### save to file ###

# bind shuffled data together to save as one file
shuffled_windowed <- cbind(shuffled_coh, 
                           shuffled_all_source_target, 
                           shuffled_pos_source_target, 
                           shuffled_neg_source_target, 
                           shuffled_all_target, 
                           shuffled_pos_target, 
                           shuffled_neg_target)

# write shuffled data to file
write.table(x = shuffled_windowed,
            file='./data/formatted/shuffled_data_windowed.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
