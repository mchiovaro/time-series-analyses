######## Cross-Recurrence Quantification Analysis ########
#
# This script runs cross-recurrence quantification analysis
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro) and A. Paxton (@a-paxton)
# University of Connecticut
# Last updated: 2021_02_01

#### 1. Set up ####

library(crqa)

# specify global plotting variables
all_event_color = "#CC79A7"
positive_event_color = "#0072B2"
negative_event_color = "#D55E00"

# read in data 
data <- read.csv("./data/formatted/formatted_data.csv")
shuffled_full <- read.csv("./data/formatted/shuffled_data_full.csv")

# split shuffled data into their own dataframes
shuffled_coh <- shuffled_full[,c(1:1000)]
shuffled_all_target <- shuffled_full[,c(4001:5000)]

#### 2. Run CRQA ####

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_deciles,
                                 ts2=data$all_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_all_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_all_target,
            './results/crqa/target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/crqa/target-rp-all_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of all events", 
           cols = all_event_color, 
           unit  = 10,
           pcex = .5,
           pch = 16,
           las = 0,
           labax = seq(0, nrow(cross_recurrence_analysis$RP)),
           labay = seq(0, nrow(cross_recurrence_analysis$RP)))
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

#### 3. Conduct permutation tests ####

# set seed for reproducibility
set.seed(123)

# initialize data frame for saving metrics
rqa_shuffled_all_target = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
                                   ts2=shuffled_all_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_all_target <- rbind(rqa_shuffled_all_target, rqa_results)
  
}

# initialize data frame for saving significance results
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_all_target[,c(i)] > rqa_results_all_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_target,'./results/crqa/target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

#### 4. Diagonal recurrence profiles ###

# specify window size
win_size = 15

# build the DRP
drp = drpfromts(ts1=data$coh_deciles,
                 ts2=data$all_deciles_target,
                 datatype="categorical",
                 windowsize = win_size,
                 delay=0,
                 embed=1,
                 rescale=0,
                 radius=.001,
                 normalize=0,
                 mindiagline=2,
                 minvertline=2,
                 tw=0)

# plot DRP
qplot(y = drp$profile, 
      x = -win_size:win_size, 
      geom="line") +
  geom_line(color="red",size=1) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("% Recurrence") + xlab("Lag (in days)") +
  ggtitle("Diagonal recurrence profile")
ggsave("./results/crqa/drp.png")
