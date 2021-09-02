setwd("c:/Users/Sean/Desktop/for github/relx1_cleaned/analysis")

# analysis prep script

# NOTES:
## This script loads libraries and prepares the data for subsequent analyses and visualizations
## Create an "output" directory under the "analysis" directory to save aggregate data to
## Execute analysis scripts either through this script or by manually running the relevant portions of code before each analysis script

# write output of analysis to text files?
write_output <- TRUE

# set directories
home_dir <- getwd()
data_dir <- paste0(home_dir, "/data")
output_dir <- paste0(home_dir, "/output")

# load libraries
libs <- list('plyr', 'tidyr','ggplot2','psycho','lme4', 'lmerTest','ClusterR')
lapply(libs, library, character.only = TRUE)

#define required functions
sem <- function(x){
	return(sd(x) / sqrt(length(x)))
}

# load aggregate data
setwd(data_dir)

## Phase 1 base category learning (BCL) data
train_class <- read.csv('relx_train_class.csv')
## phase 3 target category learning (TCL) data
xfer_class <- read.csv('relx_xfer_class.csv')
## Recognition memory study data
mem_study <- read.csv('relx_study_mem.csv')
## Recognition memory test data
mem_test <- read.csv('relx_recog_mem.csv')
# # Combined phase 1 and phase 3 data for conditions assigned to spont transfer, hint transfer or between subject baseline
all_class <- read.csv('relx_combined_class.csv')
	all_class[all_class$id == 2029,]$id <- rep(9029, 90) # correcting participant id typo from RA

setwd(home_dir)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# create condition labels
# # initialize list for participants undergoing spontaneous or hint-aided transfer
spontSubs <- c()
hintSubs <- c()
# # find subjects assigned to transfer or control
all_class$task <- rep(NA, nrow(all_class))
for(id in 1:length(unique(all_class$id))){
	# identify participants assigned to baseline
	if(nrow(all_class[all_class$id == unique(all_class$id)[id],]) == 90){
		all_class[all_class$id == unique(all_class$id)[id],]$task <- "Baseline"
	# identify participants in the category learning condition
	}else if(nrow(all_class[all_class$id == unique(all_class$id)[id],]) == 180){
		# identify within subject baseline trials (BCL)
		all_class[all_class$id == unique(all_class$id)[id] & all_class$experiment == 'relx',]$task <-'within'
		# identify spontaneous transfer trials
		if(unique(all_class[all_class$id == unique(all_class$id)[id] &  all_class$experiment == 'trancl',]$condition) == 1 |
		   unique(all_class[all_class$id == unique(all_class$id)[id] &  all_class$experiment == 'trancl',]$condition) == 3 ){
			all_class[all_class$id == unique(all_class$id)[id] & all_class$experiment == 'trancl',]$task <- 'Spontaneous'
			spontSubs <- c(unique(all_class$id)[id], spontSubs)
		}else{
			# hint-aided transfer trials
			all_class[all_class$id == unique(all_class$id)[id] & all_class$experiment == 'trancl',]$task <- 'Hint-aided'
			hintSubs <- c(unique(all_class$id)[id], hintSubs)
		}
	}else{
		all_class[all_class$id == unique(all_class$id)[id],]$task <- 'Error'
	}
} 

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for initial learning analyses (BCL for category learning conditions, TCL for Baseline)
init_class_dat <- subset(all_class, experiment == 'relx')
init_class_dat$baseline <- 'Within-Subject'
init_class_dat$conditionRC <- ifelse(init_class_dat$id %in% spontSubs, "Spontaneous",
	ifelse(init_class_dat$id %in% hintSubs, "Hint-aided","ERROR"))
second_class_dat <- subset(all_class, task == 'Baseline')
second_class_dat$baseline <- 'Between-Subject'
second_class_dat$conditionRC <- 'Baseline'

class_dat <- rbind(init_class_dat, second_class_dat)

# execute analysis and viz script for initial learning
source("initial_learning_analysis.r")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for recognition memory analyses

## regression analyses of hits and false alarms
### extrapolate correct recognition of old items from responses
mem_test$hit <- ifelse(mem_test$inStudy == 'Old' & mem_test$response == 'Yes', 1,0) 
### extrapolate missed recognition of old items from responses
mem_test$miss <- ifelse(mem_test$inStudy == 'Old' & mem_test$response == 'No', 1,0)
### extrapolate false alarms to new items from responses
mem_test$FA <- ifelse(mem_test$inStudy == 'New' & mem_test$response == 'Yes', 1,0)
### extrapolate correct rejection of new items from responses
mem_test$CR <- ifelse(mem_test$inStudy == 'New' & mem_test$response == 'No', 1,0)
### get condition info
mem_test$priorPhase <- ifelse(mem_test$condition != 3 & mem_test$condition != 6, 'Category Learning','No Learning')
mem_test$domain <- as.factor(mem_test$domain)

## sensitivity and bias
### create subject-level data
mem_perf <- ddply(mem_test, .(id, priorPhase, domain), summarize,
	accuracy = mean(accuracy), freq.hit = sum(hit),freq.miss = sum(miss),
	freq.FA = sum(FA), freq.CR = sum(CR), N = length(unique(id)))
mem_perf$priorPhase <- as.factor(mem_perf$priorPhase)
### calculate dprime and bias for each subject
sdtResult <- dprime(n_hit = mem_perf$freq.hit, n_fa= mem_perf$freq.FA, n_miss = mem_perf$freq.miss, 
  n_cr = mem_perf$freq.CR, n_targets = mem_perf$freq.hit + mem_perf$freq.miss,
  n_distractors = mem_perf$freq.FA + mem_perf$freq.CR, adjusted = TRUE)
### append dprime and bias to subject-level data
mem_perf$dprime <- sdtResult$dprime
mem_perf$bias <- sdtResult$beta

## break down into separate datasets of old and new recognition items for analyses
old_items <- subset(mem_test, inStudy == 'Old')
new_items <- subset(mem_test, inStudy == 'New')

# execute analysis and viz script for recognition phase
source("recognition_memory_analysis.r")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for transfer (TCL phase data) analyses
xfer_dat <- subset(all_class, task != 'within')
xfer_dat$task <- as.factor(xfer_dat$task)

# execute analysis and viz script for transfer phase
source("transfer_analysis.r")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for response time (RT) analyses

## assess normality of RT data
# plot(density(xfer_dat$rt)) # data has positive skew

## log transform rt data to address skew
xfer_dat_rt <- xfer_dat
xfer_dat_rt$logRT <- log(xfer_dat_rt$rt)

## remove outliers +/- 3 standard deviations above mean
xfer_dat_rt <- subset(xfer_dat_rt, logRT < (mean(logRT) + (3*sd(logRT))) & logRT > (mean(logRT) - (3*sd(logRT)))) 
# ((nrow(xfer_dat) -nrow(xfer_dat_rt)) / nrow(xfer_dat)) * 100 # 1.44 % of trials dropped

# execute analysis and viz script for response times
source("response_time_analysis.r")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for analyzing performance across phases

## relationship between initial learning and recognition memory
final_bcl_perf <- subset(ddply(init_class_dat, .(id, block), summarize, meanAcc = mean(accuracy)), block == 5)
mem_bcl <- merge(x = subset(mem_test, priorPhase == "Category Learning"), y = final_bcl_perf[,-2], by = "id", all.x = TRUE, all.y = FALSE)
### create datasets for predicting recognition memory performance
oldXbcl <- subset(memLearn, inStudy == "Old")
newXbcl <- subset(memLearn, inStudy == "New")

## relationship between initial learning and transfer
xfer_supp_dat <- merge(x = xfer_dat, y = final_bcl_perf[,-2], by = "id", all.x = TRUE, all.y = FALSE)
names(xfer_supp_dat)[ncol(xfer_supp_dat)] <- "finalBCLacc"
tclXbcl_spont <- subset(xfer_supp_dat, task == 'Spontaneous')
tclXbcl_hint <- subset(xfer_supp_dat, task == 'Hint-aided')

## relationship between recognition memory and transfer
mem_subj_perf <- ddply(mem_test, .(id, priorPhase), summarize, propHit = mean(hit)*2, propFA = mean(FA)*2)
xfer_supp_dat <- merge(x = xfer_supp_dat, y = mem_subj_perf[,-2], by = "id", all.x = TRUE, all.y = FALSE)
tclXmem_spont <- subset(xfer_supp_dat, task == 'Spontaneous')
tclXmem_hint <-  subset(xfer_supp_dat, task == 'Hint-aided')
tclXmem_base <-subset(xfer_supp_dat, task == 'Baseline')

# execute analysis and viz script for analyzing performance across phases
source("cross_phase_analysis.r")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for exploratory clustering to construct learner profiles

## get mean classification accuracy for each block of training for all participants
block_means <- ddply(xfer_dat, .(id, block,task), summarize, meanAcc = mean(accuracy))

## reshape into subject by block matrices for clustering
spont_block_mean <- cbind(
	subset(block_means,task == "Spontaneous" & block == 1)[4],
	subset(block_means,task == "Spontaneous" & block == 2)[4],
	subset(block_means,task == "Spontaneous" & block == 3)[4],
	subset(block_means,task == "Spontaneous" & block == 4)[4],
	subset(block_means,task == "Spontaneous" & block == 5)[4])
names(spont_block_mean) <- c("block 1", "block 2","block 3","block 4","block 5")

hint_block_mean <- cbind(
	subset(block_means,task == "Hint-aided" & block == 1)[4],
	subset(block_means,task == "Hint-aided" & block == 2)[4],
	subset(block_means,task == "Hint-aided" & block == 3)[4],
	subset(block_means,task == "Hint-aided" & block == 4)[4],
	subset(block_means,task == "Hint-aided" & block == 5)[4])
names(hint_block_mean) <- c("block 1", "block 2","block 3","block 4","block 5")

base_block_mean <- cbind(
	subset(block_means,task == "Baseline" & block == 1)[4],
	subset(block_means,task == "Baseline" & block == 2)[4],
	subset(block_means,task == "Baseline" & block == 3)[4],
	subset(block_means,task == "Baseline" & block == 4)[4],
	subset(block_means,task == "Baseline" & block == 5)[4])
names(base_block_mean) <- c("block 1", "block 2","block 3","block 4","block 5")

# execute clustering and viz script for analyzing performance across phases
source("cluster_profiles.r")
