# data aggregation script

# NOTES:
## The experiment 1 program files for rel_xfer outputs a .csv file for each participant's data from each phase (relx & trancl)
## This script aggregates participant data into a single file for each phase (base category learning, study, recognition test, and target category learning)
## Execute this script prior to any analysis scripts
## Create a "data" directory under the "analysis" directory to save aggregate data to
## Assumes a naming convention of "PC-experiment-condition-pnum.csv". If local machine name includes a dash, all file name indexing will need to be adjusted

# load libraries

# set directories
home_dir <- getwd()
setwd(".."); parent_dir <- getwd()
subj_data_dir <- paste0(parent_dir, "/subjects")
data_out_dir <- paste0(home_dir, "/data")

# get list of participant files
setwd(subj_data_dir)
subj_list <- list.files()
subj_list <- subj_list[subj_list != 'desktop.ini' & subj_list != ".DS_Store"]

# check for duplicate participant ids
## experiment program appends "_dupe" to subsequent occurances of any pnumbers (from same pc)
dupes <- subj_list[grepl("_dupe", subj_list)]
if(length(dupes > 0)){
	stop(paste0("duplicates identified: ", dupes))
}

# identify participants that need to be dropped 
drop <- c(8061,9056) # participants that didn't finish

# seed data frames for appending subject data
trainClass <- xferClass <- data.frame(id = numeric(), condition = numeric(), phase = character(),
	domain = character(), block = numeric(), trial = numeric(), item = character(),
	category = character(), response = character(), rt = numeric(), accuracy = numeric(), experiment = character())

memStudy <- data.frame(id = numeric(), condition = numeric(), phase = character(),
	domain = character(), trial = numeric(), item = character(), category = character(),
	rt = numeric(), experiment = character())

memTest <- data.frame(id = numeric(), condition = numeric(), phase = character(),
	domain = character(), trial = numeric(), item = character(), category = character(),
	inStudy = character(), response = character(), rt = numeric(), accuracy = numeric(),experiment = character())

# iterate through subjects and read in data
for(subject in 1:length(subj_list)){
	if(!(unlist(strsplit(subj_list[subject],'-'))[4] %in% drop)){

		if(unlist(strsplit(subj_list[subject],'-'))[2] == "relx"){
			if(unlist(strsplit(subj_list[subject],'-'))[3] != '3' & unlist(strsplit(subj_list[subject],'-'))[3] != '6'){
				# Train Classification data
				tempClass <- read.csv(subj_list[subject], header = FALSE,
					skip = 3, nrows = 90, stringsAsFactors = FALSE)
				tempClass[,ncol(tempClass)] <-  rep('relx', nrow(tempClass))
				names(tempClass) <- c('id', 'condition', 'phase', 'domain', 'block' , 'trial' , 'item',
					'category', 'response', 'rt', 'accuracy','experiment')
				trainClass <- rbind(trainClass, tempClass)
				
				# Memory - Study data
				tempStudy <- read.csv(subj_list[subject], header = FALSE,
					skip = 93, nrows = 8, stringsAsFactors = FALSE)
				tempStudy[,ncol(tempStudy)] <-  rep('relx', nrow(tempStudy))
				names(tempStudy) <- c('id', 'condition', 'phase', 'domain',
					'trial', 'item', 'category', 'rt', 'experiment')
				memStudy <- rbind(memStudy, tempStudy)

				# memory test data
				tempTest <- read.csv(subj_list[subject], header = FALSE,
					skip = 102, nrows = 18, stringsAsFactors = FALSE)
				tempTest[,ncol(tempTest)] <- rep('relx', nrow(tempTest))
				names(tempTest) <- c('id', 'condition', 'phase', 'domain', 
					'trial', 'item', 'category', 'inStudy', 'response',
					'rt', 'accuracy', 'experiment')
				memTest <- rbind(memTest, tempTest)
			}else{
				# Memory - Study data
				tempStudy <- read.csv(subj_list[subject], header = FALSE,
					skip = 3, nrows = 8, stringsAsFactors = FALSE)
				tempStudy[,ncol(tempStudy)] <-  rep('relx', nrow(tempStudy))
				names(tempStudy) <- c('id', 'condition', 'phase', 'domain',
					'trial', 'item', 'category', 'rt', 'experiment')
				memStudy <- rbind(memStudy, tempStudy)

				# memory test data
				tempTest <- read.csv(subj_list[subject], header = FALSE,
					skip = 12, nrows = 18, stringsAsFactors = FALSE)
				tempTest[,ncol(tempTest)] <-  rep('relx', nrow(tempTest))
				names(tempTest) <- c('id', 'condition', 'phase', 'domain', 
					'trial', 'item', 'category', 'inStudy', 'response',
					'rt', 'accuracy','experiment')
				memTest <- rbind(memTest, tempTest)
			}

		}else if(unlist(strsplit(subj_list[subject],'-'))[2] == "trancl"){
			# transfer classification
			tempXfer <- read.csv(subj_list[subject], header = FALSE,
					skip = 3, stringsAsFactors = FALSE)
			tempXfer[,ncol(tempXfer)] <-  rep('trancl', nrow(tempXfer))
			names(tempXfer) <- c('id', 'condition', 'phase', 'domain', 'block' , 'trial' , 'item',
				'category', 'response', 'rt', 'accuracy','experiment')
			xferClass <- rbind(xferClass, tempXfer)
		}else{
			print('data read error')
		}
	}
}

# combine data from both classification tasks
allClass <- rbind(trainClass, xferClass)

# write datasets to file
setwd(data_out_dir)
write.csv(trainClass, 'relx_train_class.csv', row.names = FALSE)
write.csv(xferClass, 'relx_xfer_class.csv', row.names = FALSE)
write.csv(memStudy, 'relx_study_mem.csv', row.names = FALSE)
write.csv(memTest, 'relx_recog_mem.csv', row.names = FALSE)
write.csv(allClass, 'relx_combined_class.csv', row.names = FALSE)
