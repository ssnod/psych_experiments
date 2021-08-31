#  cat_building - Experiment 3b


# write output of analysis to text file?
write.output <- TRUE

# load required packages
libs <- c("plyr", "binom", "lme4", "lmerTest", "tidyr","ggplot2")
lapply(libs, library, character.only = TRUE)

# define standard error function
sem <- function(var){
	return(sd(var) / sqrt(length(var[!is.na(var)])))
}

# turn off scientific notation
options(scipen = 999)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Read in data

# read in data from spontaneous reminding assessment
data.reminding <- read.csv('Experiment 3b reminding data.csv')

# read in data from study tasks
data.study <- read.csv('Experiment 3b study task data.csv')
names(data.study)[1] <- 'pnum'
# # subset only data necessary for present analyses and descriptives
# # # create long form data for encoding analyses + descriptives
data.study.lf <- gather(data.study[,c(1:3,27,52)], key = 'stim', value = 'final.task.score',-c('pnum','condition','encoding.count.bal')) 
data.study.lf$principle <- ifelse(data.study.lf$stim == "paas.description.total", "Problem-as-a-solution","Convergence")
data.study.lf$principle <- as.factor(data.study.lf$principle)

data.study.pass <- data.study[,c(1,2,3,54)]
names(data.study.pass)[4] <- 'avgPerf'
data.study.pass$principle <- 'Problem-as-a-solution'
data.study.conv <- data.study[,c(1,2,3,55)]
names(data.study.conv)[4] <- 'avgPerf'
data.study.conv$principle <- 'Convergence'
data.study.lf2 <- rbind(data.study.pass,data.study.conv)
data.study.lf2$principle <- as.factor(data.study.lf2$principle)

# merge reminding data and encoding data 
data.combined <- merge(x = data.reminding, y= data.study.lf[,-c(2:4)], 
	by= c('pnum', 'principle'), all.x = TRUE, all.y = FALSE)

study.cases <- data.frame(id = rep(data.study[,1],8),
	condition = rep(data.study[,2], 8), 
	count.bal = rep(data.study[,3], 8),
	principle = c(rep('Problem-as-a-solution',  nrow(data.study)*4),rep('Convergence',  nrow(data.study)*4)),
	case = c(rep("avalanche", nrow(data.study)),rep("wildfire", nrow(data.study)),rep("earthquake", nrow(data.study)),rep("solar", nrow(data.study)), 
		rep("army", nrow(data.study)),rep("tank", nrow(data.study)),rep("wood", nrow(data.study)),rep("oil", nrow(data.study))),
	explanation = c(data.study[,7], data.study[,12], data.study[,17], data.study[,22], data.study[,32], data.study[,37], data.study[,42], data.study[,47]))

study.cases$position <- ifelse((study.cases$count.bal == 'a' & study.cases$case == "wildfire")|(study.cases$count.bal == 'a' & study.cases$case == "tank"), 1,	ifelse((study.cases$count.bal == 'a' & study.cases$case == "avalanche")|(study.cases$count.bal == 'a' & study.cases$case == "army"), 2, ifelse((study.cases$count.bal == 'a' & study.cases$case == "earthquake")|(study.cases$count.bal == 'a' & study.cases$case == "oil"), 3, ifelse((study.cases$count.bal == 'a' & study.cases$case == "solar")|(study.cases$count.bal == 'a' & study.cases$case == "wood"), 4,
	ifelse((study.cases$count.bal == 'b' & study.cases$case == "avalanche")|(study.cases$count.bal == 'b' & study.cases$case == "army"), 1,	ifelse((study.cases$count.bal == 'b' & study.cases$case == "earthquake")|(study.cases$count.bal == 'b' & study.cases$case == "oil"), 2, ifelse((study.cases$count.bal == 'b' & study.cases$case == "solar")|(study.cases$count.bal == 'b' & study.cases$case == "wood"), 3, ifelse((study.cases$count.bal == 'b' & study.cases$case == "wildfire")|(study.cases$count.bal == 'b' & study.cases$case == "tank"), 4,
	ifelse((study.cases$count.bal == 'c' & study.cases$case == "earthquake")|(study.cases$count.bal == 'c' & study.cases$case == "oil"), 1, ifelse((study.cases$count.bal == 'c' & study.cases$case == "solar")|(study.cases$count.bal == 'c' & study.cases$case == "wood"), 2, ifelse((study.cases$count.bal == 'c' & study.cases$case == "wildfire")|(study.cases$count.bal == 'c' & study.cases$case == "tank"), 3, ifelse((study.cases$count.bal == 'c' & study.cases$case == "avalanche")|(study.cases$count.bal == 'c' & study.cases$case == "army"), 4,
	ifelse((study.cases$count.bal == 'd' & study.cases$case == "solar")|(study.cases$count.bal == 'd' & study.cases$case == "wood"), 1,	ifelse((study.cases$count.bal == 'd' & study.cases$case == "wildfire")|(study.cases$count.bal == 'd' & study.cases$case == "tank"), 2, ifelse((study.cases$count.bal == 'd' & study.cases$case == "avalanche")|(study.cases$count.bal == 'd' & study.cases$case == "army"), 3, ifelse((study.cases$count.bal == 'd' & study.cases$case == "earthquake")|(study.cases$count.bal == 'd' & study.cases$case == "oil"), 4,
	ifelse((study.cases$count.bal == 'e' & study.cases$case == "wildfire")|(study.cases$count.bal == 'e' & study.cases$case == "tank"), 1,	ifelse((study.cases$count.bal == 'e' & study.cases$case == "earthquake")|(study.cases$count.bal == 'e' & study.cases$case == "oil"), 2, ifelse((study.cases$count.bal == 'e' & study.cases$case == "solar")|(study.cases$count.bal == 'e' & study.cases$case == "wood"), 3, ifelse((study.cases$count.bal == 'e' & study.cases$case == "avalanche")|(study.cases$count.bal == 'e' & study.cases$case == "army"), 4,
	ifelse((study.cases$count.bal == 'f' & study.cases$case == "solar")|(study.cases$count.bal == 'f' & study.cases$case == "wood"), 1, ifelse((study.cases$count.bal == 'f' & study.cases$case == "wildfire")|(study.cases$count.bal == 'f' & study.cases$case == "tank"), 2, ifelse((study.cases$count.bal == 'f' & study.cases$case == "avalanche")|(study.cases$count.bal == 'f' & study.cases$case == "army"), 3, ifelse((study.cases$count.bal == 'f' & study.cases$case == "earthquake")|(study.cases$count.bal == 'f' & study.cases$case == "oil"), 4,
	ifelse((study.cases$count.bal == 'g' & study.cases$case == "avalanche")|(study.cases$count.bal == 'g' & study.cases$case == "army"), 1, ifelse((study.cases$count.bal == 'g' & study.cases$case == "earthquake")|(study.cases$count.bal == 'g' & study.cases$case == "oil"), 2, ifelse((study.cases$count.bal == 'g' & study.cases$case == "wildfire")|(study.cases$count.bal == 'g' & study.cases$case == "tank"), 3, ifelse((study.cases$count.bal == 'g' & study.cases$case == "solar")|(study.cases$count.bal == 'g' & study.cases$case == "wood"), 4,
	ifelse((study.cases$count.bal == 'i' & study.cases$case == "wildfire")|(study.cases$count.bal == 'i' & study.cases$case == "tank"), 1, ifelse((study.cases$count.bal == 'i' & study.cases$case == "earthquake")|(study.cases$count.bal == 'i' & study.cases$case == "oil"), 2, ifelse((study.cases$count.bal == 'i' & study.cases$case == "solar")|(study.cases$count.bal == 'i' & study.cases$case == "wood"), 3, ifelse((study.cases$count.bal == 'i' & study.cases$case == "avalanche")|(study.cases$count.bal == 'i' & study.cases$case == "army"), 4,
	ifelse((study.cases$count.bal == 'k' & study.cases$case == "avalanche")|(study.cases$count.bal == 'k' & study.cases$case == "army"), 1, ifelse((study.cases$count.bal == 'k' & study.cases$case == "solar")|(study.cases$count.bal == 'k' & study.cases$case == "wood"), 2, ifelse((study.cases$count.bal == 'k' & study.cases$case == "earthquake")|(study.cases$count.bal == 'k' & study.cases$case == "oil"), 3, ifelse((study.cases$count.bal == 'k' & study.cases$case == "wildfire")|(study.cases$count.bal == 'k' & study.cases$case == "tank"), 4, 999))))))))))))))))))))))))))))))))))))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Descriptive Statistics

# # Descriptives for spontaneous remindings to target cues by condition
desc.target.condition <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.target.condition <- cbind(desc.target.condition, 
	binom.logit(desc.target.condition[,3], desc.target.condition[,4], 
		conf.level = 0.95)[,c(5,6)])

desc.target.condition <- cbind(desc.target.condition[,1], rep("Combined",2),desc.target.condition[,-1])
names(desc.target.condition)[c(1,2,6,7)] <- c("condition","principle","lower95ci","upper95ci")

# # Descriptives for spontaneous remindings to target cues by condition
desc.target.principle <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(principle), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.target.principle <- cbind(desc.target.principle, 
	binom.logit(desc.target.principle[,3], desc.target.principle[,4], 
		conf.level = 0.95)[,c(5,6)])

desc.target.principle <- cbind(rep("Combined",2),desc.target.principle)
names(desc.target.principle)[c(1,6,7)] <- c("condition","lower95ci","upper95ci")


# # Descriptives for spontaneous remindings to target cues by condition and principle
desc.target.cond.princ <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition, principle), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.target.cond.princ <- cbind(desc.target.cond.princ, 
	binom.logit(desc.target.cond.princ[,4], desc.target.cond.princ[,5], 
		conf.level = 0.95)[,c(5,6)])
names(desc.target.cond.princ)[6:7] <- c("lower95ci","upper95ci")

# # combine into single data.frame for all target cues
descriptives.targets <- rbind(desc.target.condition, 
	desc.target.principle,desc.target.cond.princ)

# # # Print descriptives
cat('\n');print("REMINDINGS TO TARGETS BY CONDITION AND PRINCIPLE DESCRIPTIVES")
print(descriptives.targets)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.targets, 'E3b reminding to targets by condition and principle descriptives.csv', row.names = FALSE)
}


# # Descriptives for spontaneous remindings to distractors by condition
desc.distract.condition <- ddply(subset(data.reminding, principle == "Distractor"), 
	.(condition), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.distract.condition <- cbind(desc.distract.condition, 
	binom.logit(desc.distract.condition[,3], desc.distract.condition[,4], 
		conf.level = 0.95)[,c(5,6)])

desc.distract.condition <- cbind(desc.distract.condition[1],rep("Combined",2),desc.distract.condition[,-1])
names(desc.distract.condition)[c(2,6,7)] <- c("case","lower95ci","upper95ci")


# # Descriptives for spontaneous remindings to distractors by case
desc.distract.case <- ddply(subset(data.reminding, principle == "Distractor"), 
	.(case), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.distract.case <- cbind(desc.distract.case, 
	binom.logit(desc.distract.case[,3], desc.distract.case[,4], 
		conf.level = 0.95)[,c(5,6)])

desc.distract.case <- cbind(rep('Combined',2), desc.distract.case)
names(desc.distract.case)[c(1,6,7)] <- c("condition","lower95ci","upper95ci")


# # Descriptives for spontaneous remindings to distractors by condition
desc.distract.condcase <- ddply(subset(data.reminding, principle == "Distractor"), 
	.(condition, case), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.distract.condcase <- cbind(desc.distract.condcase, 
	binom.logit(desc.distract.condcase[,4], desc.distract.condcase[,5], 
		conf.level = 0.95)[,c(5,6)])
names(desc.distract.condcase)[6:7] <- c("lower95ci","upper95ci")

descriptives.distractors <- rbind(desc.distract.condition,
	desc.distract.case,desc.distract.condcase)

# # # Print descriptives
cat('\n');print("OVERGENERALIZATION TO DISTRACTORS BY CASE AND DESCRIPTIVES")
print(descriptives.distractors)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.distractors, 'E3b reminding to distractors by case descriptives.csv', row.names = FALSE)
}


# # Descriptives for spontaneous remindings to distractors by distractor position
desc.distract.pos <- ddply(subset(data.reminding, principle == "Distractor"), 
	.(position), summarize, spont.reminding = mean(reminding), 
	frequency = sum(reminding),
	n = length(reminding))

# # # Generate 95% CIs for transfer rate and add to descriptives
desc.distract.pos <- cbind(desc.distract.pos, 
	binom.logit(desc.distract.pos[,3], desc.distract.pos[,4], 
		conf.level = 0.95)[,c(5,6)])

names(desc.distract.pos)[5:6] <- c("lower95ci","upper95ci")

# # # Print descriptives
cat('\n');print("OVERGENERALIZATION TO DISTRACTORS BY POSITION IN ASSESSMENT")
print(desc.distract.pos)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.distract.pos, 'E3b reminding to distractors by position descriptives.csv', row.names = FALSE)
}


# # Descriptives for number of cases listed for target cues by condition
desc.num.cases.cond <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition), summarize, avg.cases = mean(mem.listed), sd.cases = sd(mem.listed), 
	min.cases = min(mem.listed), max.cases = max(mem.listed), n.obs = length(mem.listed))

# # # Print descriptives
cat('\n');print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION")
print(desc.num.cases.cond)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.num.cases.cond, 'E3b number of responses to target cues by condition and reminding success descriptives.csv', row.names = FALSE)
}


# # Descriptives for type of successful remindings to target cues by condition
desc.remind.type <- ddply(subset(data.reminding, principle != "Distractor" & reminding == 1),
	.(condition), summarize, prop.case = mean(cases.listed), n.cases = sum(cases.listed),
	prop.principle = mean(set.listed), n.principle = sum(set.listed), n.obs = length(cases.listed))

desc.remind.type  <- cbind(desc.remind.type, 
	binom.logit(desc.remind.type[,3], desc.remind.type[,6], 
		conf.level = 0.95)[,c(5,6)],
	binom.logit(desc.remind.type[,5], desc.remind.type[,6], 
		conf.level = 0.95)[,c(5,6)])
names(desc.remind.type)[7:10] <- c("lower95ci.case","upper95ci.case", "lower95ci.principle","upper95ci.principle")

# # # Print descriptives
cat('\n');print("TYPE OF RESPONSES FOR SUCCESSFUL REMINDINGS TO TARGET CUES BY CONDITION")
print(desc.remind.type)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.remind.type, 'E2 type of responses for successful remindings descriptives.csv', row.names = FALSE)
}



# # Descriptives for final study task performance by condition
desc.final.study.cond <- ddply(data.study.lf, .(condition), summarize,
	mean.score = mean(final.task.score), sem.score = sem(final.task.score),
	sd.score = sd(final.task.score))

desc.final.study.cond <- cbind(desc.final.study.cond[,1], rep("Combined",2),desc.final.study.cond[,-1])
names(desc.final.study.cond)[1:2] <- c('condition','principle')

# # Descriptives for final study task performance by principle
desc.final.study.princ <- ddply(data.study.lf, .(principle), summarize,
	mean.score = mean(final.task.score), sem.score = sem(final.task.score),
	sd.score = sd(final.task.score))

desc.final.study.princ <- cbind(rep("Combined",2),desc.final.study.princ)
names(desc.final.study.princ)[1] <- 'condition'

# # Descriptives for final study task performance by condition and principle
desc.final.study.cond.princ <- ddply(data.study.lf, .(condition, principle), summarize,
	mean.score = mean(final.task.score), sem.score = sem(final.task.score),
	sd.score = sd(final.task.score))

# # combine into single data.frame for all study task data
descriptives.final.study <- rbind(desc.final.study.cond,
	desc.final.study.princ,desc.final.study.cond.princ)

# # # Print descriptives
cat('\n');print("FINAL STUDY TASK PERFORMANCE")
print(descriptives.final.study)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.final.study, 'E3b final study task performance descriptives.csv', row.names = FALSE)
}

# # Descriptives for study task performance by condition
desc.study.id.cond <- ddply(study.cases, .(id, condition), summarize,
	avg = mean(explanation), sem = sem(explanation),
	sd.score = sd(explanation))

desc.study.cond <- ddply(desc.study.id.cond, .(condition), summarize,
	mean.score = mean(avg), sem.score = sem(avg),
	sd.score = sd(avg))

desc.study.cond <- cbind(desc.study.cond[,1], rep("Combined",2),desc.study.cond[,-1])
names(desc.study.cond)[1:2] <- c('condition','principle')

# # Descriptives for final study task performance by principle
desc.study.id.cond.princ <- ddply(study.cases, .(id,condition, principle), summarize,
	avg = mean(explanation), sem.score = sem(explanation),
	sd.score = sd(explanation))

desc.study.cond.princ <- ddply(desc.study.id.cond.princ, .(condition, principle), summarize,
	mean.score = mean(avg), sem.score = sem(avg),
	sd.score = sd(avg))

# # combine into single data.frame for all study task data
descriptives.study <- rbind(desc.study.cond,
	desc.study.princ,desc.study.cond.princ)

# # # Print descriptives
cat('\n');print("FINAL STUDY TASK PERFORMANCE")
print(descriptives.study)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.study, 'E3b study task performance descriptives.csv', row.names = FALSE)
}


# # Descriptives for study task performance across training
desc.all.study.cond <- ddply(study.cases, .(condition, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.all.study.cond<- cbind(desc.all.study.cond [,1], "Combined",desc.all.study.cond[,-1])
names(desc.all.study.cond)[1:2] <- c('condition','principle')
# # # Category-building by principle
desc.all.study.prince <- ddply(study.cases, .(condition, principle, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
# # combine into single data.frame for all study task data
descriptives.study.position <- rbind(desc.all.study.cond, desc.all.study.prince)

# # # Print descriptives
cat('\n');print("STUDY TASK PERFORMANCE BY POSITION")
print(descriptives.study.position)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.study.position, 'E3b study task performance by positiondescriptives.csv', row.names = FALSE)
}

# # Descriptives for Category-building study performance by case
desc.study.case <- ddply(study.cases, .(case, principle), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.study.case <- desc.study.case[order(desc.study.case[,2]),]

# # # Print descriptives
cat('\n');print("CATEGORY-BUILDING STUDY PERFORMANCE BY CASE")
print(desc.study.case)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.study.case, 'E3b category-building study performance by case descriptives.csv', row.names = FALSE)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inferential Statistics

# # save a copy of stats output as text file
if(write.output == TRUE){
	sink('Experiment 3b Analysis Output.txt', split = TRUE)
} 

# # Sponteous reminding - target cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION RESULTS")
# # # Is there a higher rate of spontaneous remindings in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod1CB <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1CB))

# # Calculate Odds Ratios as effect size for primary analysis
CB.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "Category-building")$reminding)
SU.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "Summarize")$reminding)

# # # Calculate Odds Ratio
CB.SU.OR <- (CB.hits / (1- CB.hits)) / (SU.hits/ (1-SU.hits))

# # # Print results
cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION EFFECTS SIZES")
print(paste('Category-building vs. Summarization Odds Ratio:', CB.SU.OR))
cat('\n')

# # Sponteous reminding - target cues by condition and principle
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION AND PRINCIPLE RESULTS")
# # # Is there a higher rate of spontaneous remindings in Category-building than Summarize?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2CBa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2CBa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2CBb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2CBb))


# # # Do principles differ for Summarize condition?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Summarize")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2SSa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2SSa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2SSb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2SSb))

# # Spontaneous reminding - number of cases listed for target cues
cat('\n');cat('\n'); print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod8CB <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8CB))

# # Spontaneous reminding - number of case-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF CASE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod9CB <- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod9CB))

# # Spontaneous reminding - number of principle-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF PRINCIPLE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of principle-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod10CB <- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod10CB))

# # Sponteous reminding - distractor cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION RESULTS")
# # # Is there a higher rate of overgeneralization in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod3CB <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3CB))


# # Sponteous reminding - distractor cues by condition and cue
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CUE RESULTS")
# # # Is there a higher rate of overgeneralization in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod4CA <- glmer(reminding ~ case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4CA))


# # Final study task - performance by condition
cat('\n');cat('\n'); print("FINAL STUDY TASK PERFORMANCE BY CONDITION RESULTS")
# # # Does Category-building differ in the ability to articulate the target principle by the end of training from Double- and Single-comparison?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Category-building')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6CBa <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6CBa))

data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Category-building')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6CBb <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6CBb))

# # # Does Single-comparison for effect of target principle?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Summarize')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6SU <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6SU))

# # All study task - performance by condition
cat('\n');cat('\n'); print("ALL STUDY TASK PERFORMANCE BY CONDITION RESULTS")
data.study.lf2$condition <- relevel(data.study.lf2$condition, ref = 'Category-building')
data.study.lf2$principle <- relevel(data.study.lf2$principle, ref = 'Problem-as-a-solution')
mod7CBa <- lm(avgPerf ~ condition*principle, data.study.lf2)
print(summary(mod7CBa))

data.study.lf2$condition <- relevel(data.study.lf2$condition, ref = 'Category-building')
data.study.lf2$principle <- relevel(data.study.lf2$principle, ref = 'Convergence')
mod7CBb <- lm(avgPerf ~ condition*principle, data.study.lf2)
print(summary(mod7CBb))

# # # Does Single-comparison for effect of target principle?
data.study.lf2$condition <- relevel(data.study.lf2$condition, ref = 'Summarize')
data.study.lf2$principle <- relevel(data.study.lf2$principle, ref = 'Problem-as-a-solution')
mod7SU <- lm(avgPerf ~ condition*principle, data.study.lf2)
print(summary(mod7SU))


# # Study task - performance across task within each condition. Included in coverletter, but not manuscript
# study.cases$condition <- relevel(study.cases$condition, ref = 'Category-building')
# mod11CB <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11CB))

# study.cases$condition <- relevel(study.cases$condition, ref = 'Summarize')
# mod11SU <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11SU))


# # study task - performance by case aggregating over condition
study.cases$case <- relevel(study.cases$case, ref = 'avalanche')
mod12a <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Problem-as-a-solution'& case != "final.paas"))
summary(mod12a)

study.cases$case <- relevel(study.cases$case, ref = 'earthquake')
mod12b <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Problem-as-a-solution' & case != "final.paas"))
summary(mod12b)

study.cases$case <- relevel(study.cases$case, ref = 'solar')
mod12c <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Problem-as-a-solution' & case != "final.paas"))
summary(mod12c)

study.cases$case <- relevel(study.cases$case, ref = 'tank')
mod12d <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Convergence' & case != "final.conv"))
summary(mod12d)

study.cases$case <- relevel(study.cases$case, ref = 'wood')
mod12e <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Convergence' & case != "final.conv"))
summary(mod12e)

study.cases$case <- relevel(study.cases$case, ref = 'army')
mod12f <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Convergence' & case != "final.conv"))
summary(mod12f)

if(write.output == TRUE){
	sink()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Visualizations

# # Spontaneous remindings to target cues by condition and principle 
# # # Get fitted values from regression models
plot1df <- subset(data.reminding, principle != 'Distractor')
plot1df$condition <- as.factor(as.character(plot1df$condition))
plot1df$fitted.cond <- predict(newdata = plot1df, mod1CB, type = 'response')
plot1df$fitted.princ <- predict(newdata = plot1df, mod2CBa, type = 'response')

# # # Add descriptives for fitted values to existing descriptives
plot1df.summary <- subset(descriptives.targets, condition != 'Combined')
plot1df.summary$fitted <- c(
	ddply(plot1df, .(condition), summarize, fitted.reminding = mean(fitted.cond))$fitted.reminding,
	ddply(plot1df, .(condition, principle), summarize, fitted.reminding = mean(fitted.princ))$fitted.reminding
	)

# # # rename vars for plotting
plot1df.summary[,1]<-as.factor(plot1df.summary[,1])
levels(plot1df.summary[,1]) <- c("Category\nBuilding","Summarization","Total")
plot1df.summary[,2]<-as.factor(plot1df.summary[,2])
levels(plot1df.summary[,2]) <- c("Overall","Convergence","Distractor","Problem-as-a-Solution")

# # # create plot and write to disk
remind.plot <- ggplot(plot1df.summary,aes(x=condition, fill = condition, y = spont.reminding)) + 
	geom_bar(stat = 'identity', col = 'black',
		fill = rep(c('grey85','grey25'),3))+ 
	facet_grid(~principle) +
	geom_errorbar(aes(ymin= lower95ci, ymax= upper95ci),width=.2) + 
	geom_point(data = plot1df.summary, aes(x=condition, color = condition, y = fitted), pch = 18,
		size = 5, color = 'black') +
	labs(y = "Proportion Structure-Based Reminding \n", x = "\n Study Task") +
	coord_cartesian(ylim=c(0, 1)) + 
    scale_y_continuous(breaks=seq(0, 1, 0.1),expand = c(0,0)) +
    theme_bw() + 
    theme(axis.text.x = element_text(face = "plain", color = "black", size = 12),
	    axis.title.x = element_text(face="bold", size=14),
       	axis.text.y = element_text(face = "plain", color = "black", size = 12),
	    axis.title.y = element_text(face="bold", size=16),
	    strip.text = element_text(face = "bold",size=14),
	    panel.grid.major.x = element_blank() , 
	    panel.grid.major.y = element_line(color = "grey"),
	    panel.grid.minor.y = element_line(color = "grey"),
	    axis.line = element_line(colour = "black"),
	    legend.position = "none")
ggsave('E3b remindings to target cues.png',remind.plot, width = 10.8, height = 6, units = 'in')

# # # Overgeneralization to distractors by condition and cue. Not included in manuscript.
# # # # Get fitted values from regression models
# plot2df <- subset(data.reminding, principle == 'Distractor')
# plot2df$condition <- as.factor(as.character(plot2df$condition))
# plot2df$fitted.cond <- predict(newdata = plot2df, mod4CB, type = 'response')
# plot2df$fitted.case <- predict(newdata = plot2df, mod5CB, type = 'response')

# # # # Add descriptives for fitted values to existing descriptives
# plot2df.summary <- subset(descriptives.distractors, condition != 'Combined')
# plot2df.summary$fitted <- c(
# 	ddply(plot2df, .(condition), summarize, fitted.reminding = mean(fitted.cond))$fitted.reminding,
# 	ddply(plot2df, .(condition, case), summarize, fitted.reminding = mean(fitted.case))$fitted.reminding
# 	)

# # # # rename vars for plotting
# plot2df.summary[,1]<-as.factor(plot2df.summary[,1])
# levels(plot2df.summary[,1]) <- c("Category\nBuilding","Double\nComparison","Single\nComparison","Total")
# plot2df.summary[,2]<-as.factor(plot2df.summary[,2])
# levels(plot2df.summary[,2]) <- c("Overall","aquarium","blackmarket","internetsec","tumor","The Birthday Party","The Wine Merchant")

# distract.plot <- ggplot(plot2df.summary,aes(x=condition, fill = case, y = spont.reminding)) + 
# 	geom_bar(stat = 'identity', col = 'black',
# 		position = position_dodge())+ 
# 	geom_errorbar(aes(ymin= lower95ci, ymax= upper95ci),width=.2, position = position_dodge(.9)) + 
# 	geom_point(data = plot2df.summary, aes(x=condition, fill = case, y = fitted), pch = 18,
# 		size = 5, color = 'black', position = position_dodge(.9)) +
# 	labs(y = "Proportion Overgeneralized \n", x = "\n Study Task", fill="Distractor") +
# 	coord_cartesian(ylim=c(0, 1)) + 
#     scale_y_continuous(breaks=seq(0, 1, 0.1),expand = c(0,0)) +
#     scale_fill_manual(values = c('grey85','grey50', 'grey25')) + 
#     theme_bw() + 
#     theme(axis.text.x = element_text(face = "plain", color = "black", size = 12),
# 	    axis.title.x = element_text(face="bold", size=14),
#        	axis.text.y = element_text(face = "plain", color = "black", size = 12),
# 	    axis.title.y = element_text(face="bold", size=16),
# 	    legend.text = element_text(size = 12),
# 	    legend.title = element_text(size = 14),
# 	    panel.grid.major.x = element_blank() , 
# 	    panel.grid.major.y = element_line(color = "grey"),
# 	    panel.grid.minor.y = element_line(color = "grey"),
# 	    axis.line = element_line(colour = "black"),
# 	    legend.position="bottom")
# ggsave('E3b remindings to distractor cues.png',distract.plot, width = 7, height = 7, units = 'in')
