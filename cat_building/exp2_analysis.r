#  cat_building - Experiment 2	


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
data.reminding <- read.csv('Experiment 2 reminding data.csv')

# read in data from study tasks
data.study <- read.csv('Experiment 2 study task data.csv')

# # subset only data necessary for present analyses and descriptives
data.study.lf <- data.study[,c(1:3,78,79)]
# # # create long form data for encoding analyses + descriptives
data.study.lf <- gather(data.study.lf, key = 'stim', value = 'final.task.score',-c('pnum','condition','encoding.count.bal')) 
data.study.lf$principle <- ifelse(data.study.lf$stim == "final.task.paas", "Problem-as-a-solution","Convergence")
data.study.lf$principle <- as.factor(data.study.lf$principle)

# merge reminding data and encoding data 
data.combined <- merge(x = data.reminding, y= data.study.lf[,-c(2:4)], 
	by= c('pnum', 'principle'), all.x = TRUE, all.y = FALSE)

# Category-building encoding data
CB.encode <- data.frame(id = rep(subset(data.study, condition == "Category-building")[,1],10),
	condition = rep(subset(data.study, condition == "Category-building")[,2], 10), 
	count.bal = rep(subset(data.study, condition == "Category-building")[,3], 10),
	principle = c(rep('Problem-as-a-solution',  nrow(subset(data.study, condition == "Category-building"))*5),rep('Convergence',  nrow(subset(data.study, condition == "Category-building"))*5)),
	case = c(rep("avalanche", nrow(subset(data.study, condition == "Category-building"))),rep("wildfire", nrow(subset(data.study, condition == "Category-building"))),rep("earthquake", nrow(subset(data.study, condition == "Category-building"))),rep("solar", nrow(subset(data.study, condition == "Category-building"))), rep("final.paas", nrow(subset(data.study, condition == "Category-building"))),
		rep("army", nrow(subset(data.study, condition == "Category-building"))),rep("tank", nrow(subset(data.study, condition == "Category-building"))),rep("wood", nrow(subset(data.study, condition == "Category-building"))),rep("oil", nrow(subset(data.study, condition == "Category-building"))), rep("final.conv", nrow(subset(data.study, condition == "Category-building")))),
	explanation = c(subset(data.study, condition == "Category-building")[,31], subset(data.study, condition == "Category-building")[,36], subset(data.study, condition == "Category-building")[,41], subset(data.study, condition == "Category-building")[,46], subset(data.study, condition == "Category-building")[,51], 
		subset(data.study, condition == "Category-building")[,56], subset(data.study, condition == "Category-building")[,61], subset(data.study, condition == "Category-building")[,66], subset(data.study, condition == "Category-building")[,71], subset(data.study, condition == "Category-building")[,76]))
# # determine position based on counterbalancing order
CB.encode$position <- ifelse(CB.encode$case == "final.paas"|CB.encode$case == "final.conv",5,
	ifelse((CB.encode$count.bal == 'a' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'a' & CB.encode$case == "tank"), 1,	ifelse((CB.encode$count.bal == 'a' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'a' & CB.encode$case == "army"), 2, ifelse((CB.encode$count.bal == 'a' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'a' & CB.encode$case == "oil"), 3, ifelse((CB.encode$count.bal == 'a' & CB.encode$case == "solar")|(CB.encode$count.bal == 'a' & CB.encode$case == "wood"), 4,
	ifelse((CB.encode$count.bal == 'b' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'b' & CB.encode$case == "army"), 1,	ifelse((CB.encode$count.bal == 'b' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'b' & CB.encode$case == "oil"), 2, ifelse((CB.encode$count.bal == 'b' & CB.encode$case == "solar")|(CB.encode$count.bal == 'b' & CB.encode$case == "wood"), 3, ifelse((CB.encode$count.bal == 'b' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'b' & CB.encode$case == "tank"), 4,
	ifelse((CB.encode$count.bal == 'c' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'c' & CB.encode$case == "oil"), 1, ifelse((CB.encode$count.bal == 'c' & CB.encode$case == "solar")|(CB.encode$count.bal == 'c' & CB.encode$case == "wood"), 2, ifelse((CB.encode$count.bal == 'c' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'c' & CB.encode$case == "tank"), 3, ifelse((CB.encode$count.bal == 'c' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'c' & CB.encode$case == "army"), 4,
	ifelse((CB.encode$count.bal == 'd' & CB.encode$case == "solar")|(CB.encode$count.bal == 'd' & CB.encode$case == "wood"), 1,	ifelse((CB.encode$count.bal == 'd' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'd' & CB.encode$case == "tank"), 2, ifelse((CB.encode$count.bal == 'd' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'd' & CB.encode$case == "army"), 3, ifelse((CB.encode$count.bal == 'd' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'd' & CB.encode$case == "oil"), 4,
	ifelse((CB.encode$count.bal == 'f' & CB.encode$case == "solar")|(CB.encode$count.bal == 'f' & CB.encode$case == "wood"), 1, ifelse((CB.encode$count.bal == 'f' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'f' & CB.encode$case == "tank"), 2, ifelse((CB.encode$count.bal == 'f' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'f' & CB.encode$case == "army"), 3, ifelse((CB.encode$count.bal == 'f' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'f' & CB.encode$case == "oil"), 4,
	ifelse((CB.encode$count.bal == 'g' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'g' & CB.encode$case == "army"), 1, ifelse((CB.encode$count.bal == 'g' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'g' & CB.encode$case == "oil"), 2, ifelse((CB.encode$count.bal == 'g' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'g' & CB.encode$case == "tank"), 3, ifelse((CB.encode$count.bal == 'g' & CB.encode$case == "solar")|(CB.encode$count.bal == 'g' & CB.encode$case == "wood"), 4,
	ifelse((CB.encode$count.bal == 'i' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'i' & CB.encode$case == "tank"), 1, ifelse((CB.encode$count.bal == 'i' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'i' & CB.encode$case == "oil"), 2, ifelse((CB.encode$count.bal == 'i' & CB.encode$case == "solar")|(CB.encode$count.bal == 'i' & CB.encode$case == "wood"), 3, ifelse((CB.encode$count.bal == 'i' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'i' & CB.encode$case == "army"), 4,
	ifelse((CB.encode$count.bal == 'k' & CB.encode$case == "avalanche")|(CB.encode$count.bal == 'k' & CB.encode$case == "army"), 1, ifelse((CB.encode$count.bal == 'k' & CB.encode$case == "solar")|(CB.encode$count.bal == 'k' & CB.encode$case == "wood"), 2, ifelse((CB.encode$count.bal == 'k' & CB.encode$case == "earthquake")|(CB.encode$count.bal == 'k' & CB.encode$case == "oil"), 3, ifelse((CB.encode$count.bal == 'k' & CB.encode$case == "wildfire")|(CB.encode$count.bal == 'k' & CB.encode$case == "tank"), 4, 999)))))))))))))))))))))))))))))))))

# Double-comparison encoding data
DC.encode <- data.frame(id = rep(subset(data.study, condition == "Double-comparison")[,1], 4),
	condition = rep(subset(data.study, condition == "Double-comparison")[,2], 4),
	count.bal = rep(subset(data.study, condition == "Double-comparison")[,3], 4),
	position = rep(c(rep('first', nrow(subset(data.study, condition == "Double-comparison"))),rep('second', nrow(subset(data.study, condition == "Double-comparison")))),2),
	principle = c(rep('Problem-as-a-solution',  nrow(subset(data.study, condition == "Double-comparison"))*2),rep('Convergence',  nrow(subset(data.study, condition == "Double-comparison"))*2)),
	explanation = c(subset(data.study, condition == "Double-comparison")[,8], subset(data.study, condition == "Double-comparison")[,14],subset(data.study, condition == "Double-comparison")[,20], subset(data.study, condition == "Double-comparison")[,26]))


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

desc.target.condition <- cbind(desc.target.condition[,1], rep("Combined",3),desc.target.condition[,-1])
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
	write.csv(descriptives.targets, 'E2 reminding to targets by condition and principle descriptives.csv', row.names = FALSE)
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

desc.distract.condition <- cbind(desc.distract.condition[1],rep("Combined",3),desc.distract.condition[,-1])
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
	write.csv(descriptives.distractors, 'E2 reminding to distractors by case descriptives.csv', row.names = FALSE)
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
	write.csv(desc.distract.pos, 'E2 reminding to distractors by position descriptives.csv', row.names = FALSE)
}


# # Descriptives for number of cases listed for target cues by condition
desc.num.cases.cond <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition), summarize, avg.cases = mean(mem.listed), sd.cases = sd(mem.listed), 
	min.cases = min(mem.listed), max.cases = max(mem.listed), n.obs = length(mem.listed))

desc.num.cases.cond  <- cbind(desc.num.cases.cond [,1], rep("Combined",3),desc.num.cases.cond[,-1])
names(desc.num.cases.cond)[c(1,2)] <- c("condition","reminding")
desc.num.cases.cond$reminding <- as.character(desc.num.cases.cond$reminding)

# # Descriptives for number of cases listed for target cues by reminding success
desc.num.cases.remind <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(reminding), summarize, avg.cases = mean(mem.listed), sd.cases = sd(mem.listed), 
	min.cases = min(mem.listed), max.cases = max(mem.listed), n.obs = length(mem.listed))

desc.num.cases.remind <- cbind(rep("Combined",2),desc.num.cases.remind)
names(desc.num.cases.remind)[1] <- "condition"

# # Descriptives for number of cases listed for target cues by condition and reminding success
desc.num.cases.cond.remind <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition), summarize, avg.cases = mean(mem.listed), sd.cases = sd(mem.listed), 
	min.cases = min(mem.listed), max.cases = max(mem.listed), n.obs = length(mem.listed))

# # combine into single data.frame for all target cues
descriptives.num.cases <- rbind(desc.num.cases.cond, 
	desc.num.cases.remind,desc.num.cases.cond.remind)

# # # Print descriptives
cat('\n');print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION")
print(descriptives.num.cases)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.num.cases, 'E2 number of responses to target cues by condition and reminding success descriptives.csv', row.names = FALSE)
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

desc.final.study.cond <- cbind(desc.final.study.cond[,1], rep("Combined",3),desc.final.study.cond[,-1])
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
	write.csv(descriptives.final.study, 'E2 final study task performance descriptives.csv', row.names = FALSE)
}


# # Descriptives for study task performance across training
# # # Category-building
desc.all.study.cond.cb <- ddply(CB.encode, .(condition, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.all.study.cond.cb <- cbind(desc.all.study.cond.cb [,1], "Combined",desc.all.study.cond.cb[,-1])
names(desc.all.study.cond.cb)[1:2] <- c('condition','principle')
# # # Double-comparison
desc.all.study.cond.dc <- ddply(DC.encode, .(condition, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.all.study.cond.dc <- cbind(desc.all.study.cond.dc [,1], "Combined",desc.all.study.cond.dc[,-1])
names(desc.all.study.cond.dc)[1:2] <- c('condition','principle')
# # # Category-building by principle
desc.all.study.prince.cb <- ddply(CB.encode, .(principle, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.all.study.prince.cb <- cbind(rep("Category-building",10),desc.all.study.prince.cb)
names(desc.all.study.prince.cb)[1] <- 'condition'
# # # Double-comparison by principle
desc.all.study.prince.dc <- ddply(DC.encode, .(principle,position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.all.study.prince.dc <- cbind(rep("Double-comparison",4),desc.all.study.prince.dc)
names(desc.all.study.prince.dc)[1] <- 'condition'

# # combine into single data.frame for all study task data
descriptives.study.position <- rbind(desc.all.study.cond.cb, desc.all.study.cond.dc,desc.all.study.prince.cb,desc.all.study.prince.dc)

# # # Print descriptives
cat('\n');print("STUDY TASK PERFORMANCE BY POSITION")
print(descriptives.study.position)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.study.position, 'E2 study task performance by positiondescriptives.csv', row.names = FALSE)
}


# # Descriptives for Category-building study performance by case
desc.study.case <- ddply(CB.encode, .(condition,case, principle), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.study.case <- desc.study.case[order(desc.study.case[,3]),]

# # # Print descriptives
cat('\n');print("CATEGORY-BUILDING STUDY PERFORMANCE BY CASE")
print(desc.study.case)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.study.case, 'E2 category-building study performance by case descriptives.csv', row.names = FALSE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inferential Statistics

# # save a copy of stats output as text file
if(write.output == TRUE){
	sink('Experiment 2 Analysis Output.txt', split = TRUE)
} 

# # Sponteous reminding - target cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION RESULTS")
# # # Is there a higher rate of spontaneous remindings in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod1CB <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1CB))

# # # Is there a higher rate of spontaneous remindings in Double-comparison than Single-comparison?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod1DC <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1DC))

# # # Is there a higher rate of spontaneous remindings in Double-comparison than Single-comparison?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Single-comparison")
mod1SC <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1SC))


# # Calculate Odds Ratios as effect size for primary analysis
CB.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "Category-building")$reminding)
DC.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "Double-comparison")$reminding)
SC.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "Single-comparison")$reminding)

# # # Calculate Odds Ratio
CB.DC.OR <- (CB.hits / (1- CB.hits)) / (DC.hits/ (1-DC.hits))
CB.SC.OR <- (CB.hits / (1- CB.hits)) / (SC.hits/ (1-SC.hits))
DC.SC.OR <- (DC.hits / (1- DC.hits)) / (SC.hits/ (1-SC.hits))

# # # Print results
cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION EFFECTS SIZES")
print(paste('Category-building vs. Double-comparison Odds Ratio:', CB.DC.OR))
print(paste('Category-building vs. Single-comparison Odds Ratio:', CB.SC.OR))
print(paste('Double-comparison vs. Single-comparison Odds Ratio:', DC.SC.OR))
cat('\n')

# # Sponteous reminding - target cues by condition and principle
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION AND PRINCIPLE RESULTS")
# # # Is there a higher rate of spontaneous remindings in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2CBa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2CBa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2CBb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2CBb))


# # # Is there a higher rate of spontaneous remindings in Double-comparison than Single-comparison?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2DCa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2DCa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2DCb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2DCb))

# # # Testing principle differences within single comparison condition
data.reminding$condition <- relevel(data.reminding$condition, ref = "Single-comparison")
mod2SC <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2SC))


# # Spontaneous reminding - number of cases listed for target cues
cat('\n');cat('\n'); print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod8CB <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8CB))

# # # Does Double-comparison differ from Single-comparison in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod8DC <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8DC))


# # Spontaneous reminding - number of case-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF CASE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod9CB <- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod9CB))

# # # Does Double-comparison differ from Single-comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod9DC<- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod9DC))

# # Spontaneous reminding - number of principle-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF PRINCIPLE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of principle-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod10CB <- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod10CB))

# # # Does Double-comparison differ from Single-comparison conditions in the number of principle-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod10DC<- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
print(summary(mod10DC))


# # Sponteous reminding - distractor cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION RESULTS")
# # # Is there a higher rate of overgeneralization in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod3CB <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3CB))

# # # Is there a higher rate of overgeneralization in Double-comparison than Single-comparison?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod3DC <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3DC))


# # Sponteous reminding - distractor cues by condition and cue
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION AND CUE RESULTS")
# # # Is there a higher rate of overgeneralization in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
mod4CB <- glmer(reminding ~ condition + case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4CB))

# # # Is there a higher rate of overgeneralization in Double-comparison than Single-comparison?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod4DC <- glmer(reminding ~ condition + case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4DC))


# # Sponteous reminding - distractor cues by condition and cue
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION, CUE, AND POSITION RESULTS")
# # # Is there a higher rate of overgeneralization in Category-building than comparison conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "Category-building")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod5CB <- glmer(reminding ~ condition + case+as.factor(position) + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod5CB))

data.reminding$condition <- relevel(data.reminding$condition, ref = "Double-comparison")
mod5DC <- glmer(reminding ~ condition + case+as.factor(position) + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod5DC))

data.reminding$condition <- relevel(data.reminding$condition, ref = "Single-comparison")
mod5SC <- glmer(reminding ~ condition + case+as.factor(position) + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod5SC))


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

# # # Does Double-comparison differ in the ability to articulate the target principle from Single-comparison?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Double-comparison')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6DCa <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6DCa))

data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Double-comparison')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6DCb <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6DCb))

# # # Does Single-comparison for effect of target principle?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'Single-comparison')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6SC <- lm(final.task.score ~ condition*principle, data.study.lf)
print(summary(mod6SC))


# # Final study task - performance by condition
cat('\n');cat('\n'); print("DO FINAL STUDY TASK DIFFS PREDICT REMINDING TO TARGET CUES")
# # # Does Category-building lead to improved remindings to target cues over comparison conditions after controlling for variance in final study task scores?
data.combined$condition <- relevel(data.combined$condition, ref = "Category-building")
mod7CB <- glmer(reminding ~ condition + final.task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7CB))

# # # Does Double-comparison lead to improved remindings to target cues over Single-comparison after controlling for variance in final study task scores?
data.combined$condition <- relevel(data.combined$condition, ref = "Double-comparison")
mod7DC <- glmer(reminding ~ condition + final.task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7DC))


# # Study task - performance across task within each condition. Included in coverletter, not most recent manuscript.
# mod11CB <- lmer(explanation ~ position + (1|id), data = CB.encode)
# summary(mod11CB)

# mod11DC<- lmer(explanation ~ position + (1|id), data = DC.encode)
# summary(mod11DC)

# # Study task - Category-building performance by case
CB.encode$case <- relevel(CB.encode$case, ref = 'avalanche')
mod12a <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Problem-as-a-solution' & case != "final.paas"))
summary(mod12a)

CB.encode$case <- relevel(CB.encode$case, ref = 'earthquake')
mod12b <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Problem-as-a-solution' & case != "final.paas"))
summary(mod12b)

CB.encode$case <- relevel(CB.encode$case, ref = 'solar')
mod12c <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Problem-as-a-solution' & case != "final.paas"))
summary(mod12c)

CB.encode$case <- relevel(CB.encode$case, ref = 'tank')
mod12d <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Convergence' & case != "final.conv"))
summary(mod12d)

CB.encode$case <- relevel(CB.encode$case, ref = 'wood')
mod12e <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Convergence' & case != "final.conv"))
summary(mod12e)

CB.encode$case <- relevel(CB.encode$case, ref = 'army')
mod12f <- lmer(explanation ~ case + (1|id), data = subset(CB.encode, principle == 'Convergence' & case != "final.conv"))
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
levels(plot1df.summary[,1]) <- c("Category\nBuilding","Double\nComparison","Single\nComparison","Total")
plot1df.summary[,2]<-as.factor(plot1df.summary[,2])
levels(plot1df.summary[,2]) <- c("Overall","Convergence","Distractor","Problem-as-a-Solution")

# # # create plot and write to dik
remind.plot <- ggplot(plot1df.summary,aes(x=condition, fill = condition, y = spont.reminding)) + 
	geom_bar(stat = 'identity', col = 'black',
		fill = rep(c('grey85','grey55','grey25'),3))+ 
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
ggsave('E2 remindings to target cues.png',remind.plot, width = 10.8, height = 6, units = 'in')

# # Overgeneralization to distractors by condition and cue 
# # # Get fitted values from regression models
plot2df <- subset(data.reminding, principle == 'Distractor')
plot2df$condition <- as.factor(as.character(plot2df$condition))
plot2df$fitted.cond <- predict(newdata = plot2df, mod4CB, type = 'response')
plot2df$fitted.case <- predict(newdata = plot2df, mod5CB, type = 'response')

# # # Add descriptives for fitted values to existing descriptives
plot2df.summary <- subset(descriptives.distractors, condition != 'Combined')
plot2df.summary$fitted <- c(
	ddply(plot2df, .(condition), summarize, fitted.reminding = mean(fitted.cond))$fitted.reminding,
	ddply(plot2df, .(condition, case), summarize, fitted.reminding = mean(fitted.case))$fitted.reminding
	)

# # # rename vars for plotting
plot2df.summary[,1]<-as.factor(plot2df.summary[,1])
levels(plot2df.summary[,1]) <- c("Category\nBuilding","Double\nComparison","Single\nComparison","Total")
plot2df.summary[,2]<-as.factor(plot2df.summary[,2])
levels(plot2df.summary[,2]) <- c("Overall","aquarium","blackmarket","internetsec","tumor","The Birthday Party","The Wine Merchant")

distract.plot <- ggplot(plot2df.summary,aes(x=condition, fill = case, y = spont.reminding)) + 
	geom_bar(stat = 'identity', col = 'black',
		position = position_dodge())+ 
	geom_errorbar(aes(ymin= lower95ci, ymax= upper95ci),width=.2, position = position_dodge(.9)) + 
	geom_point(data = plot2df.summary, aes(x=condition, fill = case, y = fitted), pch = 18,
		size = 5, color = 'black', position = position_dodge(.9)) +
	labs(y = "Proportion Overgeneralized \n", x = "\n Study Task", fill="Distractor") +
	coord_cartesian(ylim=c(0, 1)) + 
    scale_y_continuous(breaks=seq(0, 1, 0.1),expand = c(0,0)) +
    scale_fill_manual(values = c('grey85','grey50', 'grey25')) + 
    theme_bw() + 
    theme(axis.text.x = element_text(face = "plain", color = "black", size = 12),
	    axis.title.x = element_text(face="bold", size=14),
       	axis.text.y = element_text(face = "plain", color = "black", size = 12),
	    axis.title.y = element_text(face="bold", size=16),
	    legend.text = element_text(size = 12),
	    legend.title = element_text(size = 14),
	    panel.grid.major.x = element_blank() , 
	    panel.grid.major.y = element_line(color = "grey"),
	    panel.grid.minor.y = element_line(color = "grey"),
	    axis.line = element_line(colour = "black"),
	    legend.position="bottom")
ggsave('E2 remindings to distractor cues.png',distract.plot, width = 7, height = 7, units = 'in')

# # types of reminding successes by condition
# # # Get data for plot and fitted values from regression
plot3df <-  subset(data.reminding, principle != "Distractor" & reminding == 1)
plot3df$condition <- as.factor(as.character(plot3df$condition))
plot3df$fitted.casebased <- predict(newdata = plot3df, mod9CB, type = "response")
plot3df$fitted.principlebased <- predict(newdata = plot3df, mod10CB, type = "response")

# # # Add descriptives for fitted values to existing descriptives
plot3df.cases <- desc.remind.type[,c(1,2,7,8)]
plot3df.cases$Response <- 'Case-based'

plot3df.principle <- desc.remind.type[,c(1,4,9,10)]
plot3df.principle$Response <- 'Principle-based'

names(plot3df.cases)[2:4] <- names(plot3df.principle)[2:4] <- c('proportion', 'lower95ci', 'upper95ci')

plot3df.summary <- rbind(plot3df.cases,plot3df.principle)

plot3df.summary$fitted <- c(
	ddply(plot3df, .(condition), summarize, fitted.reminding = mean(fitted.casebased))$fitted.reminding,
	ddply(plot3df, .(condition), summarize, fitted.reminding = mean(fitted.principlebased))$fitted.reminding
	)

response.plot <- ggplot(plot3df.summary,aes(x=condition, fill = Response, y = proportion)) + 
	geom_bar(stat = 'identity', col = 'black',
		position = position_dodge())+ 
	geom_errorbar(aes(ymin= lower95ci, ymax= upper95ci),width=.2, position = position_dodge(.9)) + 
	geom_point(data = plot3df.summary, aes(x=condition, fill = Response, y = fitted), pch = 18,
		size = 5, color = 'black', position = position_dodge(.9)) +
	labs(y = "Proportion of Successful Remindings \n", x = "\n Study Task", fill="Response Type") +
	coord_cartesian(ylim=c(0, 1)) + 
    scale_y_continuous(breaks=seq(0, 1, 0.1),expand = c(0,0)) +
    scale_fill_manual(values = c('grey85','grey50', 'grey25')) + 
    theme_bw() + 
    theme(axis.text.x = element_text(face = "plain", color = "black", size = 12),
	    axis.title.x = element_text(face="bold", size=14),
       	axis.text.y = element_text(face = "plain", color = "black", size = 12),
	    axis.title.y = element_text(face="bold", size=16),
	    legend.text = element_text(size = 12),
	    legend.title = element_text(size = 14),
	    panel.grid.major.x = element_blank() , 
	    panel.grid.major.y = element_line(color = "grey"),
	    panel.grid.minor.y = element_line(color = "grey"),
	    axis.line = element_line(colour = "black"),
	    legend.position="bottom")
ggsave('E2 types of successful remindings.png',response.plot, width = 7, height = 7, units = 'in')
