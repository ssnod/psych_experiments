#  cat_building - Experiment 3


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
data.reminding <- read.csv('Experiment 3a reminding data.csv')
# # keep data from subs that completed at least half of reminding assessment, but drop missing values
data.reminding <- subset(data.reminding, include == 1)

# read in data from study tasks
data.study <- read.csv('Experiment 3a study task data.csv')
# # # create long form data for encoding analyses + descriptives
data.study.lf <- gather(data.study[,c(1,2,21,42)], key = 'stim', value = 'task.score',-c('pnum','condition')) 
data.study.lf$principle <- ifelse(data.study.lf$stim == "paas.avg", "Problem-as-a-solution","Convergence")
data.study.lf$principle <- as.factor(data.study.lf$principle)

# merge reminding data and encoding data 
data.combined <- merge(x = data.reminding, y= data.study.lf[,-c(2,3)], 
	by= c('pnum', 'principle'), all.x = TRUE, all.y = FALSE)

# study cases
study.cases <- data.frame(id = rep(data.study[,1],8),
	condition = rep(data.study[,2], 8), 
	count.bal = rep(data.study[,3], 8),
	principle = c(rep('Problem-as-a-solution',  nrow(data.study)*4),rep('Convergence',  nrow(data.study)*4)),
	case = c(rep("avalanche", nrow(data.study)),rep("wildfire", nrow(data.study)),rep("earthquake", nrow(data.study)),rep("solar", nrow(data.study)), 
		rep("army", nrow(data.study)),rep("tank", nrow(data.study)),rep("wood", nrow(data.study)),rep("oil", nrow(data.study))),
	explanation = c(data.study[,7], data.study[,11], data.study[,15], data.study[,19], data.study[,28], data.study[,32], data.study[,36], data.study[,40]))

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

desc.target.condition <- cbind(desc.target.condition[,1], rep("Combined",4),desc.target.condition[,-1])
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
	write.csv(descriptives.targets, 'E3 reminding to targets by condition and principle descriptives.csv', row.names = FALSE)
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

desc.distract.condition <- cbind(desc.distract.condition[1],rep("Combined",4),desc.distract.condition[,-1])
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
	write.csv(descriptives.distractors, 'E3 reminding to distractors by case descriptives.csv', row.names = FALSE)
}


# # Descriptives for number of cases listed for target cues by condition
desc.num.cases.cond <- ddply(subset(data.reminding, principle != "Distractor"), 
	.(condition), summarize, avg.cases = mean(mem.listed), sd.cases = sd(mem.listed), 
	min.cases = min(mem.listed), max.cases = max(mem.listed), n.obs = length(mem.listed))

# # # Print descriptives
cat('\n');print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION AND REMINDING SUCCESS")
print(desc.num.cases.cond)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.num.cases.cond, 'E3a number of responses to target cues by condition and reminding success descriptives.csv', row.names = FALSE)
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
	write.csv(desc.remind.type, 'E3a type of responses for successful remindings descriptives.csv', row.names = FALSE)
}


# # Descriptives for final study task performance by condition
desc.study.cond <- ddply(data.study.lf, .(condition), summarize,
	mean.score = mean(task.score), sem.score = sem(task.score),
	sd.score = sd(task.score))

desc.study.cond <- cbind(desc.study.cond[,1], rep("Combined",4),desc.study.cond[,-1])
names(desc.study.cond)[1:2] <- c('condition','principle')

# # Descriptives for final study task performance by principle
desc.study.princ <- ddply(data.study.lf, .(principle), summarize,
	mean.score = mean(task.score), sem.score = sem(task.score),
	sd.score = sd(task.score))

desc.study.princ <- cbind(rep("Combined",2),desc.study.princ)
names(desc.study.princ)[1] <- 'condition'

# # Descriptives for final study task performance by condition and principle
desc.study.cond.princ <- ddply(data.study.lf, .(condition, principle), summarize,
	mean.score = mean(task.score), sem.score = sem(task.score),
	sd.score = sd(task.score))

# # combine into single data.frame for all study task data
descriptives.study <- rbind(desc.study.cond,
	desc.study.princ,desc.study.cond.princ)

# # # Print descriptives
cat('\n');print("AVERAGE STUDY TASK PERFORMANCE")
print(descriptives.study)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.study, 'E3a average study task performance descriptives.csv', row.names = FALSE)
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
	write.csv(descriptives.study.position, 'E3a study task performance by positiondescriptives.csv', row.names = FALSE)
}

desc.study.case <- ddply(study.cases, .(case, principle), summarize,
	score = mean(explanation), sd.score = sd(explanation))
desc.study.case <- desc.study.case[order(desc.study.case[,3]),]

# # # Print descriptives
cat('\n');print("CATEGORY-BUILDING STUDY PERFORMANCE BY CASE")
print(desc.study.case)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(desc.study.case, 'E3a category-building study performance by case descriptives.csv', row.names = FALSE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inferential Statistics

# # save a copy of stats output as text file
if(write.output == TRUE){
	sink('Experiment 3a Analysis Output.txt', split = TRUE)
} 

# # Sponteous reminding - target cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION RESULTS")
# # # Is there a higher rate of spontaneous remindings in the FullTask than Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
mod1FT <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1FT))

# # # Is there a higher rate of spontaneous remindings in Only-explanation than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyExplain")
mod1OE<- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1OE))

# # # Is there a higher rate of spontaneous remindings in Only-instructions than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
mod1OI<- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod1OI))


# # Calculate Odds Ratios as effect size for primary analysis
FT.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "FullTask")$reminding)
FT.n <- length(subset(data.reminding, principle != 'Distractor' & condition == "FullTask")$reminding)
OL.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "OnlyLabels")$reminding)
OL.n <- length(subset(data.reminding, principle != 'Distractor' & condition == "OnlyLabels")$reminding)
OI.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "OnlyInstruct")$reminding)
OI.n <- length(subset(data.reminding, principle != 'Distractor' & condition == "OnlyInstruct")$reminding)
OE.hits <- mean(subset(data.reminding, principle != 'Distractor' & condition == "OnlyExplain")$reminding)
OE.n <- length(subset(data.reminding, principle != 'Distractor' & condition == "OnlyExplain")$reminding)

# # # Calculate Odds Ratio
FT.OL.OR <- (FT.hits / (1- FT.hits)) / (OL.hits/ (1-OL.hits))
FT.OI.OR <- (FT.hits / (1- FT.hits)) / (OI.hits/ (1-OI.hits))
FT.OE.OR <- (FT.hits / (1- FT.hits)) / (OE.hits/ (1-OE.hits))
OL.OI.OR <- (OL.hits / (1- OL.hits)) / (OI.hits/ (1-OI.hits))
OL.OE.OR <- (OL.hits / (1- OL.hits)) / (OE.hits/ (1-OE.hits))
OI.OE.OR <- (OI.hits / (1- OI.hits)) / (OE.hits/ (1-OE.hits))

# # # Print results
cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION EFFECTS SIZES")
print(paste('Full-task vs. Only-Labels Odds Ratio:', FT.OL.OR))
cat('\n')
print(paste('Full-task vs. Only-Instructions Odds Ratio:', FT.OI.OR))
cat('\n')
print(paste('Full-task vs. Only-Explanation Odds Ratio:', FT.OE.OR))
cat('\n')
print(paste('Only-Labels vs. Only-Instructions Odds Ratio:', OL.OI.OR))
cat('\n')
print(paste('Only-Labels vs. Only-Explanation Odds Ratio:', OL.OE.OR))
cat('\n')
print(paste('Only-Instructions vs. Only-Explanation Odds Ratio:', OI.OE.OR))
cat('\n')


# # Sponteous reminding - target cues by condition and principle
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING TARGET CUES BY CONDITION AND PRINCIPLE RESULTS")
# # # Is there a higher rate of spontaneous remindings in Full task than Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2FTa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2FTa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2FTb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2FTb))

# # # Is there a higher rate of spontaneous remindings in Only-labels than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2OLa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OLa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2OLb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OLb))

# # # Is there a higher rate of spontaneous remindings in Only-explanation than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyExplain")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2OEa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OEa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2OEb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OEb))

# # # Spontaneous remindings for Only-instructions to see differences between principles
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
data.reminding$principle <- relevel(data.reminding$principle, ref = 'Problem-as-a-solution')
mod2OIa <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OIa))

data.reminding$principle <- relevel(data.reminding$principle, ref = 'Convergence')
mod2OIb <- glmer(reminding ~ condition*principle + (1|pnum), 
	subset(data.reminding, principle != "Distractor"), family = 'binomial')
print(summary(mod2OIb))


# # Spontaneous reminding - number of cases listed for target cues
cat('\n');cat('\n'); print("NUMBER OF RESPONSES PROVIDED TO TARGET CUES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
mod8FT <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8FT))

# # # Does Double-comparison differ from Single-comparison in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
mod8OL <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8OL))

# # # Does Double-comparison differ from Single-comparison in the number of responses provided for each reminding assessment?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
mod8OI <- lmer(mem.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor"))
print(summary(mod8OI))


# # Spontaneous reminding - number of case-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF CASE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
mod9FT <- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod9FT)

# # # Does Double-comparison differ from Single-comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
mod9OL<- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod9OL)

# # # Does Double-comparison differ from Single-comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
mod9OI<- glmer(cases.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod9OI)


# # Spontaneous reminding - number of principle-based reminding successes for target cues by condition
cat('\n');cat('\n'); print("NUMBER OF PRINCIPLE-BASED REMINDING SUCCESSES BY CONDITION")
# # # Does Category-building differ from comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
mod10FT <- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod10FT)

# # # Does Double-comparison differ from Single-comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
mod10OL<- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod10OL)

# # # Does Double-comparison differ from Single-comparison conditions in the number of case-based reminding successes?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
mod10OI<- glmer(set.listed ~ condition + (1|pnum), data = subset(data.reminding, principle != "Distractor" & reminding == 1), family = 'binomial')
summary(mod10OI)


# # Sponteous reminding - distractor cues by condition
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION RESULTS")
# # # Is there a higher rate of spontaneous remindings in the FullTask than Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
mod3FT <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3FT))

# # # Is there a higher rate of overgeneralization in Only-explanation than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyExplain")
mod3OE<- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3OE))

# # # Is there a higher rate of overgeneralization in Only-instructions than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
mod3OI<- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3OI))

data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
mod3OL<- glmer(reminding ~ condition + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod3OL))

# # Sponteous reminding - distractor cues by condition and cue
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CONDITION AND CUE RESULTS")
# # # Is there a higher rate of overgeneralization in the FullTask than Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "FullTask")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod4FTa <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4FTa)) # fails to converge

data.reminding$case <- relevel(data.reminding$case, ref = "winemerchant")
mod4FTb <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4FTb)) # fails to converge

# # # Is there a higher rate of overgeneralization in Only-explanation than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyExplain")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod4OEa <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OEa)) # fails to converge

data.reminding$case <- relevel(data.reminding$case, ref = "winemerchant")
mod4OEb <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OEb)) # fails to converge

# # # Is there a higher rate of overgeneralization in Only-instructions than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyInstruct")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod4OIa <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OIa)) # fails to converge

data.reminding$case <- relevel(data.reminding$case, ref = "winemerchant")
mod4OIb <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OIb)) # fails to converge

# # # Is there a higher rate of overgeneralization in Only-labels than other Only1 conditions?
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod4OLa <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OLa)) # fails to converge

data.reminding$case <- relevel(data.reminding$case, ref = "winemerchant")
mod4OLb <- glmer(reminding ~ condition*case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod4OLb)) # fails to converge

# # Sponteous reminding - distractor cues by condition and cue
cat('\n');cat('\n'); print("SPONTNAEOUS REMINDING DISTRACTOR CUES BY CUE RESULTS")
# # # Is there a higher rate of overgeneralization to the Wine Merchant distractor
data.reminding$condition <- relevel(data.reminding$condition, ref = "OnlyLabels")
data.reminding$case <- relevel(data.reminding$case, ref = "tworibbon")
mod5WM <- glmer(reminding ~ case + (1|pnum), 
	subset(data.reminding, principle == "Distractor"), family = 'binomial')
print(summary(mod5WM)) # fails to converge


# # Study task - performance by condition
cat('\n');cat('\n'); print("FINAL STUDY TASK PERFORMANCE BY CONDITION RESULTS")
# # # Does the FullTask differ in the ability to articulate the target principle from Only1 conditions?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'FullTask')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6FTa <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6FTa))

data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6FTb <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6FTb))

# # # Does the Only-explanation differ in the ability to articulate the target principle from other Only1 conditions?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'OnlyExplain')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6OEa <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OEa))

data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6OEb <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OEb))

# # # Does the Only-instructions differ in the ability to articulate the target principle from other Only1 conditions?
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'OnlyInstruct')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6OIa <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OIa))

data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6OIb <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OIb))

# # # Ability to articulate the target principle for Only-labels to see differences between principles
data.study.lf$condition <- relevel(data.study.lf$condition, ref = 'OnlyLabels')
data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Problem-as-a-solution')
mod6OLa <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OLa))

data.study.lf$principle <- relevel(data.study.lf$principle, ref = 'Convergence')
mod6OLb <- lm(task.score ~ condition*principle, data.study.lf)
print(summary(mod6OLb))

# # Final study task - performance by condition
cat('\n');cat('\n'); print("DO FINAL STUDY TASK DIFFS PREDICT REMINDING TO TARGET CUES")
# # # Does Full-task differ from Only1 conditions in spontaneous reminding when controlling for study task performance?
data.combined$condition <- relevel(data.combined$condition, ref = "FullTask")
mod7FT <- glmer(reminding ~ condition*task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7FT))

# # # Does Only-explanation differ from other Only1 conditions in spontaneous reminding when controlling for study task performance?
data.combined$condition <- relevel(data.combined$condition, ref = "OnlyExplain")
mod7OE <- glmer(reminding ~ condition*task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7OE))

# # # Does Only-instructions differ from other Only1 conditions in spontaneous reminding when controlling for study task performance?
data.combined$condition <- relevel(data.combined$condition, ref = "OnlyInstruct")
mod7OI <- glmer(reminding ~ condition*task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7OI))

# # # Does Only-labels differ from other Only1 conditions in spontaneous reminding when controlling for study task performance?
data.combined$condition <- relevel(data.combined$condition, ref = "OnlyLabels")
mod7OL <- glmer(reminding ~ condition*task.score + (1|pnum), 
	subset(data.combined, principle != "Distractor"), family = 'binomial')
print(summary(mod7OL))


# # Study task - performance across task within each condition. Included in coverletter, but not manuscript.
# study.cases$condition <- relevel(study.cases$condition, ref = 'FullTask')
# mod11FT <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11FT))

# study.cases$condition <- relevel(study.cases$condition, ref = 'OnlyExplain')
# mod11OE <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11OE))

# study.cases$condition <- relevel(study.cases$condition, ref = 'OnlyInstruct')
# mod11OI <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11OI))

# study.cases$condition <- relevel(study.cases$condition, ref = 'OnlyLabels')
# mod11OL <- lmer(explanation ~ position*condition + (1|id), data = study.cases)
# print(summary(mod11OL))

# # study task - performance by case aggregating over condition
study.cases$case <- relevel(study.cases$case, ref = 'avalanche')
mod12a <- lmer(explanation ~ case + (1|id), data = subset(study.cases, principle == 'Problem-as-a-solution'))
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
plot1df$fitted.cond <- predict(newdata = plot1df, mod1FT, type = 'response')
plot1df$fitted.princ <- predict(newdata = plot1df, mod2FTa, type = 'response')

# # # Add descriptives for fitted values to existing descriptives
plot1df.summary <- subset(descriptives.targets, condition != 'Combined')
plot1df.summary$fitted <- c(
	ddply(plot1df, .(condition), summarize, fitted.reminding = mean(fitted.cond))$fitted.reminding,
	ddply(plot1df, .(condition, principle), summarize, fitted.reminding = mean(fitted.princ))$fitted.reminding
	)

# # # rename vars for plotting
plot1df.summary[,1]<-as.factor(plot1df.summary[,1])
levels(plot1df.summary[,1]) <- c("Full\nTask","Only\nExplain.","Only\nInstruct.","Only\nLabels","Total")
plot1df.summary[,2]<-as.factor(plot1df.summary[,2])
levels(plot1df.summary[,2]) <- c("Overall","Convergence","Distractor","Problem-as-a-Solution")

# # # create plot and write to dik
remind.plot <- ggplot(plot1df.summary,aes(x=condition, fill = condition, y = spont.reminding)) + 
	geom_bar(stat = 'identity', col = 'black',
		fill = rep(c('grey85','grey65','grey45','grey25'),3))+ 
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
ggsave('E3a remindings to target cues.png',remind.plot, width = 10.8, height = 6, units = 'in')

# # Overgeneralization to distractors by condition and cue 
# # # Get fitted values from regression models
plot2df <- subset(data.reminding, principle == 'Distractor')
plot2df$condition <- as.factor(as.character(plot2df$condition))
plot2df$fitted.cond <- predict(newdata = plot2df, mod3FT, type = 'response')
plot2df$fitted.case <- predict(newdata = plot2df, mod4FTa, type = 'response')

# # # Add descriptives for fitted values to existing descriptives
plot2df.summary <- subset(descriptives.distractors, condition != 'Combined')
plot2df.summary$fitted <- c(
	ddply(plot2df, .(condition), summarize, fitted.reminding = mean(fitted.cond))$fitted.reminding,
	ddply(plot2df, .(condition, case), summarize, fitted.reminding = mean(fitted.case))$fitted.reminding
	)

# # # rename vars for plotting
plot2df.summary[,1]<-as.factor(plot2df.summary[,1])
levels(plot2df.summary[,1]) <-c("Full\nTask","Only\nExplain.","Only\nInstruct.","Only\nLabels","Total")
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
ggsave('E3a remindings to distractor cues.png',distract.plot, width = 7, height = 7, units = 'in')

# # types of reminding successes by condition
# # # Get data for plot and fitted values from regression
plot3df <-  subset(data.reminding, principle != "Distractor" & reminding == 1)
plot3df$condition <- as.factor(as.character(plot3df$condition))
plot3df$fitted.casebased <- predict(newdata = plot3df, mod9FT, type = "response")
plot3df$fitted.principlebased <- predict(newdata = plot3df, mod10FT, type = "response")

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
ggsave('E3a remindings types cues.png',response.plot, width = 7, height = 7, units = 'in')
