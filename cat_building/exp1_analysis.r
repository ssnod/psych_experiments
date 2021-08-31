#  Experiment 1	


# write output of analysis to text file?
write.output <- TRUE

# load required packages
libs <- c("plyr", "binom","oddsratio", "lmerTest")
lapply(libs, library, character.only = TRUE)

# define standard error function
sem <- function(var){
	return(sd(var) / sqrt(length(var[!is.na(var)])))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Read in data

# read in data for spontaneous transfer analyses
data.spont <- read.csv('Experiment 1 data.csv')

# subset participants with complete hint-aided transfer data
data.hint <- subset(data.spont, hint.include == 1)

# Category-building encoding data
CB.encode <- data.frame(id = rep(subset(data.spont, condition == "Category-building")[,1], 5),
	condition = rep(subset(data.spont, condition == "Category-building")[,2], 5),
	count.bal = rep(subset(data.spont, condition == "Category-building")[,3], 5),
	case = c(rep("avalanche", nrow(subset(data.spont, condition == "Category-building"))),rep("wildfire", nrow(subset(data.spont, condition == "Category-building"))),rep("earthquake", nrow(subset(data.spont, condition == "Category-building"))),rep("solar", nrow(subset(data.spont, condition == "Category-building"))), rep("final", nrow(subset(data.spont, condition == "Category-building")))),
	position = c(subset(data.spont, condition == "Category-building")[,21], subset(data.spont, condition == "Category-building")[,27], subset(data.spont, condition == "Category-building")[,33], subset(data.spont, condition == "Category-building")[,39], rep(5, nrow(subset(data.spont, condition == "Category-building")))),
	explanation = c(subset(data.spont, condition == "Category-building")[,16], subset(data.spont, condition == "Category-building")[,22], subset(data.spont, condition == "Category-building")[,28], subset(data.spont, condition == "Category-building")[,34], subset(data.spont, condition == "Category-building")[,40]),
	smallscale = c(subset(data.spont, condition == "Category-building")[,17], subset(data.spont, condition == "Category-building")[,23], subset(data.spont, condition == "Category-building")[,29], subset(data.spont, condition == "Category-building")[,35], subset(data.spont, condition == "Category-building")[,41]),
	damage =  c(subset(data.spont, condition == "Category-building")[,18], subset(data.spont, condition == "Category-building")[,24], subset(data.spont, condition == "Category-building")[,30], subset(data.spont, condition == "Category-building")[,36], subset(data.spont, condition == "Category-building")[,42]),
	prevention = c(subset(data.spont, condition == "Category-building")[,19], subset(data.spont, condition == "Category-building")[,25], subset(data.spont, condition == "Category-building")[,31], subset(data.spont, condition == "Category-building")[,37], subset(data.spont, condition == "Category-building")[,43]),
	agreement = c(subset(data.spont, condition == "Category-building")[,20], subset(data.spont, condition == "Category-building")[,26], subset(data.spont, condition == "Category-building")[,32], subset(data.spont, condition == "Category-building")[,38], subset(data.spont, condition == "Category-building")[,44]))

# Double-comparison encoding data
DC.encode <- data.frame(id = rep(subset(data.spont, condition == "Double-comparison")[,1], 2),
	condition = rep(subset(data.spont, condition == "Double-comparison")[,2], 2),
	count.bal = rep(subset(data.spont, condition == "Double-comparison")[,3], 2),
	position = c(rep('first', nrow(subset(data.spont, condition == "Double-comparison"))),rep('second', nrow(subset(data.spont, condition == "Double-comparison")))),
	explanation = c(subset(data.spont, condition == "Double-comparison")[,5], subset(data.spont, condition == "Double-comparison")[,11]),
	smallscale = c(subset(data.spont, condition == "Double-comparison")[,6], subset(data.spont, condition == "Double-comparison")[,12]),
	damage =  c(subset(data.spont, condition == "Double-comparison")[,7], subset(data.spont, condition == "Double-comparison")[,13]),
	prevention = c(subset(data.spont, condition == "Double-comparison")[,8], subset(data.spont, condition == "Double-comparison")[,14]),
	agreement = c(subset(data.spont, condition == "Double-comparison")[,9], subset(data.spont, condition == "Double-comparison")[,15]))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Descriptive Statistics

# # Descriptives for spontaneous transfer performance
spont.descriptives <- ddply(data.spont, .(condition), summarize, 
	spont.transfer = mean(spont.highest.score), 
	frequency = sum(spont.highest.score),
	n = length(spont.highest.score),
	num.solutions = mean(num.spont),sd.solutions = sd(num.spont))

# # # Generate 95% CIs for transfer rate and add to descriptives
spont.descriptives <- cbind(spont.descriptives, 
	binom.logit(spont.descriptives[,3], spont.descriptives[,4], 
		conf.level = 0.95)[,c(5,6)])
names(spont.descriptives)[7:8] <- c("lower95ci","upper95ci")

# # # Print descriptives
cat('\n');print("SPONTANEOUS TRANSFER DESCRIPTIVES")
print(spont.descriptives)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(spont.descriptives, 'E1 spont xfer descriptives.csv', row.names = FALSE)
}


# # Descriptives for spontaneous transfer performance
hint.descriptives <- ddply(data.hint, .(condition), summarize, 
	hint.transfer = mean(hint.highest.score), 
	frequency = sum(hint.highest.score),
	n = length(hint.highest.score))

# # # Generate 95% CIs for transfer rate and add to descriptives
hint.descriptives <- cbind(hint.descriptives, 
	binom.logit(hint.descriptives[,3], hint.descriptives[,4], 
		conf.level = 0.95)[,c(5,6)])
names(hint.descriptives)[5:6] <- c("lower95ci","upper95ci")

# # # Print descriptives
cat('\n'); print("HINT-AIDED TRANSFER DESCRIPTIVES")
print(hint.descriptives)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(hint.descriptives, 'E1 hint xfer descriptives.csv', row.names = FALSE)
}


# # Descriptives for final study task
study.descriptives <- ddply(subset(data.spont, condition != 'Baseline'),
	.(condition), summarize, 
	meanFinalStudyTask = mean(final.encoding.all),
	std = sd(final.encoding.all))

# # # Print descriptives
cat('\n');print("FINAL STUDY TASK DESCRIPTIVES")
print(study.descriptives)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(study.descriptives, 'E1 final study task descriptives.csv', row.names = FALSE)
}


# # Descriptives for study task performance across training
study.pos.desc.CB <- ddply(CB.encode, .(condition, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))[-6,]
study.pos.desc.DC <- ddply(DC.encode, .(condition, position), summarize,
	score = mean(explanation), sd.score = sd(explanation))
study.pos.descriptives <- rbind(study.pos.desc.CB, study.pos.desc.DC)

# # # Print descriptives
cat('\n');print("STUDY TASK SCORE BY POSITION")
print(study.pos.descriptives)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(study.pos.descriptives, 'E1 study task by position descriptives.csv', row.names = FALSE)
}

# # Descriptives for study task performance across training
study.case.desc <- ddply(subset(CB.encode, case != "final"), .(condition, case), summarize,
	score = mean(explanation, na.rm = T), sd.score = sd(explanation,na.rm = T))

# # # Print descriptives
cat('\n');print("CATEGORY-BUILDING STUDY TASK SCORE BY CASE")
print(study.case.desc )

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(study.case.desc, 'E1 study task by CASE descriptives.csv', row.names = FALSE)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inferential Statistics

# # save a copy of stats output as text file
if(write.output == TRUE){
	sink('Experiment 1 Analysis Output.txt', split = TRUE)
} 

# # Sponteous transfer
cat('\n');cat('\n'); print("SPONTANEOUS TRANSFER RESULTS")
# # # do study tasks improve transfer relative to baseline?
data.spont$condition <- relevel(data.spont$condition, ref = "Baseline")
mod1BL <- glm(spont.highest.score ~ condition, data.spont, family="binomial")
print(summary(mod1BL))
# # # odds ratios for model
print(or_glm(data = data.spont, model = mod1BL, incr = levels(data.spont$condition)))

# # # does category-building improve transfer relative to comparison conditions?
data.spont$condition <- relevel(data.spont$condition, ref = "Category-building")
mod1CB <- glm(spont.highest.score ~ condition, data.spont, family="binomial")
print(summary(mod1CB))

# # # does double-comparison improve transfer over single comparison?
data.spont$condition <- relevel(data.spont$condition, ref = "Double-comparison")
mod1DC <- glm(spont.highest.score ~ condition, data.spont, family="binomial")
print(summary(mod1DC))
# # # odds ratios for Category-building vs. Double-comparison
print(or_glm(data = data.spont, model = mod1DC, incr = levels(data.spont$condition)))

# # # single comparison as reference for odds-ratios
data.spont$condition <- relevel(data.spont$condition, ref = "Single-comparison")
mod1SC <- glm(spont.highest.score ~ condition, data.spont, family="binomial")
print(summary(mod1SC))
# # # odds ratios for Category-building vs. Single-comparison and Double-comparison vs. Single-comparison
print(or_glm(data = data.spont, model = mod1SC, incr = levels(data.spont$condition)))


# # Hint-aided transfer
cat('\n');cat('\n');print("HINT-AIDED TRANSFER RESULTS")
# # # does category building differ from double- and single-comparison?
data.hint$condition <- relevel(data.hint$condition, ref = "Category-building")
mod2CB <- glm(hint.highest.score ~ condition, 
	data.hint, family="binomial")
print(summary(mod2CB))

# # # does double-comparison differ from single-comparison?
data.hint$condition <- relevel(data.hint$condition, ref = "Double-comparison")
mod2DC <- glm(hint.highest.score ~ condition, 
	data.hint, family="binomial")
print(summary(mod2DC))
# # # odds ratios for Category-building vs. Double-comparison
print(or_glm(data = data.hint, model = mod2DC, incr = levels(data.spont$condition)))

# # # single-comparison as reference for odds-ratios
data.hint$condition <- relevel(data.hint$condition, ref = "Single-comparison")
mod2SC <- glm(hint.highest.score ~ condition, 
	data.hint, family="binomial")
print(summary(mod2SC))
# # # odds ratios for for Double-comparison vs. Single-comparison
print(or_glm(data = data.hint, model = mod2SC, incr = levels(data.spont$condition)))


# # Final study task performance
cat('\n');cat('\n');print("FINAL STUDY TASK PERFORMANCE RESULTS")
# # # Does category-building differ from comparison in final study task performance?
data.spont$condition <- relevel(data.spont$condition, ref = "Category-building")
mod3CB <- lm(final.encoding.all ~ condition, data = subset(data.spont, condition != "Baseline"))
print(summary(mod3CB))

# # # Does double-comparison differ from single-comparison in final study task performance?
data.spont$condition <- relevel(data.spont$condition, ref = "Double-comparison")
mod3DC <- lm(final.encoding.all ~ condition, data = subset(data.spont, condition != "Baseline"))
print(summary(mod3DC))

# # Study task performance by position - Category-building
cat('\n');cat('\n');print("STUDY TASK PERFORMANCE BY POSITION FOR CATEGORY-BUILDING")
mod4CB <- lmer(explanation ~ position + (1|id), data = CB.encode)
print(summary(mod4CB))

# # Study task performance by position - Double-comparison
cat('\n');cat('\n');print("STUDY TASK PERFORMANCE BY POSITION FOR CATEGORY-BUILDING")
mod4DC <- lmer(explanation ~ position + (1|id), data = DC.encode)
print(summary(mod4DC))

# # Study task performance by case - Category-building
cat('\n');cat('\n');print("STUDY TASK PERFORMANCE BY CASE FOR CATEGORY-BUILDING")
CB.encode$case <- relevel(CB.encode$case, ref = 'avalanche')
mod5AV <- lmer(explanation ~ case + (1|id), data = CB.encode)
print(summary(mod5AV))

CB.encode$case <- relevel(CB.encode$case, ref = 'earthquake')
mod5EQ <- lmer(explanation ~ case + (1|id), data = CB.encode)
print(summary(mod5EQ))

CB.encode$case <- relevel(CB.encode$case, ref = 'solar')
mod5SF <- lmer(explanation ~ case + (1|id), data = CB.encode)
print(summary(mod5SF))


if(write.output == TRUE){
	sink()
}
