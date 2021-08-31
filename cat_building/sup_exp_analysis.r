# cat_building - Supplemental Experiment (Appendix F)


# write output of analysis to text file?
write.output <- TRUE

# load required packages
libs <- c("plyr","ordinal","ggplot2")
lapply(libs, library, character.only = TRUE)

# define standard error function
sem <- function(var){
	return(sd(var) / sqrt(length(var[!is.na(var)])))
}

# turn off scientific notation
options(scipen = 999)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Read in data
data.comp <- read.csv('Supplemental experiment data.csv')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Descriptive Statistics

# # Descriptives for similarity ratings by condition
simrating.desc.cond <- ddply(data.comp, .(condition), summarize,
	mean.rating = mean(sim.rating), median.rating = median(sim.rating),
	sd.rating = sd(sim.rating), sem.rating = sem(sim.rating))

simrating.desc.cond  <- cbind(simrating.desc.cond[,1], 
	rep("Combined",2),rep("Combined",2),simrating.desc.cond [,-1])
names(simrating.desc.cond )[1:3] <- c("condition","principle","distractor")

# # Descriptives for similarity ratings by condition and principle
simrating.desc.princ <- ddply(data.comp, .(condition,principle), summarize,
	mean.rating = mean(sim.rating), median.rating = median(sim.rating),
	sd.rating = sd(sim.rating), sem.rating = sem(sim.rating))

simrating.desc.princ <- cbind(simrating.desc.princ[,1:2], 
	rep("Combined",2),simrating.desc.princ [,-c(1:2)])
names(simrating.desc.princ)[3] <- c("distractor")

# # Descriptives for similarity ratings by condition and distractor
simrating.desc.distract <- ddply(data.comp, .(condition, distractor), summarize,
	mean.rating = mean(sim.rating), median.rating = median(sim.rating),
	sd.rating = sd(sim.rating), sem.rating = sem(sim.rating))

simrating.desc.distract  <- cbind(simrating.desc.distract[,1], 
	rep("Combined",3), simrating.desc.distract[,-1])
names(simrating.desc.distract)[1:2] <- c("condition","principle")

# # Descriptives for similarity ratings by condition, principle, and distractor
simrating.desc.princ.distract <- ddply(data.comp, .(condition,principle, distractor), summarize,
	mean.rating = mean(sim.rating), median.rating = median(sim.rating),
	sd.rating = sd(sim.rating), sem.rating = sem(sim.rating))

# # combine into single data.frame for all target cues
descriptives.simratings <- rbind(simrating.desc.cond, 
	simrating.desc.princ,simrating.desc.distract,simrating.desc.princ.distract)

# # # Print descriptives
cat('\n');print("SIMILARITY RATINGS BY CONDITION, PRINCIPLE, AND DISTRACTOR DESCRIPTIVES")
print(descriptives.simratings)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(descriptives.simratings, 'Supplemental Experiment similarity ratings descriptives.csv', row.names = FALSE)
}

# # Descriptives for the number of similarities articulated in Unrelated-pairs
explain.desc <- ddply(subset(data.comp, condition == 'Unrelated-pairs'), .(pnum,distractor),summarize,
	total.components.listed = sum(spurious.similarity))

# # # Print descriptives
cat('\n');print("SIMILARITIES LISTED IN UNRELATED-PAIRS")
print(explain.desc)

# # # write descriptives to file
if(write.output == TRUE){
	write.csv(explain.desc, 'Supplemental Experiment similarities listed in unrelated pairs.csv', row.names = FALSE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inferential Statistics

# # save a copy of stats output as text file
if(write.output == TRUE){
	sink('Supplemental Experiment Analysis Output.txt', split = TRUE)
} 

# # Similarity ratings by condition and principle
cat('\n');cat('\n'); print("SIMILARITY RATINGS BY CONDITION AND PRINCIPLE")
data.comp$condition <- relevel(data.comp$condition, ref = 'Related-pairs')
data.comp$principle <- relevel(data.comp$principle, ref = 'Convergence')
clm1a <- clm(as.factor(sim.rating) ~ condition*principle, data = data.comp)
print(summary(clm1a))

data.comp$principle <- relevel(data.comp$principle, ref = 'Problem-as-a-solution')
clm1b <- clm(as.factor(sim.rating) ~ condition*principle, data = data.comp)
print(summary(clm1b))

data.comp$condition <- relevel(data.comp$condition, ref = 'Unrelated-pairs')
data.comp$principle <- relevel(data.comp$principle, ref = 'Convergence')
clm1c <- clm(as.factor(sim.rating) ~ condition*principle, data = data.comp)
print(summary(clm1c))

# # Similarity ratings by condition and principle
cat('\n');cat('\n'); print("SIMILARITY RATINGS FOR UNRELATED-PAIRS BY DISTRACTOR")
clm2 <- clm(as.factor(sim.rating) ~ distractor, 
	data = subset(data.comp, condition == 'Unrelated-pairs'))
print(summary(clm2))

# # Does the number of similarities listed between distractors and target cases significantly occur?
cat('\n');cat('\n'); print("SIMILARITIES LISTED BETWEEN DISTRACTORS AND TARGET CASES")
print(binom.test(nrow(subset(explain.desc, total.components.listed == 1)),
	n = nrow(explain.desc),	p = 0, alternative = 'greater', conf.level = .95))

if(write.output == TRUE){
	sink()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Visualizations
plotDF <- rbind(data.comp,data.comp)
plotDF$principle <- as.character(plotDF$principle)
plotDF$principle[117:nrow(plotDF)] <- rep('Overall', (nrow(plotDF)-116))
plotDF$principle <- as.factor(plotDF$principle)
plotDF$principle <- relevel(plotDF$principle, ref = 'Overall')
plotDF$condition <- relevel(plotDF$condition, ref = 'Related-pairs')

rating.plot <- ggplot(plotDF,aes(x = condition, y = sim.rating)) +
	geom_point(position = position_jitter(width = 0.4, height = 0.1), size = 3, color = 'grey35', alpha = .7)+
	geom_boxplot(outlier.shape = NA, alpha = .2, fill = 'grey80')+
	stat_summary(fun.y = 'mean', geom = "point",pch = 18, size = 5, color = 'black')+
	facet_wrap(~principle, ncol = 3)+
	theme_bw() + 
	labs(y = "Similarity Rating \n", x = "\n Pair Type") +
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
ggsave('similarity ratings.png',rating.plot, width = 10.8, height = 6, units = 'in')
