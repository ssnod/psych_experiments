# Analyses and visualization for initial learning data

## save a copy of stats output as text file
if(write_output == TRUE){
	setwd(output_dir)
	sink('E1 Initial Learning Analysis Output.txt', split = TRUE)
} 

## does initial BCL and TCL learning quality differ?
cat('\n');cat('\n'); print("INITIAL LEARNING BY CONDITION")
class_dat$task <- as.factor(class_dat$task)
levels(class_dat$task) <- c('Between-Subject', 'Within-Subject')
class_dat$task <- relevel(class_dat$task, ref = 'Between-Subject')
ILmod1a <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = class_dat, family = binomial())
print(summary(ILmod1a))

class_dat$task <- relevel(class_dat$task, ref = 'Within-Subject')
ILmod1b <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = class_dat, family = binomial())
print(summary(ILmod1b))

## do counterbalance orders (stimulus domains) differ in ease of acquisition?
cat('\n');cat('\n'); print("INITIAL LEARNING BY DOMAIN")
class_dat$domain <- as.factor(class_dat$domain)
class_dat$domain <- relevel(class_dat$domain, ref = 'rock')
ILmod2a <- glmer(accuracy ~ domain*block + (1|id) + (1|item), data = class_dat, family = binomial())
print(summary(ILmod2a))
class_dat$domain <- relevel(class_dat$domain, ref = 'mobile')
ILmod2b <- glmer(accuracy ~ domain*block + (1|id) + (1|item), data = class_dat, family = binomial())
print(summary(ILmod2b))

if(write_output == TRUE){
	sink()
	setwd(home_dir)
}

## get predicted values from regression model
class_dat$predicted <- predict(ILmod1a, newdata = class_dat, type = 'response')

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization for initial learning data
## summarize combined data w/ predicted values
IL_plot_dat1 <- ddply(class_dat, .(task,block), summarize,
 	meanPred = mean(predicted), semPred = sem(predicted), meanAcc = mean(accuracy))
IL_plot_dat1 <- cbind(IL_plot_dat1[,c(1,2)], rep('Combined Domains', nrow(IL_plot_dat1)),IL_plot_dat1[,c(3:5)])
names(IL_plot_dat1)[3] <- 'domain'

## summarize domain data w/ predicted values
IL_plot_dat2 <- ddply(class_dat, .(task, block, domain), summarize,
 	meanPred = mean(predicted), semPred = sem(predicted), meanAcc = mean(accuracy))

## merge combined data with domain data
plot_dat <- rbind(IL_plot_dat1, IL_plot_dat2)

## set labels for plot
plot_dat$domain <- as.factor(plot_dat$domain)
levels(plot_dat$domain) <- c('Combined', "Mobile Domain", 'Rock Domain')
levels(plot_dat$task) <- c("Category Learning", "Baseline")
plot_dat$block2 <- plot_dat$block + .5

## create plot
IL_plot <- ggplot(plot_dat, aes(x = block, y = meanPred, color = task))+
	facet_wrap(~domain, ncol = 3) +
	geom_line(size = 2)+
	geom_point(aes(x = block2, y = meanAcc), size = 5, alpha = .5) +
	geom_errorbar(aes(ymin = meanPred - semPred, ymax = meanPred + semPred), size = 1.5, width = 0.5)+
	scale_color_manual(values = c("#009E73","#D55E00")) +
	geom_hline(yintercept = .33, size = 1.5, linetype = 'dashed', color = "#56B4E9") +
	labs(y = "Accuracy \n", x = "Block", color = " ",
		caption = "Error bars reflect +/- 1 SEM.\nRight offset points reflect unadjusted means.") +
	theme(axis.text.x = element_text(face = "plain", color = "black", size = 12)) +
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5)) + 
	theme(axis.line = element_line(colour = "black"), 
		panel.border = element_blank()) + 
	coord_cartesian(ylim=c(0.3, 1)) + 
    scale_y_continuous(breaks=seq(0.3, 1, .1)) +
    theme(legend.position = "bottom",
    	legend.text = element_text(size = rel(1.5)), 
		legend.title= element_text(size = rel(2)),
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00))

## save plot
if(write_output == TRUE){
	setwd(output_dir)
	ggsave('initial learning plot.png',plot = IL_plot,width = 12, height = 7, units = 'in')
}
setwd(home_dir)
