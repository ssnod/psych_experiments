# Analyses and visualization for response time data

## save a copy of stats output as text file
if(write_output == TRUE){
	setwd(output_dir)
	sink('E1 Response Time Analysis Output.txt', split = TRUE)
} 

## condition by block analyses
cat('\n');cat('\n'); print("TRANSFER RT BY CONDITION")
xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Baseline")
xferRTmod1 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = xfer_dat_rt)
print(summary(xferRTmod1))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Spontaneous")
xferRTmod2 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = xfer_dat_rt)
print(summary(xferRTmod2))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Hint-aided")
xferRTmod3 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = xfer_dat_rt)
print(summary(xferRTmod3))

## to follow from accuracy-based analysis we broke this analysis down by domain instead of 3-way interaction
cat('\n');cat('\n'); print("TRANSFER RT BY CONDITION AND DOMAIN")
xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Baseline")
xferRTmod4 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "rock"))
print(summary(xferRTmod4))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Spontaneous")
xferRTmod5<- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "rock"))
print(summary(xferRTmod5))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Hint-aided")
xferRTmod6 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "rock"))
print(summary(xferRTmod6))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Baseline")
xferRTmod7 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "mobile"))
print(summary(xferRTmod7))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Spontaneous")
xferRTmod8<- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "mobile"))
print(summary(xferRTmod8))

xfer_dat_rt$task <- relevel(xfer_dat_rt$task, ref = "Hint-aided")
xferRTmod9 <- lmer(logRT ~ task*block +  (1|id) + (1|item), data = subset(xfer_dat_rt, domain == "mobile"))
print(summary(xferRTmod9))

if(write_output == TRUE){
	sink()
	setwd(home_dir)
}

## get predicted values from regression model
xRT_dat1 <- xfer_dat_rt
xRT_dat1$predRT <- predict(xferRTmod3, newdata = xRT_dat1)
xRT_dat1$domain <- "Combined"

xRT_dat2 <- subset(xfer_dat_rt, domain == "rock")
xRT_dat2$predRT <- predict(xferRTmod6, newdata = xRT_dat2)
xRT_dat2$domain <- "Rock Domain (TCL)"

xRT_dat3 <- subset(xfer_dat_rt, domain == "mobile")
xRT_dat3$predRT <- predict(xferRTmod9, newdata = xRT_dat3)
xRT_dat3$domain <- "Mobile Domain (TCL)"

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization for transfer RT data
## summarize combined data of predicted values
xRT_plot_dat1 <- ddply(xRT_dat1, .(task, block, domain), summarize,
 	meanRT = mean(predRT), semRT = sem(predRT))
## summarize rock domain data of predicted values
xRT_plot_dat2 <- ddply(xRT_dat2, .(task, block, domain), summarize,
 	meanRT = mean(predRT), semRT = sem(predRT))
## summarize mobile domain data of predicted values
xRT_plot_dat3 <- ddply(xRT_dat3, .(task, block, domain), summarize,
 	meanRT = mean(predRT), semRT = sem(predRT))

## merge combined data with domain data
xRT_plot_dat <- rbind(xRT_plot_dat1, xRT_plot_dat2, xRT_plot_dat3)

## create plot
xRT_plot <- ggplot(xRT_plot_dat, aes(x = block, y = meanRT, color = task))+
	facet_wrap(~domain, ncol = 3) +
	geom_line(size = 2)+
	geom_errorbar(aes(ymin = meanRT - semRT, ymax = meanRT + semRT), size = 1.5, width = 0.5)+
	scale_color_manual(values = c("#E69F00","#0072B2", "#D55E00")) +
		labs(y = "Mean Predicted Log RT \n", x = "TCL Block", color = "Condition") +
	theme(axis.text.x = element_text(face = "plain", color = "black", size = 12)) +
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5)) + 
	theme(axis.line = element_line(colour = "black"), 
		panel.border = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = 'bottom', legend.text = element_text(size = rel(1.8)), legend.title= element_text(size = rel(1.8)))+
	theme(axis.text.y = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.text.x = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.title.y = element_text(size = rel(2.5), angle = 90)) + 
	theme(strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold')) +
	theme(axis.title.x = element_text(size = rel(2.5), angle = 00))

if(write_output == TRUE){
	setwd(output_dir)
	ggsave('transfer response time plot.png',plot = xRT_plot,width = 12, height = 7, units = 'in')
}
setwd(home_dir)
