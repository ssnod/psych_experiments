# Analyses and visualization for transfer (TCL phase) data

## save a copy of stats output as text file
if(write_output == TRUE){
	setwd(output_dir)
	sink('E1 Transfer Analysis Output.txt', split = TRUE)
} 

## condition by block analyses
cat('\n');cat('\n'); print("TRANSFER BY CONDITION")
xfer_dat$task <- relevel(xfer_dat$task, ref = 'Baseline')
xfer_mod1a <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = xfer_dat, family = binomial())
print(summary(xfer_mod1a))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Spontaneous')
xfer_mod1b <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = xfer_dat, family = binomial())
print(summary(xfer_mod1b))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Hint-aided')
xfer_mod1c<- glmer(accuracy ~ task*block + (1|id) + (1|item), data = xfer_dat, family = binomial())
print(summary(xfer_mod1c))

## adding domain to the previous model to test for differences in counterbalance order
# xfer_dat$task <- relevel(xfer_dat$task, ref = 'Baseline')
# xfer_dat$domain <- as.factor(xfer_dat$domain)
# xfer_dat$domain <- relevel(xfer_dat$domain, ref = 'mobile')
# xfer_mod2a <- glmer(accuracy ~ task*block*domain + (1|id) + (1|item), data = xfer_dat, family = binomial())
# summary(xfer_mod2a)

# xfer_dat$domain <- relevel(xfer_dat$domain, ref = 'rock')
# xfer_mod2b <- glmer(accuracy ~ task*block*domain + (1|id) + (1|item), data = xfer_dat, family = binomial())
# summary(xfer_mod2b)

### models fail to converge when experimental conditions (spontaneous and hint-aided) used as referents

## to address convergence problems we broke this analysis down by domain instead of 3-way interaction
cat('\n');cat('\n'); print("TRANSFER BY CONDITION AND DOMAIN")
xfer_dat$task <- relevel(xfer_dat$task, ref = 'Baseline')
xfer_mod5a <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'rock'), family = binomial())
print(summary(xfer_mod5a))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Spontaneous')
xfer_mod5b <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'rock'), family = binomial())
print(summary(xfer_mod5b))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Hint-aided')
xfer_mod5c<- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'rock'), family = binomial())
print(summary(xfer_mod5c))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Baseline')
xfer_mod6a <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'mobile'), family = binomial())
print(summary(xfer_mod6a))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Spontaneous')
xfer_mod6b <- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'mobile'), family = binomial())
print(summary(xfer_mod6b))

xfer_dat$task <- relevel(xfer_dat$task, ref = 'Hint-aided')
xfer_mod6c<- glmer(accuracy ~ task*block + (1|id) + (1|item), data = subset(xfer_dat, domain == 'mobile'), family = binomial())
print(summary(xfer_mod6c))

if(write_output == TRUE){
	sink()
	setwd(home_dir)
}


## get predicted values from regression model
xfer_dat <- xfer_dat
xfer_rock_dat <- subset(xfer_dat, domain == 'rock')
xfer_mobile_dat <- subset(xfer_dat, domain == 'mobile')
xfer_dat$predicted <- predict(xfer_mod1a, newdata = xfer_dat, type = 'response')
xfer_rock_dat$predicted <- predict(xfer_mod5a, newdata = xfer_rock_dat, type = 'response')
xfer_mobile_dat$predicted <- predict(xfer_mod6a, newdata = xfer_mobile_dat, type = 'response')

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization for transfer data
## summarize combined data w/ predicted values
xfer_plot_dat1 <- ddply(xfer_dat, .(task, block), summarize,
 	meanPred = mean(predicted), semPred = sem(predicted), meanAcc = mean(accuracy))
xfer_plot_dat1$domain <- "Combined"
## summarize rock domain data w/ predicted values
xfer_plot_dat2 <- ddply(xfer_rock_dat, .(task, block), summarize,
 	meanPred = mean(predicted), semPred = sem(predicted), meanAcc = mean(accuracy))
xfer_plot_dat2$domain <- "Rock Domain (TCL)"
## summarize mobile domain data w/ predicted values
xfer_plot_dat3 <- ddply(xfer_mobile_dat, .(task, block), summarize,
 	meanPred = mean(predicted), semPred = sem(predicted), meanAcc = mean(accuracy))
xfer_plot_dat3$domain <- "Mobile Domain (TCL)"

## merge combined data with domain data
xfer_plot_dat <- rbind(xfer_plot_dat1, xfer_plot_dat2, xfer_plot_dat3)
xfer_plot_dat$block2 <- xfer_plot_dat$block + .5

## create plot
xfer_plot <- ggplot(xfer_plot_dat, aes(x = block, y = meanPred, color = task))+
	facet_wrap(~domain, ncol = 3) +
	geom_line(size = 2)+
	geom_point(aes(x = block2, y = meanAcc), size = 5, alpha = .5) +
	geom_errorbar(aes(ymin = meanPred - semPred, ymax = meanPred + semPred), size = 1.5, width = 0.5)+
	scale_color_manual(values = c("#E69F00","#0072B2", "#D55E00")) +
	geom_hline(yintercept = .33, size = 1.5, linetype = 'dashed', color = "grey") +
	labs(y = "Accuracy \n", x = "TCL Block", color = " ",
		caption = "Error bars reflect +/- 1 SEM.\nRight offset points reflect unadjusted means.") +
	theme(axis.text.x = element_text(face = "plain", color = "black", size = 12)) +
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5)) + 
	theme(axis.line = element_line(colour = "black"), 
		panel.border = element_blank()) + 
	coord_cartesian(ylim=c(0.3, 1)) + 
    scale_y_continuous(breaks=seq(0.3, 1, .1)) +
    theme(legend.position = 'bottom', legend.text = element_text(size = rel(1.8)), legend.title= element_text(size = rel(1.8)))+
	theme(axis.text.y = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.text.x = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.title.y = element_text(size = rel(2.5), angle = 90)) + 
	theme(strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold')) +
	theme(axis.title.x = element_text(size = rel(2.5), angle = 00))

if(write_output == TRUE){
	setwd(output_dir)
	ggsave('transfer plot.png',plot = xfer_plot,width = 12, height = 7, units = 'in')
}
setwd(home_dir)