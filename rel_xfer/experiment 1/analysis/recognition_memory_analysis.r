# Analyses and visualization for recognition test data

## save a copy of stats output as text file
if(write_output == TRUE){
	setwd(output_dir)
	sink('E1 Recognition Test Output.txt', split = TRUE)
} 

## analysis of hit rate - agggegate followed by as a function of stimulus domain
cat('\n');cat('\n'); print("ANALYSIS OF HIT RATE")
rmod1a <- glmer(hit ~ priorPhase + (1|id) + (1|item), data = old_items, family = binomial())
print(summary(rmod1a))

mem_test$domain <- relevel(mem_test$domain, ref = "rock")
rmod1b <- glmer(hit ~ priorPhase*domain + (1|id) + (1|item), data = old_items, family = binomial())
print(summary(rmod1b))

mem_test$domain <- relevel(mem_test$domain, ref = "mobile")
rmod1c <- glmer(hit ~ priorPhase*domain + (1|id) + (1|item), data = old_items, family = binomial())
print(summary(rmod1c))

## analysis of false alarm rate - agggegate followed by as a function of stimulus domain
cat('\n');cat('\n'); print("ANALYSIS OF FALSE ALARM RATE")
rmod2a <- glmer(FA ~ priorPhase + (1|id) + (1|item), data = new_items, family = binomial())
print(summary(rmod2a))

mem_test$domain <- relevel(mem_test$domain, ref = "rock")
rmod2b <- glmer(FA ~ priorPhase*domain + (1|id) + (1|item), data = new_items, family = binomial())
print(summary(rmod2b))

mem_test$domain <- relevel(mem_test$domain, ref = "mobile")
rmod2c <- glmer(FA ~ priorPhase*domain + (1|id) + (1|item), data = new_items, family = binomial())
print(summary(rmod2c))

## analysis of sensitivity and bias
cat('\n');cat('\n'); print("D-PRIME ANALYSIS")
dprime_mod <- lm(dprime ~ priorPhase, data = mem_perf)
print(summary(dprime_mod))

bias_mod <- lm(bias ~ priorPhase, data = mem_perf)
print(summary(bias_mod))

if(write_output == TRUE){
	sink()
	setwd(home_dir)
}

## get predicted values from regression models
old_items$prediction <- predict(rmod1a, newdata = old_items, type = "response")
new_items$prediction <- predict(rmod2a, newdata = new_items, type = "response")

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization for recognition memory test

## summarize combined old item data with predicted values
rec_hit_sub_comb <- ddply(old_items, .(id, priorPhase), summarize,
	Pred = mean(prediction), Acc = mean(hit))
recog_hit_comb <- ddply(rec_hit_sub_comb, .(priorPhase), summarize,
	meanPred = mean(Pred), semPred = sem(Pred), meanAcc = mean(Acc))
recog_hit_comb$response <- 'Hit Rate'
recog_hit_comb$dom <- 'Combined Domains'

## summarize by domain old item data with predicted values
rec_hit_sub_dom <- ddply(old_items, .(id, priorPhase, domain), summarize,
	Pred = mean(prediction), Acc = mean(hit))
recog_hit_dom <- ddply(rec_hit_sub_dom, .(priorPhase, domain), summarize,
	meanPred = mean(Pred), semPred = sem(Pred), meanAcc = mean(Acc))
recog_hit_dom$response <- 'Hit Rate'
recog_hit_dom$dom <- c('Mobile Domain (BCL)', 'Rock Domain (BCL)','Mobile Domain (BCL)', 'Rock Domain (BCL)')

## summarize combined new item data with predicted values
recog_fa_sub_comb <- ddply(new_items, .(id, priorPhase), summarize,
	Pred = mean(prediction), Acc = mean(FA))
recog_fa_comb <- ddply(recog_fa_sub_comb, .(priorPhase), summarize,
	meanPred = mean(Pred), semPred = sem(Pred), meanAcc = mean(Acc))
recog_fa_comb$response <- 'False Alarm Rate'
recog_fa_comb$dom <- 'Combined Domains'

## summarize by domain new item data with predicted values
recog_fa_sub_dom <- ddply(new_items, .(id, priorPhase,domain), summarize,
	Pred = mean(prediction), Acc = mean(FA))
recog_fa_dom <- ddply(recog_fa_sub_dom, .(priorPhase,domain), summarize,
	meanPred = mean(Pred), semPred = sem(Pred), meanAcc = mean(Acc))
recog_fa_dom$response <- 'False Alarm Rate'
recog_fa_dom$dom <- c('Mobile Domain (BCL)', 'Rock Domain (BCL)','Mobile Domain (BCL)', 'Rock Domain (BCL)')

## recombine data for plotting
recog_plot_dat <- rbind(recog_hit_comb, recog_hit_dom[,-2], recog_fa_comb, recog_fa_dom[,-2])
recog_plot_dat$priorPhase <- as.factor(recog_plot_dat$priorPhase)
levels(recog_plot_dat$priorPhase) <- c("Category\nLearning\n", "Baseline")

## create plot
recog_plot <- ggplot(data = recog_plot_dat, aes(x = priorPhase, y = meanPred, fill = response))+
	facet_wrap(~dom, ncol = 3)+
	geom_bar(stat = "identity", position = position_dodge()) +
	geom_errorbar(aes(ymin = meanPred - semPred, ymax = meanPred + semPred), position = position_dodge(), size = 1.5)+
	geom_point(aes(x = priorPhase, y = meanAcc), position = position_dodge(.9), size = 6) +
	scale_fill_manual(values = c("#009E73","#E69F00")) +
	labs(y = "Proportion\n", x = "Prior Phase", fill = " ",
		caption = "Error bars reflect +/- 1 SEM.\nPoints reflect unadjusted means.") +
	theme(axis.text.x = element_text(face = "plain", color = "black", size = 12)) +
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5)) + 
	theme(axis.line = element_line(colour = "black"), 
		panel.border = element_blank()) + 
	coord_cartesian(ylim=c(0, 1)) + 
    scale_y_continuous(breaks=seq(0, 1, .2)) +
    theme(legend.position = 'bottom', legend.text = element_text(size = rel(1.8)), legend.title= element_text(size = rel(1.8)))+
	theme(axis.text.y = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.text.x = element_text(size = rel(2.5), angle = 00)) + 
	theme(axis.title.y = element_text(size = rel(2.5), angle = 90)) + 
	theme(strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold')) +
	theme(axis.title.x = element_text(size = rel(2.5), angle = 00))

## save plot	
if(write_output == TRUE){
	setwd(output_dir)
	ggsave('recognition test plot.png',plot = recog_plot,width = 12, height = 7, units = 'in')
}
setwd(home_dir)


