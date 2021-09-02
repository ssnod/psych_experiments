# Analyses and visualization for cross phase performance data

## save a copy of stats output as text file
if(write_output == TRUE){
	setwd(output_dir)
	sink('E1 Cross Phase Performance Analysis Output.txt', split = TRUE)
} 

## initial learning and recognition memory
cat('\n');cat('\n'); print("RECOGNITION PERFORMANCE BY FINAL BLOCK BCL ACCURACY")
### does BCL performance predict hit rate?
hit_by_bcl_mod <- glmer(hit ~ meanAcc + (1|id) + (1|item), data = oldXbcl , family = binomial())
print(summary(hit_by_bcl_mod))
### does BCL performance predict hit rate?
fa_by_bcl_mod <- glmer(FA ~ meanAcc + (1|id) + (1|item), data = newXbcl, family = binomial())
print(summary(fa_by_bcl_mod))

## initial learning and transfer (TCL)
cat('\n');cat('\n'); print("TCL PERFORMANCE BY FINAL BLOCK BCL ACCURACY")
tcl_by_bcl_spont_mod <- glmer(accuracy ~ finalBCLacc*block + (1|id) + (1|item), data = tclXbcl_spont, family = binomial())
print(summary(tcl_by_bcl_spont_mod))
tcl_by_bcl_hint_mod <- glmer(accuracy ~ finalBCLacc*block + (1|id) + (1|item), data = tclXbcl_hint, family = binomial())
print(summary(tcl_by_bcl_hint_mod))

## recognition memory and transfer (TCL)
cat('\n');cat('\n'); print("TCL PERFORMANCE BY RECOGNITION MEMORY HITS")
xfer_hit_spont_mod <- glmer(accuracy ~ propHit*block + (1|id) + (1|item), data = tclXmem_spont , family = binomial())
print(summary(xfer_hit_spont_mod))
xfer_hit_hint_mod <- glmer(accuracy ~ propHit*block + (1|id) + (1|item), data = tclXmem_hint, family = binomial())
print(summary(xfer_hit_hint_mod))
xfer_hit_base_mod <- glmer(accuracy ~ propHit*block + (1|id) + (1|item), data = tclXmem_base , family = binomial())
print(summary(xfer_hit_base_mod))
cat('\n');cat('\n'); print("TCL PERFORMANCE BY RECOGNITION MEMORY FALSE ALARMS")
xfer_fa_spont_mod <- glmer(accuracy ~ propFA*block + (1|id) + (1|item), data = tclXmem_spont , family = binomial())
print(summary(xfer_fa_spont_mod))
xfer_fa_hint_mod <- glmer(accuracy ~ propFA*block + (1|id) + (1|item), data = tclXmem_hint, family = binomial())
print(summary(xfer_fa_hint_mod))
xfer_fa_base_mod <- glmer(accuracy ~ propFA*block + (1|id) + (1|item), data = tclXmem_base , family = binomial())
print(summary(xfer_fa_base_mod))

if(write_output == TRUE){
	sink()
	setwd(home_dir)
}

## get predicted values from regression model

### initial learning and recognition memory
oldXbcl$predicted <- predict(hit_by_bcl_mod, newdata = oldXbcl, type = 'response')
newXbcl$predicted <- predict(fa_by_bcl_mod, newdata = newXbcl, type = 'response')

### initial learning and transfer (TCL)
tclXbcl_spont$predicted <- predict(tcl_by_bcl_spont_mod, newdata = tclXbcl_spont, type = 'response')
tclXbcl_hint$predicted <- predict(tcl_by_bcl_hint_mod, newdata = tclXbcl_hint, type = 'response')

### recognition memory and transfer(TCL)
#### hit rate
tclXmem_spont$hitPred <- predict(xfer_hit_spont_mod, newdata = tclXmem_spont, type = 'response')
tclXmem_hint$hitPred <- predict(xfer_hit_hint_mod, newdata = tclXmem_hint, type = 'response') 
tclXmem_base$hitPred <- predict(xfer_hit_base_mod, newdata = tclXmem_base, type = 'response')
#### false alarm rate
tclXmem_spont$faPred <- predict(xfer_fa_spont_mod, newdata = tclXmem_spont, type = 'response')
tclXmem_hint$faPred <- predict(xfer_fa_hint_mod, newdata = tclXmem_hint, type = 'response')
tclXmem_base$faPred <- predict(xfer_fa_base_mod, newdata = tclXmem_base, type = 'response')

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization for cross phases analyses

## prep data for initial learning and recognition memory plot
oldXbcl$measure <- "Hits"
newXbcl$measure <- "False Alarms"
recogXbcl_plot_dat <- rbind(oldXbcl, newXbcl)

## prep data for initial learning and transfer (TCL) plot
xferXbcl_plot_dat <- rbind(tclXbcl_spont, tclXbcl_hint)
xferXbcl_plot_dat$task <- relevel(xferXbcl_plot_dat$task, ref = "Spontaneous")

## prep data for recognition memory and transfer (TCL) plot 
xfer_mem_plot_dat <- rbind(tclXmem_spont, tclXmem_hint, tclXmem_base)

## create initial learning and recognition memory plot
recogXbcl_plot <- ggplot(recogXbcl_plot_dat, aes(x = meanAcc, y = predicted)) +
	facet_wrap(~ measure, ncol = 2) +
	geom_point(position = position_jitter(width = 0.02, height = 0.02), color = 'dodgerblue3', alpha = .8)+
	coord_cartesian(ylim=c(0, 1), xlim = c(0,1.1)) + 
    scale_x_continuous(breaks=seq(0, 1, .25)) +
    scale_y_continuous(breaks=seq(0, 1, .25)) +
	labs(x = "Final BCL Block Accuracy", y = "Predicted Probability")+
	theme_bw() +
	theme(
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00),
		axis.line = element_line(colour = "black"), 
		panel.border = element_blank())

## create initial learning and transfer (TCL) plot
xferXbcl_plot <- ggplot(xferXbcl_plot_dat, aes(x = block, y = predicted, color = finalBCLacc))+
	facet_wrap(~ task, ncol = 2) +
	geom_point(position = position_jitter(width = 0.4, height = 0.02), alpha = .4)+
	coord_cartesian(ylim=c(0, 1), xlim = c(0.5,5.5)) + 
    scale_y_continuous(breaks=seq(0, 1, .25)) +
    scale_x_continuous(breaks=seq(1, 5, 1)) +
	labs(x = "TCL Block", y = "Predicted Trial-wise Accuracy", color = "Final BCL Block\nAccuracy")+
	theme_bw() +
	theme(
		legend.position = "bottom",
		legend.text = element_text(size = rel(1)), 
		legend.title= element_text(size = rel(2)),
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00),
		axis.line = element_line(colour = "black"), 
		panel.border = element_blank())

## create recognition memory hit and transfer (TCL) plot
xfer_hit_plot <- ggplot(xfer_mem_plot_dat, aes(x = block, y = hitPred, color = propHit))+
	facet_wrap(~ task, ncol = 3) +
	geom_point(position = position_jitter(width = 0.4, height = 0.02), alpha = .4, size = .9)+
	coord_cartesian(ylim=c(0, 1), xlim = c(0.5,5.5)) + 
    scale_y_continuous(breaks=seq(0, 1, .25)) +
    scale_x_continuous(breaks=seq(1, 5, 1)) +
	labs(x = "TCL Block", y = "Predicted Trial-wise Accuracy", color = "Hit Rate")+
	theme_bw() +
	theme(
		legend.position = "bottom",
		legend.text = element_text(size = rel(1)), 
		legend.title= element_text(size = rel(2)),
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00),
		axis.line = element_line(colour = "black"), 
		panel.border = element_blank())

## create recognition memory false alarm and transfer (TCL) plot
xfer_fa_plot <- ggplot(xfer_mem_plot_dat, aes(x = block, y = faPred, color = propFA))+
	facet_wrap(~ task, ncol = 3) +
	geom_point(position = position_jitter(width = 0.4, height = 0.02), alpha = .4, size = .9)+
	coord_cartesian(ylim=c(0, 1), xlim = c(0.5,5.5)) + 
    scale_y_continuous(breaks=seq(0, 1, .25)) +
    scale_x_continuous(breaks=seq(1, 5, 1)) +
    scale_color_continuous(breaks = seq(0,1,.25), labels = c("0", "0.25", "0.5", "0.75", "1")) +
	labs(x = "TCL Block", y = "Predicted Trial-wise Accuracy", color = "False Alarm Rate")+
	theme_bw() +
	theme(
		legend.position = "bottom",
		legend.text = element_text(size = rel(1)), 
		legend.title= element_text(size = rel(2)),
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00),
		axis.line = element_line(colour = "black"), 
		panel.border = element_blank())

setwd(vizDir)

setwd(homeDir)


## save plots
if(write_output == TRUE){
	setwd(output_dir)
	ggsave('recognition by initial learning plot.png',plot = recogXbcl_plot,width = 12, height = 7, units = 'in')
	ggsave('transfer by initial learning plot.png',plot = xferXbcl_plot,width = 12, height = 7, units = 'in')
	ggsave('transfer by recognition hits plot.png',plot = xfer_hit_plot,width = 12, height = 7, units = 'in')
	ggsave('transfer by recognition false alarm plot.png',plot = xfer_fa_plot,width = 12, height = 7, units = 'in')
}
setwd(home_dir)
