# Cluster analysis to determine profiles of learners during transfer

## Determine optimal number of clusters for each condition
### spontaneous data
Swcss <- vector()
for(i in 1:20) Swcss[i] <- sum(kmeans(spont_block_mean,i, nstart = 20)$withinss)
plot(1:20, Swcss,type = 'b', main = paste('Clusters'), xlab = 'Number of Clusters', ylab = 'WCSS')
#### interpreted as 5 clusters

### hint-aided data
Hwcss <- vector()
for(i in 1:20) Hwcss[i] <- sum(kmeans(hint_block_mean,i, nstart = 20)$withinss)
plot(1:20, Hwcss,type = 'b', main = paste('Clusters'), xlab = 'Number of Clusters', ylab = 'WCSS')
#### interpreted as 3 clusters

# # baseline data
Bwcss <- vector()
for(i in 1:20) Bwcss[i] <- sum(kmeans(base_block_mean,i, nstart = 20)$withinss)
plot(1:20, Bwcss,type = 'b', main = paste('Clusters'), xlab = 'Number of Clusters', ylab = 'WCSS')
#### interpreted as 3 clusters

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# fit kmeans to participant data
km_spont <- kmeans(spont_block_mean, centers = 5, nstart = 20)
km_hint <- kmeans(hint_block_mean, centers = 3, nstart = 20)
km_base <- kmeans(base_block_mean, centers = 3, nstart = 20)

## interpretation of profiles of clusters produced by kmeans
# Profile A: block 1-2 “near perfect accuracy” learners 
# Profile B: block 1-3 “near perfect accuracy” learners
# Profile C: gradual “decent” learners
# Profile D: gradual partial learners
# Profile E: chance 

# extract centroids (Note: order of profile names will need to be changed each run of the script)
spont_centroids <- data.frame(task = rep("Spontaneous", 25), 
	cluster = c(rep("Profile B", 5), rep("Profile A", 5),rep("Profile D", 5),rep("Profile E", 5),rep("Profile C", 5)), 
	block = rep(seq(1,5,1),5), 
	accuracy = c(km_spont$centers[1,],km_spont$centers[2,],km_spont$centers[3,],km_spont$centers[4,],km_spont$centers[5,])
	)

hint_centroids <- data.frame(task = rep("Hint-aided", 15), 
	cluster = c(rep("Profile C", 5), rep("Profile B", 5),rep("Profile E", 5)), 
	block = rep(seq(1,5,1),3), 
	accuracy = c(km_hint$centers[1,],km_hint$centers[2,],km_hint$centers[3,])
	)

base_centroids <- data.frame(task = rep("Baseline", 15), 
	cluster = c(rep("Profile B", 5), rep("Profile E", 5),rep("Profile C", 5)), 
	block = rep(seq(1,5,1),3), 
	accuracy = c(km_base$centers[1,],km_base$centers[2,],km_base$centers[3,])
	)

## combine centroids into single dataset for plotting
fitted_centroids <- rbind(spont_centroids, hint_centroids, base_centroids)

## save a copy of profiles produced by kmeans
if(write_output == TRUE){
	setwd(output_dir)
	write.csv(fitted_centroids, 'E1 Profiles of Learners.csv', row.names = FALSE)
	setwd(home_dir)
} 


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# visualization of clustering solution (profiles of learners)
cluster_plot <- ggplot(fitted_centroids, aes(x = block, y = accuracy, color = cluster))+
	facet_wrap(~task, ncol = 3)+
	geom_line(size = 2)+
	geom_hline(yintercept = .33, size = 1.5, linetype = 'dashed', color = "grey") +
	coord_cartesian(ylim=c(0.3, 1)) + 
    scale_y_continuous(breaks=seq(0.3, 1, .1)) +
	labs(x = "TCL Block", y = "TCL Accuracy", color = "Cluster")+
	scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73",  "#D55E00", "#CC79A7"))+
	theme_bw()+
	theme(
		legend.position = "bottom",
		legend.text = element_text(size = rel(1.5)), 
		legend.title= element_text(size = rel(2)),
		axis.text.y = element_text(size = rel(2), angle = 00),
		axis.text.x = element_text(size = rel(2), angle = 00),
		axis.title.y = element_text(size = rel(2.5), angle = 90),
		strip.text.x = element_text(size = rel(2.5), angle = 00, face = 'bold'),
		axis.title.x = element_text(size = rel(2.5), angle = 00),
		axis.line = element_line(colour = "black"))

if(write_output == TRUE){
	setwd(output_dir)
	ggsave('profiles of learners plot.png',plot = cluster_plot,width = 12, height = 7, units = 'in')
	setwd(home_dir)
}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# predict cluster assignment to each participant
spont_block_mean$pred <- predict_KMeans(spont_block_mean, km_spont$centers)
hint_block_mean$pred <- predict_KMeans(hint_block_mean, km_hint$centers)
base_block_mean$pred <- predict_KMeans(base_block_mean, km_base$centers)

## get frequicies of each cluster
table(spont_block_mean$pred)
table(hint_block_mean$pred)
table(base_block_mean$pred)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# check validity of clustering solution against individual learning curves, adjust number of clusters in algorithm if necessary
# # merge participant learning curves in the spontaneous condition with centroids produced by kmeans
spont_block_mean$id <- seq(1,nrow(spont_block_mean),1)
spont_block_mean$data <- "subject"
spont_subj <- data.frame(id = rep(spont_block_mean$id, 5), data = rep(spont_block_mean$data, 5),
	block = c(rep(1, nrow(spont_block_mean)), rep(2, nrow(spont_block_mean)),rep(3, nrow(spont_block_mean)),rep(4, nrow(spont_block_mean)),rep(5, nrow(spont_block_mean))),
	accuracy = c(spont_block_mean[,1], spont_block_mean[,2], spont_block_mean[,3],spont_block_mean[,4],spont_block_mean[,5]),
	cluster = rep(spont_block_mean$pred, 5))
spont_subj <- rbind(spont_subj,
	data.frame(id = NA, data = "clustering",
		block = c(rep(1, 5), rep(2, 5),rep(3, 5), rep(4, 5),rep(5, 5)),
		accuracy = c(km_spont$centers[,1], km_spont$centers[,2],km_spont$centers[,3],km_spont$centers[,4],km_spont$centers[,5]),
		cluster = c(rep(seq(1,5,1),5)))
	)
# # plot sponaneous data for visual inspection
ggplot(spont_subj, aes(x = block, y = accuracy))+
	facet_wrap(~ as.factor(cluster), ncol = 2)+
	geom_line(data = subset(spont_subj, data =="subject" ), aes(x = block, y = accuracy, color = as.factor(id)), linetype = "dashed", size = .75, alpha = .5)+
	geom_line(data = subset(spont_subj, data =="clustering" ), aes(x = block, y = accuracy), color = "black", size = 1.5)+
	scale_x_continuous(breaks = seq(1,3,1))+
	labs(x = "TCL block", y = "TCL accuracy")+
	theme_bw() +
	theme(legend.position = "none")

# # merge participant learning curves in the hint-aided condition with centroids produced by kmeans
hint_block_mean$id <- seq(1,nrow(hint_block_mean),1)
hint_block_mean$data <- "subject"
hint_subj <- data.frame(id = rep(hint_block_mean$id, 5), data = rep(hint_block_mean$data, 5),
	block = c(rep(1, nrow(hint_block_mean)), rep(2, nrow(hint_block_mean)),rep(3, nrow(hint_block_mean)),rep(4, nrow(hint_block_mean)),rep(5, nrow(hint_block_mean))),
	accuracy = c(hint_block_mean[,1], hint_block_mean[,2], hint_block_mean[,3],hint_block_mean[,4],hint_block_mean[,5]),
	cluster = rep(hint_block_mean$pred, 5))
hint_subj <- rbind(hint_subj,
	data.frame(id = NA, data = "clustering",
		block = c(rep(1, 3), rep(2, 3),rep(3, 3), rep(4, 3),rep(5, 3)),
		accuracy = c(km_hint$centers[,1], km_hint$centers[,2],km_hint$centers[,3],km_hint$centers[,4],km_hint$centers[,5]),
		cluster = c(rep(seq(1,3,1),5)))
	)
# # plot sponaneous data for visual inspection
ggplot(hint_subj, aes(x = block, y = accuracy))+
	facet_wrap(~ as.factor(cluster), ncol = 2)+
	geom_line(data = subset(hint_subj, data =="subject" ), aes(x = block, y = accuracy, color = as.factor(id)), linetype = "dashed", size = .75, alpha = .5)+
	geom_line(data = subset(hint_subj, data =="clustering" ), aes(x = block, y = accuracy), color = "black", size = 1.5)+
	scale_x_continuous(breaks = seq(1,3,1))+
	labs(x = "TCL block", y = "TCL accuracy")+
	theme_bw() +
	theme(legend.position = "none")

# # merge participant learning curves in the hint-aided condition with centroids produced by kmeans
base_block_mean$id <- seq(1,nrow(base_block_mean),1)
base_block_mean$data <- "subject"
base_subj <- data.frame(id = rep(base_block_mean$id, 5), data = rep(base_block_mean$data, 5),
	block = c(rep(1, nrow(base_block_mean)), rep(2, nrow(base_block_mean)),rep(3, nrow(base_block_mean)),rep(4, nrow(base_block_mean)),rep(5, nrow(base_block_mean))),
	accuracy = c(base_block_mean[,1], base_block_mean[,2], base_block_mean[,3],base_block_mean[,4],base_block_mean[,5]),
	cluster = rep(base_block_mean$pred, 5))
base_subj <- rbind(base_subj,
	data.frame(id = NA, data = "clustering",
		block = c(rep(1, 3), rep(2, 3),rep(3, 3), rep(4, 3),rep(5, 3)),
		accuracy = c(km_base$centers[,1], km_base$centers[,2],km_base$centers[,3],km_base$centers[,4],km_base$centers[,5]),
		cluster = c(rep(seq(1,3,1),5)))
	)

# # plot sponaneous data for visual inspection
ggplot(base_subj, aes(x = block, y = accuracy))+
	facet_wrap(~ as.factor(cluster), ncol = 2)+
	geom_line(data = subset(base_subj, data =="subject" ), aes(x = block, y = accuracy, color = as.factor(id)), linetype = "dashed", size = .75, alpha = .5)+
	geom_line(data = subset(base_subj, data =="clustering" ), aes(x = block, y = accuracy), color = "black", size = 1.5)+
	scale_x_continuous(breaks = seq(1,3,1))+
	labs(x = "TCL block", y = "TCL accuracy")+
	theme_bw() +
	theme(legend.position = "none")