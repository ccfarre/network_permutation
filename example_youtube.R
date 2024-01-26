
library(igraph)
library(corpustools)
library(readr)
library(ggraph)
library(plyr)

#Load dataset
setwd("~/Dropbox/Youtube/Network")
videos <- read_csv("classified_videos_proportional_Dataset5_top152022-01-28_18_36_39.csv")

# Remove NAs in political bias
videos <- videos[complete.cases(videos[,"new_bias"]),]
videos <- videos[sample(nrow(videos), 3000), ]

origin_destination <- videos[,3:4]
origin_destination <- origin_destination[,c(2,1)]
colnames(origin_destination) <- c("source", "target")
origin_destination$con <- paste(origin_destination$source, origin_destination$target)

od <- count(origin_destination$con)
# od <- dplyr:::count(origin_destination, con)
colnames(od) <- c("x", "freq")
origin_destination$weight <- od$freq[match(origin_destination$con, od$x)]

od2 <- unique(origin_destination)
od2 <- od2[,-3]
od3 <- od2[od2$weight > 2,] #Keep only strong connections, explain why 2

network <- graph_from_data_frame(d=od3, directed=T) 

edge_density(network)
reciprocity(network)
diameter(network, directed = TRUE, unconnected = TRUE, weights = NULL)
mean_distance(network, directed = TRUE, unconnected = TRUE)
transitivity(network, type="global")
mean(transitivity(network, type="local"), na.rm=TRUE)

V(network)$category <- videos$new_bias[match(V(network)$name, videos$video)]

network2 = simplify(network, remove.multiple = TRUE, remove.loops = TRUE)
V(network2)$category <- videos$new_bias[match(V(network2)$name, videos$video)]

ggraph(network2, layout = "linear", circular = TRUE) +
  geom_edge_link(aes(), edge_colour = "black", edge_alpha = .2, arrow = arrow(angle = 30, length = unit(0.05, "inches"),
                                                            ends = "last", type = "closed") ) +
  scale_edge_width(range = c(0.1, 1.5))+
  geom_node_point(aes(color = category)) +
  scale_color_manual(values = c("C" = "grey",
                                "CL"="#3399FF",
                                "L"="#000099", "R" = "#990000", "CR" = "#FF0000")) +
  theme(legend.title=element_blank(), legend.position="top", panel.background = element_rect(fill = "white"), plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))

##### TEST WITH NULL MODELS
library(janitor)
library(dplyr)
library(econullnetr)

od2$s_cat <- videos$new_bias[match(od2$source, videos$video)]
od2$t_cat <- videos$new_bias[match(od2$target, videos$video)]
od2 <- od2[complete.cases(od2),]

agg <- aggregate(weight ~ s_cat + t_cat, data= od2, FUN=sum)
aggnetwork <- graph_from_data_frame(d=agg, directed=T) 
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ggraph(aggnetwork, layout = "nicely") +
  geom_edge_link(aes(alpha = range01(weight), width = range01(weight)), edge_colour = "grey") +
  scale_edge_width(range = c(0.1, 3))+
  geom_node_point(aes(color = name, size = 3)) +
  scale_color_manual(values = c("C" = "grey",
                                "CL"="#3399FF",
                                "L"="#000099", "R" = "#990000", "CR" = "#FF0000")) +
  theme(legend.title=element_blank(), legend.position="top", panel.background = element_rect(fill = "white"), plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))

count <- count(od2$t_cat)
# count <- od2 %>% group_by(t_cat) %>% summarise(number = n())
count <- as.data.frame(t(count))
count <- count %>% row_to_names(row_number = 1)
count[] <- sapply(count, as.numeric)
colnames(count) <- c("C", "CL", "CR", "L", "R")


mat <- as_long_data_frame(aggnetwork)
colnames(mat) <- c("from", "to", "weight", "name1", "name2")
mat <- as.data.frame.matrix(xtabs(weight ~ name1 + name2, mat))
mat$source <- row.names(mat)
mat <- mat %>% select(source, everything())

null_net <- generate_null_net(mat[, 1:6], count,
                          sims = 100, data.type = "counts", 
                          summary.type = "sum", prog.count = F)

op <- par(mfrow = c(2, 3))
plot_preferences(null_net, "L", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of recommendations", p.cex = 1.5, l.cex = 0.9, lwd = 2,
                 xlim = c(0,120000))
plot_preferences(null_net, "CL", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of recommendations", p.cex = 1.5, l.cex = 0.9, lwd = 2,
                 xlim = c(0,120000))
plot_preferences(null_net, "C", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of recommendations", p.cex = 1.5, l.cex = 0.9, lwd = 2,
                 xlim = c(-1,120000))
plot_preferences(null_net, "CR", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of recommendations", p.cex = 1.5, l.cex = 0.9, lwd = 2,
                 xlim = c(0,120000))
plot_preferences(null_net, "R", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of recommendations", p.cex = 1.5, l.cex = 0.9, lwd = 2,
                 xlim = c(0,120000))

results <- as.data.frame(null_net[['rand.data']])
#Next: calculate confidence intervals

sil.links <- test_interactions(null_net, signif.level = 0.95)

# Option 1
ggplot(sil.links, aes(x = Resource, y = Observed)) +
  geom_errorbar(aes(ymin = Lower.95.CL, ymax = Upper.95.CL), alpha = 0.6) +
  geom_point(size = 2, color = "red") + 
  theme_minimal() +
  facet_wrap(~Consumer, ncol = 1, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5))

# Option 2
sil.links$col <- "red"
sil.links[sil.links$Observed > sil.links$Upper.95.CL , "col"] <- "green"
sil.links[sil.links$Observed < sil.links$Lower.95.CL , "col"] <- "blue"

ggplot(sil.links, aes(x = Resource, y = Observed, colour = col)) +
  geom_errorbar(aes(ymin = Lower.95.CL, ymax = Upper.95.CL), alpha = 0.6) +
  geom_point(aes(size = 2), size = 2, show.legend = F) + 
  theme_minimal() +
  scale_color_hue(labels=c("red" = "As Expected", "green"="Above Expected", "blue"="Below Expected")) +
  facet_wrap(~Consumer, ncol = 1, scales = "free") +
  theme(legend.title=element_blank(), legend.position="top")

# Plots to compare
agg2 <- aggregate(weight ~ t_cat, data= od2, FUN=sum)
agg3 <- aggregate(weight ~ s_cat, data= od2, FUN=sum)

ggplot(agg2, aes(x = reorder(t_cat, -weight), y = weight, fill = t_cat)) + 
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) + 
  ylab("Number of times recmomended") +
  xlab("Political Bias") +
  theme_minimal()

aggnetwork2 <- graph_from_data_frame(d=sil.links, directed=T) 

ggraph(aggnetwork2, layout = "graphopt") +
  geom_edge_fan(aes(label = round(SES, 2), alpha = .8,
                width = 2, colour = Test), angle_calc = 'along',
                label_dodge = unit(2.5, 'mm'), label_push = unit(2.5, 'mm'),
                arrow = arrow(angle = 10, length = unit(3, "mm"),
                type = "closed"), end_cap = circle(4, 'mm')) + 
  scale_edge_width(range = c(0.3, 1.2)) + 
  scale_color_manual(values = c("C" = "grey", "CL"="#3399FF", "L"="#000099", 
                                "R" = "#990000", "CR" = "#FF0000"), 
                     labels = c("Center", "Center-Left", "Left", "Right", "Center-Right")) +
  scale_edge_color_manual(values = c("ns" = "grey", "Weaker"="#FC7171","Stronger"="#08BC02"),
                          labels = c("Not Significant", "Weaker", "Stronger")) +
  geom_node_point(aes(color = name, size = 5)) +
  labs(color = "Political Bias") +
  guides(edge_width = "none", edge_alpha = "none", size = "none") +
  theme(panel.background = element_rect(fill = "white")) +
  theme_void() 

sil.links$name <- paste(sil.links$Consumer, sil.links$Resource, sep = " to ")

long.res <- sil.links %>%
  gather(Observed, Null, Lower.95.CL, Upper.95.CL, -name)

ggplot(long.res, aes(x = name, y = Observed, fill = Null)) +
  geom_col(position = "dodge")

ggplot(sil.links, aes(x=Observed, y=Null, fill=factor(Consumer))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Lower.95.CL, ymax=Upper.95.CL), width=.2,
                position=position_dodge(.9)) 

#### CHECK EXTRA THINGS

video_stats <- videos
n_videos <- video_stats %>% group_by(video) %>% summarise(number = n())
video_stats$n_rec <- n_videos$number[match(video_stats$video, n_videos$video)]


ggplot(video_stats, aes(x=log(likes), y=log(n_rec))) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(video_stats, aes(x=log(dislikes), y=log(n_rec))) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(video_stats, aes(x=log(lm_negative), y=log(n_rec))) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(video_stats, aes(x=log(nrc_el_fear), y=log(n_rec))) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(video_stats, aes(x=log(nrc_el_positive), y=log(n_rec))) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(video_stats, aes(fill=new_bias, y=nrc_el_positive, x=new_bias)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal()

# Average depth of videos
av_depth <- video_stats %>% group_by(video) %>% summarise(mean_depth = mean(current_depth))
av_depth$likes <- video_stats$likes[match(av_depth$video, video_stats$video)]

ggplot(av_depth, aes(x=log(likes), y=mean_depth)) +
  geom_point() +
  theme(
    legend.position='none'
  )

# Ordinal logistic regression
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

# The next model does not work unless you delete all NAs in variables (complete cases)
m <- polr(as.factor(current_depth) ~ range01(nrc_el_pct) + range01(nrc_el_anger_pct) + range01(nrc_el_anticipation_pct) +
            range01(nrc_el_disgust_pct) + range01(nrc_el_fear_pct) + range01(nrc_el_joy_pct) + range01(nrc_el_negative_pct) +
            range01(nrc_el_positive_pct) + range01(nrc_el_sadness_pct) + range01(nrc_el_surprise_pct) +
            range01(nrc_el_trust_pct) + range01(difftime), data = videos2, Hess=TRUE)

summary(m)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # Calculate p values
(ctable <- cbind(ctable, "p value" = p))

ci_def <- as.data.frame(confint.default(m)) # CIs assuming normality
colnames(ci_def) <- c("low", "high")
ci_def$av <- (ci_def$low + ci_def$high) / 2
ci_def <- rownames_to_column(ci_def, var = "variable")

ggplot(ci_def, aes(variable, av)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high)) +
  coord_flip() +
  theme_minimal()

# TEST

m <- polr(as.factor(new_bias) ~ range01(nrc_el_pct) + range01(nrc_el_anger_pct) + range01(nrc_el_anticipation_pct) +
            range01(nrc_el_disgust_pct) + range01(nrc_el_fear_pct) + range01(nrc_el_joy_pct) + range01(nrc_el_negative_pct) +
            range01(nrc_el_positive_pct) + range01(nrc_el_sadness_pct) + range01(nrc_el_surprise_pct) +
            range01(nrc_el_trust_pct) + range01(difftime), data = videos2, Hess=TRUE)

summary(m)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # Calculate p values
(ctable <- cbind(ctable, "p value" = p))

ci_def <- as.data.frame(confint.default(m)) # CIs assuming normality
colnames(ci_def) <- c("low", "high")
ci_def$av <- (ci_def$low + ci_def$high) / 2
ci_def <- rownames_to_column(ci_def, var = "variable")

ggplot(ci_def, aes(variable, av)) +        # ggplot2 plot with confidence intervals
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high)) +
  coord_flip() +
  theme_minimal()

# Multinominal logistic regression
mlr <- select(videos, new_bias, nrc_el_pct , nrc_el_anger_pct , nrc_el_anticipation_pct ,
              nrc_el_disgust_pct , nrc_el_fear_pct , nrc_el_joy_pct , nrc_el_negative_pct ,
              nrc_el_positive_pct , nrc_el_sadness_pct , nrc_el_surprise_pct ,
              nrc_el_trust_pct , lm_uncertainty_pct , lm_litigious_pct, lm_constraining_pct, 
              lm_superfluous_pct, lm_interesting_pct, difftime)

mlr$type2 <- relevel(factor(mlr$new_bias), ref = "C")

# Run the model
# library(nnet)
results_mlr <- multinom(type2 ~ nrc_el_pct + nrc_el_anger_pct + nrc_el_anticipation_pct +
                          nrc_el_disgust_pct + nrc_el_fear_pct + nrc_el_joy_pct + nrc_el_negative_pct +
                          nrc_el_positive_pct + nrc_el_sadness_pct + nrc_el_surprise_pct +
                          nrc_el_trust_pct + lm_uncertainty_pct + lm_litigious_pct + lm_constraining_pct + 
                          lm_superfluous_pct + lm_interesting_pct + difftime, data = mlr, Hess = TRUE)
summary(results_mlr)

tt <- broom::tidy(results_mlr,conf.int=TRUE)
ci <- as.data.frame(confint(results_mlr, level=0.95))

# Plot

ggplot(tt, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high))  +
  geom_hline(yintercept=0, linetype=2)+
  geom_pointrange(aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, 
                      group=y.level, color=y.level), 
                  position = position_dodge(width = 0.6), size=1) +
  labs(
    x = "",
    y = "Estimate",
    colour = "Category") +
  coord_flip() +
  theme_minimal()





