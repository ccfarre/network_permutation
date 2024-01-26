



##### SUM NOT MAINTAINING DEGREE


# Generate data
source <- c("Sales", "Marketing", "Finance", "IT", "HR", "Legal", "Operations")
Sales <- c(869, 2173, 1008, 1165, 2497, 139, 342)
Marketing <- c(1065, 2959, 1576, 275, 2516, 0, 829)
Finance <- c(1269, 4385, 1362, 1754, 3601, 0, 877)
IT <- c(561, 669, 712, 1105, 780, 0, 156)
HR <- c(1004, 2746, 1822, 0, 4136, 527, 663)
Legal <- c(9, 0, 0, 0, 27, 0, 0)
Operations <- c(602, 1204, 602, 0, 903, 0, 301)

emails <- data.frame(source, Sales, Marketing, Finance, IT, HR, Legal, Operations)

Sales <- 24
Marketing <- 54
Finance <- 32
IT <- 20
HR <- 62
Legal <- 3
Operations <- 12

employees <- data.frame(Sales, Marketing, Finance, IT, HR, Legal, Operations)

# Create the network
# Convert dataframe to matrix
mat <- as.matrix(emails[,2:8])

# Create graph object
g <- graph_from_adjacency_matrix(mat, weighted = TRUE, mode = "directed")

# Rename vertices with Origin values
V(g)$department <- emails[,1]

cbind(
  nodes    = vcount(g),
  nedges  = ecount(g),
  density = edge_density(g),
  recip   = reciprocity(g),
  betweenness  = centr_betw(g)$centralization,
  path_length = mean_distance(g)
)

# calculate the PageRank score for each node
pr <- page_rank(g, directed = TRUE, weights = E(g)$weight)$vector

# identify the most central node
most_central <- which.max(pr)

# print the PageRank score for each node
print(pr)

# print the most central node
cat("The most central node is:", V(g)[most_central]$name)

# Plot the original network

ggraph(g, layout = "circle") +
  geom_node_point(aes(shape = as.factor(department), size = 10)) +
  geom_edge_fan(aes(alpha = 0.2, width = weight/2), linejoin = "bevel" ,
                arrow = arrow(angle = 40, length = unit(1, "mm"),
                              type = "open"), start_cap = circle(2, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_width(range = c(0.3, 2)) + 
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9)) + 
  labs(edge_width = "N of emails") +
  guides(edge_alpha = "none", size = "none", shape = guide_legend(title = "Department")) +
  theme_graph()




# Prepare the simulations
sims = 300 # Number of simulations

# Set up a data frame that summarises the Origin data, containing:
#   1. The original order of the data; 2. Origin units; 3. Number of
#      interactions per individual and total interactions (sum)

c.summary <- data.frame(ord = seq(1, nrow(emails)),
                        sample = rep("A", nrow(emails)),
                        units = emails[, 1],
                        links = rowSums(emails[, 2:ncol(emails)] != 0),
                        total = rowSums(emails[, 2:ncol(emails)]))

# Create data frame 'rd' by adding lower units of analysis
rd <- cbind.data.frame(rep("A", nrow(employees)), employees)
colnames(rd)[1] <- "r.sample"

# Add in the summary data for individual Origins ('c.summary')
rd2 <- merge(c.summary, rd, by.x = "sample", by.y = "r.sample")
rd2 <- rd2[order(rd2$ord), ]

# Create a data frame to contain the simulated data
res.df <- rd2[, 3:ncol(rd2)]
res.df[, 4:ncol(res.df)] <- 0

for (h in 1:sims){
  
  # Create a data frame to contain the simulated data
  res.df <- rd2[, 3:ncol(rd2)]
  res.df[, 4:ncol(res.df)] <- 0
  
  # Not mantaining degree
  for (i in 1:nrow(res.df)) {
  res.choice <- sample(colnames(rd2[, 6:ncol(rd2)]), size = rd2$total[i],
                       prob = rd2[i, 6:ncol(rd2)], replace = TRUE)
  pc <- data.frame(table(res.choice))
  rownames(pc) <- pc$res.choice
  res.df[i, rownames(pc)] <- pc[rownames(pc), 2]

# Prepare output.sum.not.deg from the iteration of the model (iterations indexed by
#   the value of h)

# Sum
  res.agg <- stats::aggregate(res.df[, 4:ncol(res.df)],
                              by = list(res.df$units), sum)
  colnames(res.agg)[1] <- "Origin"
  res.agg <- cbind(Iteration = as.matrix(rep(paste("It.", h, sep=""),
                                             length(res.agg[, 1]))), res.agg)
  ifelse(h == 1, res.compiled <- res.agg, {
    res.compiled <- rbind(res.compiled, res.agg)})
  }
}

# Compile output.sum.not.deg objects:
# 1. Observed Origin-Destination interactions
# 2. Null model output.sum.not.degs

# Sum

obs.interactions <- stats::aggregate(emails[, 2:ncol(emails)],
                                     list(emails[, 1]), sum)
colnames(obs.interactions)[1] <- "Origin"
null.interactions <- stats::aggregate(res.compiled[, 3:ncol(res.compiled)],
                                      list(res.compiled$Origin), mean)
colnames(null.interactions)[1] <- "Origin"

# Create output.sum.not.deg list
null.res.sum.not.deg <- list(rand.data = res.compiled, obs.interactions = obs.interactions,
                         n.iterations = sims)


# Set significance level
signif.level = 0.95

p <- (1 - signif.level) / 2


# Summarise modelled interactions
null.mean <- stats::aggregate(null.res.sum.not.deg$rand.data[, 3:ncol(null.res.sum.not.deg$rand.data)],
                              list(null.res.sum.not.deg$rand.data$Origin), mean)
null.upp <- stats::aggregate(null.res.sum.not.deg$rand.data[, 3:ncol(null.res.sum.not.deg$rand.data)],
                             list(null.res.sum.not.deg$rand.data$Origin), stats::quantile,
                             probs = 1 - p)
null.low <- stats::aggregate(null.res.sum.not.deg$rand.data[, 3:ncol(null.res.sum.not.deg$rand.data)],
                             list(null.res.sum.not.deg$rand.data$Origin), stats::quantile,
                             probs = p)
sd.inters <- stats::aggregate(null.res.sum.not.deg$rand.data[, 3:ncol(null.res.sum.not.deg$rand.data)],
                              list(null.res.sum.not.deg$rand.data$Origin), stats::sd)

# Convert to long table format using 'melt'
obs.inters <- reshape2::melt(null.res.sum.not.deg$obs.interactions, id.vars = "Origin")
colnames(obs.inters)[2] <- "Destination"
null.mean <- reshape2::melt(null.mean, id.vars = "Group.1")
colnames(null.mean)[c(1, 2)] <- c("Origin", "Destination")
null.low <- reshape2::melt(null.low, id.vars = "Group.1")
colnames(null.low)[c(1, 2)] <- c("Origin", "Destination")
null.upp <- reshape2::melt(null.upp, id.vars = "Group.1")
colnames(null.upp)[c(1, 2)] <- c("Origin", "Destination")
sd.inters <- reshape2::melt(sd.inters, id.vars = "Group.1")
colnames(sd.inters)[c(1, 2)] <- c("Origin", "Destination")


# Merge the observed interaction strengths with those from the null model,
#   the upper and lower confidence limits and standard deviation
output.sum.not.deg <- merge(obs.inters, null.mean, by.x = c("Origin", "Destination"),
                by.y = c("Origin", "Destination"))
colnames(output.sum.not.deg)[c(3,4)]<-c("Observed", "Null")
output.sum.not.deg <- merge(output.sum.not.deg, null.low, by.x = c("Origin", "Destination"),
                by.y = c("Origin", "Destination"))
colnames(output.sum.not.deg)[5] <- paste('Lower.', signif.level * 100, '.CL', sep = "")
output.sum.not.deg <- merge(output.sum.not.deg, null.upp, by.x = c("Origin", "Destination"),
                by.y = c("Origin", "Destination"))
colnames(output.sum.not.deg)[6] <- paste('Upper.', signif.level * 100, '.CL', sep = "")
output.sum.not.deg <- merge(output.sum.not.deg, sd.inters, by.x = c("Origin", "Destination"),
                by.y = c("Origin", "Destination"))
colnames(output.sum.not.deg)[7] <- "sd"

# Test significance, comparing observed link strength to the CLs
output.sum.not.deg$Test <- as.character(rep("Not Significant"))
for (i in 1:nrow(output.sum.not.deg)) {
  if (output.sum.not.deg$Observed[i] > output.sum.not.deg$Upper[i]) {output.sum.not.deg$Test[i] <- "Above Expected"}
  if (output.sum.not.deg$Observed[i] < output.sum.not.deg$Lower[i]) {output.sum.not.deg$Test[i] <- "Below Expected"}
}

# Calculate the standard effect sizes for observed v. null interactions
output.sum.not.deg$SES <- with(output.sum.not.deg, (Observed - Null) / sd)
output.sum.not.deg$SES[!is.finite(output.sum.not.deg$SES)] <- NA # Replaces Inf and NaN with NA

# Visualize results
ggplot(output.sum.not.deg, aes(x = Destination, y = Observed)) +
  geom_errorbar(aes(ymin = Lower.95.CL, ymax = Upper.95.CL), alpha = 0.6) +
  geom_point(size = 2, shape = as.factor(output.sum.not.deg$Test)) + 
  theme_minimal() +
  facet_wrap(~Origin, ncol = 1, scales = "free") +
  scale_y_continuous(breaks = c(0, 2000, 4000), limits = c(-500, max(output.sum.deg$Observed) + 500), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

output.sum.not.deg$col <- "red"
output.sum.not.deg[output.sum.not.deg$Observed > output.sum.not.deg$Upper.95.CL , "col"] <- "green"
output.sum.not.deg[output.sum.not.deg$Observed < output.sum.not.deg$Lower.95.CL , "col"] <- "blue"

ggplot(output.sum.not.deg, aes(x = Destination, y = Observed, colour = col)) +
  geom_errorbar(aes(ymin = Lower.95.CL, ymax = Upper.95.CL), alpha = 0.6) +
  geom_point(aes(size = .1), size = 2, show.legend = F) + 
  theme_minimal() +
  scale_color_hue(labels=c("red" = "As Expected", "green"="Above Expected", "blue"="Below Expected")) +
  facet_wrap(~Origin, ncol = 1, scales = "free") +
  theme(legend.title=element_blank(), legend.position="top")

# Visualize network with only significant links
aggnetwork2 <- graph_from_data_frame(d=output.sum.not.deg, directed=T) 
clean_net <- subgraph.edges(aggnetwork2, E(aggnetwork2)[Test == "Above Expected" | Test == "Below Expected"])

ggraph(clean_net, layout = "circle") +
  geom_node_point(aes(shape = as.factor(name), size = 10)) +
  geom_edge_fan(aes(alpha = 0.2, width = Observed), linejoin = "bevel" ,
                arrow = arrow(angle = 40, length = unit(1, "mm"),
                              type = "open"), start_cap = circle(2, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_width(range = c(0.3, 2)) + 
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9)) + 
  labs(edge_width = "N of emails") +
  guides(edge_alpha = "none", size = "none", shape = guide_legend(title = "Department")) +
  theme_graph()

# Why is this important? For community detection
com_new <- cluster_optimal(clean_net)
com_old <- cluster_optimal(g)

com_new$modularity
com_old$modularity

V(clean_net)$cluster <- com_new$membership
V(g)$cluster_old <- com_old$membership

ggraph(g, layout = "circle") +
  geom_node_point(aes(size = 10, alpha = 0.01), shape =c(0, 1, 2, 5, 6, 7, 9)) +
  geom_edge_fan(aes(width = weight/2), alpha = 0.1, linejoin = "bevel" ,
                arrow = arrow(angle = 40, length = unit(1, "mm"),
                              type = "open"), start_cap = circle(2, 'mm'), end_cap = circle(3, 'mm')) +
  scale_edge_width(range = c(0.3, 2)) + 
  ggforce::geom_mark_rect(aes(x,y,linetype=as.factor(cluster_old), col = as.factor(cluster_old))) +
  labs(linetype = "Cluster") +
  guides(edge_width = "none", edge_alpha = "none", color = "none", alpha = "none", size = "none") +
  theme(panel.background = element_rect(fill = "white"), 
        plot.title=element_text( hjust=0.2, vjust=0.2, face='bold'))

ggraph(clean_net, layout = "circle") +
  geom_node_point(aes(shape = as.factor(name), size = 10)) +
  geom_edge_fan(aes(width = Observed/2), alpha = 0.1, linejoin = "bevel" ,
                arrow = arrow(angle = 40, length = unit(1, "mm"),
                              type = "open"), start_cap = circle(2, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_width(range = c(0.3, 2)) + 
  ggforce::geom_mark_rect(aes(x,y,linetype=as.factor(cluster), col = as.factor(cluster))) +
  labs(linetype = "Cluster") +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9)) + 
  labs(edge_width = "N of emails") +
  guides(edge_alpha = "none", size = "none", shape = guide_legend(title = "Department")) +
  guides(edge_width = "none", edge_alpha = "none", alpha = "none", size = "none", color = "none") +
  theme(panel.background = element_rect(fill = "white"), 
        plot.title=element_text( hjust=0.2, vjust=0.2, face='bold'))

