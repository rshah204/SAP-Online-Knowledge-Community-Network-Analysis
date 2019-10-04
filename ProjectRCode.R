###############################################
#### Advanced Lab 2: SAP Community Network ####
####            Rahul Shah                 ####
###############################################

# Setting Working Directory 
dir_path <- getwd()
setwd(dir_path)

# Clearing everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

## Edges
ecount(g_SAPSub)
# ---
# [1] 6090
# ---

## Vertices
vcount(g_SAPSub)
# ---
# [1] 3415
# ---

## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

# Create edge weights
E(g_SAPSub)$weight <- 1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Summarize the graph structure
summary(g_SAPSub_simpl)

## Edges
ecount(g_SAPSub_simpl)
# ---
# [1] 4120
# ---

## Vertices
vcount(g_SAPSub_simpl)
# ---
# [1] 3415
# ---

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <- inv_weight

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'))
neighbors(g_SAPSub_simpl, v=c('592540'))


######################
#### Node Degrees ####
######################
in.deg <- degree(g_SAPSub_simpl,v=V(g_SAPSub_simpl),mode="in")
out.deg <- degree(g_SAPSub_simpl,v=V(g_SAPSub_simpl),mode="out")
all.deg <- degree(g_SAPSub_simpl,v=V(g_SAPSub_simpl),mode="all")
# Overall Max Degree Node
max(degree(g_SAPSub_simpl)) # Gives overall max degree in network
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl)==max(degree(g_SAPSub_simpl))] # gives the corresponding user
# Max In Degree Node
max(degree(g_SAPSub_simpl, mode='in')) # Gives max indegree in network
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl, mode='in')==max(degree(g_SAPSub_simpl, mode='in'))] # gives the corresponding user
degree(g_SAPSub_simpl)['3510478'] # Total degrees of this user
# Max Out Degree Node
max(degree(g_SAPSub_simpl, mode='out')) # Gives max outdegree in network
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl, mode='out')==max(degree(g_SAPSub_simpl, mode='out'))] # gives the corresponding user

# Mean Degree
mean(degree(g_SAPSub_simpl))
table(degree(g_SAPSub_simpl, mode="in"))
table(degree(g_SAPSub_simpl, mode="out"))

##########################################
# Vertex Strength Distribution Histogram #
##########################################
par(mfrow=c(2,1))
# InDegree
hist(graph.strength(g_SAPSub_simpl, mode='in'), col="pink",
     xlab="Vertex Strength", ylab="Frequency", 
     main="Vertex Strength Distribution: InDegree",
     breaks = 50)
# OutDegree
hist(graph.strength(g_SAPSub_simpl, mode='out'), col="lightblue",
     xlab="Vertex Strength", ylab="Frequency", 
     main="Vertex Strength Distribution: OutDegree",
     breaks = 50)

###############################
# Log-log degree distribution #
###############################
par(mfrow=c(1, 2))
# InDegree
d.net.in <-degree(g_SAPSub_simpl, mode='in')
dd.net.in <- degree.distribution(g_SAPSub_simpl, mode='in')
d.in <- 1:max(d.net.in)-1
ind.in <- (dd.net.in != 0)
plot(d.in[ind.in], dd.net.in[ind.in], log="xy", col="blue",
     xlab=c("Log-InDegree"), ylab=c("Log-Intensity"),
     main="Log-Log InDegree Distribution")
# OutDegree
d.net.out <-degree(g_SAPSub_simpl, mode='out')
dd.net.out <- degree.distribution(g_SAPSub_simpl, mode='out')
d.out <- 1:max(d.net.out)-1
ind.out <- (dd.net.out != 0)
plot(d.out[ind.out], dd.net.out[ind.out], log="xy", col="red",
     xlab=c("Log-OutDegree"), ylab=c("Log-Intensity"),
     main="Log-Log OutDegree Distribution")


###############################################
# Average neighbor degree versus vertex degree#
###############################################
par(mfrow=c(1, 1))
d.net <-degree(g_SAPSub_simpl)
a.nn.deg <- graph.knn(g_SAPSub_simpl,V(g_SAPSub_simpl))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

####################################
# Connectivity Measures of Network #
#################################### 
# Network Density
graph.density(g_SAPSub_simpl)
# Average Path Length
mean_distance(g_SAPSub_simpl) #average.path.length(g_SAPSub_simpl, directed=TRUE)

# Number of weakly connected components
is.connected(g_SAPSub_simpl, mode="weak")
# ---
## [1] FALSE
# ---
g_SAPSub_simpl.wcc <- clusters(g_SAPSub_simpl, mode="weak")
table(g_SAPSub_simpl.wcc$csize)
# ---
##    2    3    4    5    6    7    9 2696 
##  239   43   11    3    5    2    1    1
# ---
# Number of strongly connected components
is.connected(g_SAPSub_simpl, mode="strong")
# ---
## [1] FALSE
# ---
g_SAPSub_simpl.scc <- clusters(g_SAPSub_simpl, mode="strong")
table(g_SAPSub_simpl.scc$csize)
# ---
##    1    2    3    9 
## 3387    8    1    1 
# ---

########################
# Reciprocal Relations #
########################
reciprocity(g_SAPSub_simpl)
dyad_census(g_SAPSub_simpl)
which_mutual(g_SAPSub_simpl)
sum(which_mutual(g_SAPSub_simpl))/2 == dyad_census(g_SAPSub_simpl)$mut
# Table of vertexes in mutual edges
table(unlist(strsplit(attr(E(g_SAPSub_simpl)[which_mutual(g_SAPSub_simpl)],"vnames"),"\\|")))/2

####################
# Network Diameter #
####################
# Diameter with regular weights
diameter(g_SAPSub_simpl, weights= num_weight)
# Diameter with inverse weights
diameter(g_SAPSub_simpl, weights= inv_weight)

##############
# Clustering #
##############
# Global clustering coefficient
transitivity(g_SAPSub_simpl, weights = inv_weight)

#################
# Clique Census #
#################
# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(cliques(g_SAPSub_simpl), length))
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
cliques(g_SAPSub_simpl)[sapply(cliques(g_SAPSub_simpl), length) == 5]
clique.number(g_SAPSub_simpl)


#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight


################################
# Local Measures: Centralities #
################################

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
# Degree centrality
degree_SAP <- degree(g_SAPSub_simpl)
# Node betweenness
nodebetweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(nodebetweens_SAP, constraints_SAP, clustering_SAP, degree_SAP)
a_node<-aggregate(nodebetweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")

# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(nodebetweens_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness of nodes")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

# Closeness Centrality
close_SAP <- closeness(g_SAPSub_simpl)
# Eigen Centrality
eig_SAP <- evcent(g_SAPSub_simpl)$vector

# Hub and Authority Scores
hub_SAP <- hub.score(g_SAPSub_simpl, weights=inv_weight)$vector
auth_SAP <- authority.score(g_SAPSub_simpl, weights=inv_weight)$vector
head(sort(hub_SAP, decreasing=TRUE))
head(sort(auth_SAP, decreasing=TRUE))

centralities <- cbind(degree_SAP, nodebetweens_SAP, edgebetweens_SAP, close_SAP, eig_SAP, hub_SAP, auth_SAP)
cor.matrix = round(cor(centralities), 4)
write.csv(cor.matrix, "cor_matrix.csv")


#############################
#  Network Visualizations   #
#############################
par(mfrow=c(1, 1))
table(degree(g_SAPSub_simpl, mode="in"))
table(degree(g_SAPSub_simpl, mode="out"))

###############################
# Fruchterman Reingold Layout #
###############################
set.seed(9143)  # setting seed so as to make the layout reproducible
layout1 <- layout.fruchterman.reingold(g_SAPSub_simpl,niter=500) # Creating a layout object to tell iGraph what layout I want

# Node Color
V(g_SAPSub_simpl)$color <- "grey"
#V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>8]$color <- "yellow"  #Distinguishing High Degree Nodes as yellow
#V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="out")>8]$color <- "green" 

V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>1]$color <- "lightcyan"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>2]$color <- "cyan"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>3]$color <- "cyan3"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>4]$color <- "mediumblue"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>8]$color <- "navyblue"

# Vertex Area Proportional to Vertex Strength i.e. Weighted Vertex Degree
V(g_SAPSub_simpl)$size <- 3*(graph.strength(g_SAPSub_simpl))^(1/3)
#V(g_SAPSub_simpl)$size <- 3*degree(g_SAPSub_simpl, mode = "all")^(1/3)

# Vertex Area Proportional to Betweenness Centrality
#V(g_SAPSub_simpl)$size <- (betweenness(g_SAPSub_simpl))/200

# Width	of Edges Proportional to the (logarithm	of) Edge Weights
E(g_SAPSub_simpl)$width <- log(E(g_SAPSub_simpl)$weight+1)
E(g_SAPSub_simpl)$color <- "gray80"

#Plotting 
plot(g_SAPSub_simpl, edge.arrow.size=0.2, vertex.label = NA)
legend("left", inset=.8, title="Node Color Scheme",
       c("InDeg=0","InDeg=1","InDeg=2","InDeg=3","InDeg=4,5,6,7","InDeg>8"), 
       fill=c('grey','lightcyan','cyan','cyan3','mediumblue', 'navyblue'), horiz=FALSE, box.lty=0, cex=0.7)
title("SAP Network Graph: Fruchterman Reingold Layout")

###########################
# Kamada and Kawai Layout #
###########################
set.seed(9143)  # setting seed so as to make the layout reproducible
layout2 <- layout.kamada.kawai(g_SAPSub_simpl)
# Node Color
V(g_SAPSub_simpl)$color <- "grey"
#V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>8]$color <- "yellow"  #Distinguishing High Degree Nodes as yellow
#V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="out")>8]$color <- "green" 

V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>1]$color <- "lightcyan"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>2]$color <- "cyan"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>3]$color <- "cyan3"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>4]$color <- "mediumblue"
V(g_SAPSub_simpl)[degree(g_SAPSub_simpl, mode="in")>8]$color <- "navyblue"

# Vertex Area Proportional to Vertex Strength i.e. Weighted Vertex Degree
V(g_SAPSub_simpl)$size <- 3*(graph.strength(g_SAPSub_simpl))^(1/3)
#V(g_SAPSub_simpl)$size <- 3*degree(g_SAPSub_simpl, mode = "all")^(1/3)

# Vertex Area Proportional to Betweenness Centrality
#V(g_SAPSub_simpl)$size <- (betweenness(g_SAPSub_simpl))/200

# Width	of Edges Proportional to the (logarithm	of) Edge Weights
E(g_SAPSub_simpl)$width <- log(E(g_SAPSub_simpl)$weight+1)
E(g_SAPSub_simpl)$color <- "gray80"

#Plotting 
plot(g_SAPSub_simpl, layout=layout2, edge.arrow.size=0.2, vertex.label = NA)
legend("left", inset=.8, title="Node Color Scheme",
       c("InDeg=0","InDeg=1","InDeg=2","InDeg=3","InDeg=4,5,6,7","InDeg>8"), 
       fill=c('grey','lightcyan','cyan','cyan3','mediumblue', 'navyblue'), horiz=FALSE, box.lty=0, cex=0.7)
title("SAP Network Graph: Kamada and Kawai Layout")
