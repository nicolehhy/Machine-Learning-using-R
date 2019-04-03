setwd("C:/STONY/Practice/R (No.13)")
media <- read.csv("Media-EDGES1.csv", header=T, as.is=T)
tnames=names(table(c(names(table(media$from)),names(table(media$to)))))
from <- as.character(media[,1])
to <- as.character(media[,2])

library(igraph)
mnet <- graph.empty()
mnet <- add.vertices(mnet,length(tnames),
                     calling=as.character(media[,1]),called=as.character(media[,2]))
ids <- 1:length(tnames)
names(ids) <- tnames
ids
from <- as.character(media[,1])
to <- as.character(media[,2])
edges <- matrix(c(ids[from],ids[to]),nc=2)
mnet <- add.edges(mnet,t(edges))
mnet <- as.undirected(mnet)
eig = evcent(mnet)$vector
eig


library(igraph)
setwd("C:/STONY/Practice/R (No.13)")

# Read in the data:
nodes <- read.csv("Media-NODES.csv", header=T, as.is=T)
links <- read.csv("Media-EDGES2.csv", header=T, as.is=T)

# Examine the data
head(nodes)
head(links)

# Converting the data to an igraph object:
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

# Examine the resulting object: it has to be igraph when we plot the network
class(net)
net 


# make a network plot
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 

# make a network plot into a round shape
l <- layout_in_circle(net)
plot(net, layout=l,vertex.label=V(net)$media)



# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# Set edge width based on weight:
E(net)$width <- E(net)$weight

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl
plot(net,vertex.label=V(net)$media) 


# Plot the degree distribution for the network
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(degree(net)), y=1-deg.dist, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")
