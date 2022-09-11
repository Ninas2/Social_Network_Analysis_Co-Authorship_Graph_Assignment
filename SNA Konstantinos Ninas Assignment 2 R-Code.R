#setwd("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\3rd Quarter\\Social Network Analysis\\Assignment 2")
setwd("C:\\Users\\user\\Desktop\\edge")
#.libPaths('C:\\Users\\ninas\\R\\RPackages')

#import of library
library(igraph)
library(ggplot2)
library('DT')
library(randomcoloR)

#loads all the csv files
edge_list_2016 <- read.csv('edge_list_2016.csv')
edge_list_2017 <- read.csv('edge_list_2017.csv')
edge_list_2018 <- read.csv('edge_list_2018.csv')
edge_list_2019 <- read.csv('edge_list_2019.csv')
edge_list_2020 <- read.csv('edge_list_2020.csv')


#create one graph for each year
graph_2016 <- graph_from_data_frame(edge_list_2016, directed = F)
graph_2017 <- graph_from_data_frame(edge_list_2017, directed = F)
graph_2018 <- graph_from_data_frame(edge_list_2018, directed = F)
graph_2019 <- graph_from_data_frame(edge_list_2019, directed = F)
graph_2020 <- graph_from_data_frame(edge_list_2020, directed = F)

#assigning weight to each edge of each graph
E(graph_2016)$weight <- edge_list_2016[,3]
E(graph_2017)$weight <- edge_list_2017[,3]
E(graph_2018)$weight <- edge_list_2018[,3]
E(graph_2019)$weight <- edge_list_2019[,3]
E(graph_2020)$weight <- edge_list_2020[,3]

#check if all graphs are weighted-all True
is_weighted(graph_2016)
is_weighted(graph_2017)
is_weighted(graph_2018)
is_weighted(graph_2019)
is_weighted(graph_2020)

#we find any possible multiple edges in order to remove them
#we will create a function to do the above process
mult_edges_rem <- function(graph,edge_list){
  edges_to_be_rem <- edge_list[which_multiple(graph),]
  #create a graph with all multiple edges
  edges_to_be_rem <- graph_from_data_frame(edges_to_be_rem, directed = F)
  #we remove them from the initial graph
  graph <- graph - edges_to_be_rem
}

#we will take the new graphs without multiple edges by calling the
#above function
graph_2016 <- mult_edges_rem(graph_2016, edge_list_2016)
graph_2017 <- mult_edges_rem(graph_2017, edge_list_2017)
graph_2018 <- mult_edges_rem(graph_2018, edge_list_2018)
graph_2019 <- mult_edges_rem(graph_2019, edge_list_2019)
graph_2020 <- mult_edges_rem(graph_2020, edge_list_2020)



#Task 2 - Plot the evolution of graphs based on certain metrics
#identify the number of vertices for each graph
v_2016 <- vcount(graph_2016)
v_2017 <- vcount(graph_2017)
v_2018 <- vcount(graph_2018)
v_2019 <- vcount(graph_2019)
v_2020 <- vcount(graph_2020)

#put all the vertice counts along with the year in a dataframe
vertices <- data.frame(rbind(c(2016,v_2016),c(2017,v_2017), c(2018,v_2018)
      ,c(2019,v_2019), c(2020,v_2020)))

#rename the columns
colnames(vertices) <- c('Year','Vertices')

#plot the number of vertices throughout the years
ggplot(vertices, aes(y=Vertices, x=Year))  +
  geom_area( fill="steelblue", alpha=0.4) +
  geom_line(color="steelblue", size=2) +
  geom_point(size=3, color="steelblue") +
  labs(title = "Number of Vertices per Year") +
  theme(plot.title = element_text(hjust = 0.5))

#identify the number of edges for each graph
e_2016 <- ecount(graph_2016)
e_2017 <- ecount(graph_2017)
e_2018 <- ecount(graph_2018)
e_2019 <- ecount(graph_2019)
e_2020 <- ecount(graph_2020)

#put all the edge counts along with the year in a dataframe
edges <- data.frame(rbind(c(2016,e_2016),c(2017,e_2017), c(2018,e_2018)
                             ,c(2019,e_2019), c(2020,e_2020)))

#rename the columns
colnames(edges) <- c('Year','Edges')

#plot the number of edges throughout the years
ggplot(edges, aes(Year, Edges)) +
  geom_bar(stat="identity") +
  labs(title = "Number of Edges per Year") +
  theme(plot.title = element_text(hjust = 0.5))


#identify the diameter for each graph
d_2016 <- diameter(graph_2016)
d_2017 <- diameter(graph_2017)
d_2018 <- diameter(graph_2018)
d_2019 <- diameter(graph_2019)
d_2020 <- diameter(graph_2020)


#put all the diameters along with the year in a dataframe
diameters <- data.frame(rbind(c(2016,d_2016),c(2017,d_2017), c(2018,d_2018)
                             ,c(2019,d_2019), c(2020,d_2020)))

#rename the columns
colnames(diameters) <- c('Year','Diameter')


#plot the diameters throughout the years
ggplot(diameters, aes(Year, Diameter)) +
  geom_line(color="steelblue") +
  geom_point() +
  labs(title = "Diameter per Year") +
  theme(plot.title = element_text(hjust = 0.5))


#identify the average degree per graph
avg_deg_2016 <- mean(igraph::degree(graph_2016))
avg_deg_2017 <- mean(igraph::degree(graph_2017))
avg_deg_2018 <- mean(igraph::degree(graph_2018))
avg_deg_2019 <- mean(igraph::degree(graph_2019))
avg_deg_2020 <- mean(igraph::degree(graph_2020))

#put all the average degrees along with the year in a dataframe
avg_degrees <- data.frame(rbind(c(2016,avg_deg_2016),c(2017,avg_deg_2017), c(2018,avg_deg_2018)
                              ,c(2019,avg_deg_2019), c(2020,avg_deg_2020)))

#rename the columns
colnames(avg_degrees) <- c('Year','Average Degree')

#plot the diameters throughout the years
ggplot(avg_degrees, aes(Year, `Average Degree`)) + geom_line() +
  labs(title = "Average Degree per Year") +
  theme(plot.title = element_text(hjust = 0.5))


#Task 3 - find the 10 most important nodes based on degree and pagerank
#for each year and compare them
#The top-10 authors of 2016 as far as their degree is concerned
all_degrees_2016 <- igraph::degree(graph_2016)
top_10_characters <- sort.int(all_degrees_2016,decreasing=TRUE,index.return=FALSE)[1:10]
top_degrees <- data.frame(Authors=character(10))
top_degrees$top_2016 <- rownames(as.data.frame(top_10_characters))


#The top-10 authors of 2017 as far as their degree is concerned
all_degrees_2017 <- igraph::degree(graph_2017)
top_10_characters <- sort.int(all_degrees_2017,decreasing=TRUE,index.return=FALSE)[1:10]
top_degrees$top_2017 <- rownames(as.data.frame(top_10_characters))


#The top-10 authors of 2018 as far as their degree is concerned
all_degrees_2018 <- igraph::degree(graph_2018)
top_10_characters <- sort.int(all_degrees_2018,decreasing=TRUE,index.return=FALSE)[1:10]
top_degrees$top_2018 <- rownames(as.data.frame(top_10_characters))
 

#The top-10 authors of 2019 as far as their degree is concerned
all_degrees_2019 <- igraph::degree(graph_2019)
top_10_characters <- sort.int(all_degrees_2019,decreasing=TRUE,index.return=FALSE)[1:10]
top_degrees$top_2019 <- rownames(as.data.frame(top_10_characters))
 

#The top-10 authors of 2020 as far as their degree is concerned
all_degrees_2020 <- igraph::degree(graph_2020)
top_10_characters <- sort.int(all_degrees_2020,decreasing=TRUE,index.return=FALSE)[1:10]
top_degrees$top_2020 <- rownames(as.data.frame(top_10_characters))
top_degrees <- top_degrees[,-1]
  
#print the table with the 10 greatest degree authors per year
datatable(top_degrees)


#we will do the same process, but considering their pagerank values
#for 2016
pageranks <- page_rank(graph_2016, directed = FALSE)
pageranks_top_10 <- sort(pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:10]
top_pr <- data.frame(Authors=character(10))
top_pr$top_2016 <- rownames(as.data.frame(pageranks_top_10))


#for 2017
pageranks <- page_rank(graph_2017, directed = FALSE)
pageranks_top_10 <- sort(pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:10]
top_pr$top_2017 <- rownames(as.data.frame(pageranks_top_10))


#for 2018
pageranks <- page_rank(graph_2018, directed = FALSE)
pageranks_top_10 <- sort(pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:10]
top_pr$top_2018 <- rownames(as.data.frame(pageranks_top_10))
 

#for 2019 
pageranks <- page_rank(graph_2019, directed = FALSE)
pageranks_top_10 <- sort(pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:10]
top_pr$top_2019 <- rownames(as.data.frame(pageranks_top_10))
 

#for 2020 
pageranks <- page_rank(graph_2020, directed = FALSE)
pageranks_top_10 <- sort(pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:10]
top_pr$top_2020 <- rownames(as.data.frame(pageranks_top_10))
top_pr <- top_pr[,-1]


#print the table with the 10 greatest pagerank authors per year
datatable(top_pr)


#Task 4 - perform community detection
#1st method -  fast greedy clustering #fast
fast_2016 <- cluster_fast_greedy(graph_2016) 
fast_2017 <- cluster_fast_greedy(graph_2017)
fast_2018 <- cluster_fast_greedy(graph_2018)
fast_2019 <- cluster_fast_greedy(graph_2019)
fast_2020 <- cluster_fast_greedy(graph_2020)

#2nd method - infomap clustering #slow-creates more communities
info_com_2016 <- cluster_infomap(graph_2016) 
info_com_2017 <- cluster_infomap(graph_2017)
info_com_2018 <- cluster_infomap(graph_2018)
info_com_2019 <- cluster_infomap(graph_2019)
info_com_2020 <- cluster_infomap(graph_2020)

#3rd method - louvain clustering
louvain_2016 <- cluster_louvain(graph_2016) 
louvain_2017 <- cluster_louvain(graph_2017) 
louvain_2018 <- cluster_louvain(graph_2018) 
louvain_2019 <- cluster_louvain(graph_2019) 
louvain_2020 <- cluster_louvain(graph_2020) 
#faster than infomap, creates less communities
#approximately the same with fast greedy

#we will find all the authors that exist in all 5 graphs
nodes_until_17 <- intersect(names(V(graph_2016)),names(V(graph_2017)))
nodes_until_18 <- intersect(nodes_until_17,names(V(graph_2018)))
nodes_until_19 <- intersect(nodes_until_18,names(V(graph_2019)))
nodes_until_20 <- intersect(nodes_until_19,names(V(graph_2020)))

#we select a random author from the list of authors in all 5 years
rand_author <- nodes_until_20[129]
rand_author

#we will use the louvain clustering method for Ruizhe Ma
#to detect the evolution in the communities he belongs to
#we create a function that will find the authors in the same community
#with a selected author
community <- function(rand_author, gr){
  #we find the index of the selected author in the graph
  graph_index <- which(names(V(gr))==rand_author)

  #we find the community the user belongs to
  louvain <- cluster_louvain(gr)
  comm_index <- louvain$membership[graph_index]
  
  #we find all the authors that belong in the selected community of that year
  all_nodes_in_comm <- which(louvain$membership==comm_index)
  
  #find the name of the authors in that community and its size
  comm <- names(V(gr))[all_nodes_in_comm]
  return(comm)
}

#we find the authors in the same community as the selected one in each year
comm_2016 <- community(rand_author,graph_2016); comm_2016
comm_2017 <- community(rand_author,graph_2017); comm_2017
comm_2018 <- community(rand_author,graph_2018); comm_2018
comm_2019 <- community(rand_author,graph_2019); comm_2019
comm_2020 <- community(rand_author,graph_2020); comm_2020


#we will create a function that will disregard too small
#and too big communities, and plot the subgraph
subgraph_plot <- function(graph){
  
  #apply louvain algorithm in the graph
  louvain <- cluster_louvain(graph)
  
  
  subgraph<-delete_vertices(graph,V(graph)[louvain$membership %in%  unique(which(table(louvain$membership)>30 |
                 table(louvain$membership)<15))])
  
  #conduct louvain clustering on the subgraph
  louvain <- cluster_louvain(subgraph)
  
  #get the community for each node
  V(subgraph)$community <- louvain$membership
  
  #assign color to communities
  colors<-distinctColorPalette(k=max(louvain$membership))
  
  #calculate custom sizes-according to the community size
  comm_sizes <- table(louvain$membership)
  
  #empty vector that will be filled with each node's size
  sizes <- c()
  
  #assign the appropriate size given its community
  for (i in 1:length(louvain$membership)){
    sizes[i] <- comm_sizes[which(names(comm_sizes)==louvain$membership[i])]
  }
  
  #plot the subgraph with specific sizes and colors per community
  plot(subgraph, vertex.label = NA
       ,vertex.color=colors[louvain$membership]
       , vertex.size=sizes/4)
  #we also return the suubraph to further examine it
  return(subgraph)
}

#we plot the graph for each year
#we take subplot for each year and check whether the communities
#are the same
subg_2016 <-subgraph_plot(graph_2016) #2016

subg_2017<-subgraph_plot(graph_2017) #2017

subg_2018<-subgraph_plot(graph_2018) #2018

subg_2019<-subgraph_plot(graph_2019) #2019

subg_2020<-subgraph_plot(graph_2020) #2020
#same community with the previous graph for all years for Ruizhe Ma

#we will check for additional random authors to see
#if their communities are the same in the graph and the subgraph
rand_author <- names(V(subg_2016))[4]
rand_author
community(rand_author,graph_2016) 
community(rand_author,subg_2016)#same community

rand_author <- names(V(subg_2017))[55]
rand_author
community(rand_author,graph_2017) 
community(rand_author,subg_2017)#same community

rand_author <- names(V(subg_2018))[455]
rand_author
community(rand_author,graph_2018) 
community(rand_author,subg_2018)#same community

rand_author <-names(V(subg_2019))[399]
rand_author
community(rand_author,graph_2019) 
community(rand_author,subg_2019)#same community

rand_author <-names(V(subg_2020))[182]
rand_author
community(rand_author,graph_2020) 
community(rand_author,subg_2020)#same community

#we assume that the remaining communities of the subgraph
#are the same with those of the initial graph for each
#year with the same community sizes (between 15 and 30)

