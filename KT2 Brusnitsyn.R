if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}

create_random_graph_data <- function(num_nodes, num_edges) {
  set.seed(123)  
  edges <- data.frame(
    from = sample(1:num_nodes, num_edges, replace = TRUE),
    to = sample(1:num_nodes, num_edges, replace = TRUE)
  )
  return(edges)
}

create_graph <- function(edges) {
  graph <- graph.data.frame(edges, directed = TRUE)
  return(graph)
}

visualize_graph <- function(graph) {

  layouts <- list(
    layout_with_fr(graph),  
    layout.kamada.kawai(graph),  
    layout_nicely(graph)  
  )
  
  node_colors <- rainbow(vcount(graph))
  
  for (i in 1:length(layouts)) {
    par(mfrow=c(1,1))  
    plot(
      graph,
      layout = layouts[[i]],
      vertex.color = node_colors,
      vertex.size = 10,
      vertex.label.cex = 0.8,
      main = paste("Граф с укладкой", i)
    )
    legend("topright", legend = unique(node_colors), fill = unique(node_colors), inset = 0.05)
  }
}

num_nodes <- 10  
num_edges <- 15  
edges <- create_random_graph_data(num_nodes, num_edges)
graph <- create_graph(edges)
visualize_graph(graph)