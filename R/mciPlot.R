mciPlot <- function(x,...){
  g = graph.adjacency(adjmatrix=x$adjMat, mode="undirected")
   
  
  #V(g)$label <- V(g)$name
  #V(g)$size  <- 50
  #g$cex      <-  4
  #V(g)$label.cex <- 1.2
  
  V(g)$color <- "grey"
  
  
  #g$layout <- layout.lgl
  plot(g)
  return(invisible(x))
}