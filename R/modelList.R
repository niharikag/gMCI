## testEdges: List all edges that can be added with test statistic, Find most significant edge

## Author: Niharika

## Input
## object   : imod-object
## edgeList : A list of edges; each edge is a vector

## Output
## A dataframe with test statistics (p-value or change in AIC), edges and logical
## telling if the edge can be added 

getSigEdge <- function(object, edgeSet=NULL,...){
  UseMethod("getSigEdge")
}

getSigEdge <- function(object, edgeSet=NULL,...){
  
}