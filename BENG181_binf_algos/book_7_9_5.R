options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
P <- lines[1]
Q <- lines[2]

P <- parse_genome(P)
Q <- parse_genome(Q)

#make sure edges listed bidirectionally
#output is still in list like edges is
two_break_on_genome_graph <- function( edges, i1, i2, i3, i4 ) {
	edges <- remove_edge( edges, i1, i2)
	edges <- remove_edge( edges, i3, i4)
	i1 <- as.character(i1)
	i2 <- as.character(i2)
	i3 <- as.character(i3)
	i4 <- as.character(i4)
	if( is.null(edges[[ i1 ]]) ) {
		edges[[ i1 ]] <- as.character(i3) 
	}
	else {
		edges[[ i1 ]] <- c(edges[[ i1 ]], as.character(i3))
	}
	if( is.null(edges[[ i2 ]]) ) {
		edges[[ i2 ]] <- as.character(i4) 
	}
	else {
		edges[[ i2 ]] <- c(edges[[ i2 ]], as.character(i4))
	}
	if( is.null(edges[[ i3 ]]) ) {
		edges[[ i3 ]] <- as.character(i1) 
	}
	else {
		edges[[ i3 ]] <- c(edges[[ i3 ]], as.character(i1))
	}
	if( is.null(edges[[ i4 ]]) ) {
		edges[[ i4 ]] <- as.character(i2) 
	}
	else {
		edges[[ i4 ]] <- c(edges[[ i4 ]], as.character(i2))
	}
	return(edges)	
}

two_break_on_genome <- function( genome, i1, i2, i3, i4 ) {
	edges <- colored_edges(genome, dbl=T)
	print(edges)
	edges <- two_break_on_genome_graph( edges, i1, i2, i3, i4 )
	genome <- graph_to_genome(edges)
	return(genome)
}

inp <- parse_genome("(+1 -2 -4 +3)")
i1 <- 1
i2 <- 6
i3 <- 3
i4 <- 8
print(two_break_on_genome( inp, i1, i2, i3, i4))
