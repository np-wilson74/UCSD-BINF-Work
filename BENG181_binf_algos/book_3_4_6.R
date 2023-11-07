setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])
text <- lines[2]

de_brujin <- function(text, k) {
	n <- nchar(text)
	graph <- list()
	#go thru each k-mer frame and map frame's suffix to prefix 
	for(ind in 0:(n-k)) {
		frame <- Text(text, ind, k)
		if( prefix(frame) %in% names(graph) ) {
			graph[[ prefix(frame) ]] <- c(graph[[ prefix(frame) ]], suffix(frame))
		}
		else {
			graph[[ prefix(frame) ]] <- suffix(frame)
		}
	}
	return(graph)
}

print_graph(de_brujin(text,k))
