options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
dag <- parse_graph(lines)

topological_ordering <- function(dag) {
	order <- c()
	all <- unique(c(names(dag), unlist(dag)))
	#being in unlist means node has incoming edge
	srcs <- all[ !(all %in% unlist(dag)) ]
	while( length(srcs) > 0 ) {
		#pick source, add it to ordering, remove it
		src <- srcs[1]
		order <- c(order, src)
		all <- all[ all != src ]
		srcs <- srcs[ srcs != src ]
		dag[[ src ]] <- NULL
		
		#find new sources
		srcs <- all[ !(all %in% unlist(dag)) ]
	}
	if( length(unlist(dag)) > 0 ) {return("Input graph is not a DAG")}
	return(order)
}

print(paste(topological_ordering(dag), collapse=", "))
