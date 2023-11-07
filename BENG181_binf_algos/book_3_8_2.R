setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
adjacency_list <- readLines(fname)

#convert adjacency list in "0 -> 3" format to list object
parse_graph <- function(adjacency_list) {
	new_list <- list()
	for(line in adjacency_list) {
		parts <- strsplit(line, " ")[[1]]
		from <- parts[1]
		to <- parts[3]
		if(is.na(to)) {next}
		to <- strsplit(to, ",")[[1]]
		new_list[[ from ]] <- to
	}
	return(new_list)
}

#input is list
eulerean_cycle <- function(adjacency_list) {
	adj_list <- adjacency_list
	curr <- names(adjacency_list)[1]
	cycle <- c()
	has_edges <- c()
	list_has_edge <- T
	repeat {
		#Walk cycle
		start <- curr
		repeat {
			cycle <- c(cycle, curr)
			has_edges <- c(has_edges, length(adj_list[[ curr ]]) > 1)
			next_ <- adj_list[[ curr ]][1]
			#remove edge
                        adj_list[[ curr ]] <- adj_list[[ curr ]][ adj_list[[ curr ]] != next_ ]
                        curr <- next_
			#If we're stuck, stop
			#This should imply we're back at start of our walk
			#idk which condition does it exactly but its all the same idea
			if( is.na(curr) || is.null(curr) || length(curr) == 0 ) {
				break
			}
		}
		#Deal with getting back to start

		#Check if there's any edges left to explore	
		if( !any(has_edges) ) {break}

		#remove last element from cycle since it's same as start point
		#This is here so that if the code ends we'll see the last cycle
		cycle <- cycle[ 1:length(cycle)-1 ]		
		has_edges <- has_edges[ 1:length(has_edges)-1 ]		

		#Select new starting point
		new_start <- cycle[ has_edges ][1]
		new_start_ind <- which( cycle == new_start )[1]

		#shift our walk so we started at new_start
		for(ind in 1:(new_start_ind-1)) {
			cycle <- c(cycle[-1], cycle[1])
			has_edges <- c(has_edges[-1], has_edges[1])
		}
		has_edges[1] <- F
		curr <- new_start
		
	}
	return(cycle)

}

#change from vector to "1->2->3 ..."
format_cycle <- function(cycle) {
	return(paste(cycle, collapse='->'))
}


cat(format_cycle(eulerean_cycle(parse_graph(adjacency_list))))
