setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
adjacency_list <- readLines(fname)

#adjacency_list in actual list object
eulerian_path <- function(adjacency_list) {
	#find start and end
	n_in <- list()
	n_out <- list()
	for( node in names(adjacency_list) ) {
		n_out[[ node ]] <- length(adjacency_list[[ node ]])
		for( target in adjacency_list[[ node ]] ) {
			if(is.null(n_in[[ target ]])) {
				n_in[[ target ]] <- 0
			}
			n_in[[ target ]] <- n_in[[ target ]] + 1
		}
	}
	all_nodes <- unique( c(names(n_in), names(n_out)) ) 
	end <- NULL
	start <- NULL
	for( node in all_nodes ) {
		if( is.null(n_in[[ node ]]) ) {
			n_in[[ node ]] <- 0
		}
		if( is.null(n_out[[ node ]]) ) {
			n_out[[ node ]] <- 0
		}
		if( n_in[[ node ]] > n_out[[ node ]] ) {
			end <- node
		}
		if( n_in[[ node ]] < n_out[[ node ]] ) {
			start <- node
		}
	}
	
	#flag for if start and end can't be found
	if(is.null(start) || is.null(end)) {
		print("yeah we can't find the start/stop")	
	}
	
	#draw path from end -> start
	adjacency_list[[ end ]] <- c( adjacency_list[[ end ]], start )
	
	#solve eulerean circuit
	eulerean_cycle <- eulerean_circuit(adjacency_list)
	
	#remove last element of circuit because we're shifting
	eulerean_cycle <- eulerean_cycle[1:(length(eulerean_cycle)-1)] 
	
	#shift circuit so it starts at start and ends at end
	dont_shift <- ( eulerean_cycle[1] == start && eulerean_cycle[length(eulerean_cycle)] == end )
        new_start_candidates <- which( eulerean_cycle == start )	
	if(!dont_shift) {
		for(ind in new_start_candidates) {
			if( ind == 1 ) {next}
			if( eulerean_cycle[ind-1] == end ) {
				new_start_ind <- ind
				break
			}
		}
        	for(ind in 1:(new_start_ind-1)) {
			eulerean_cycle <- c(eulerean_cycle[-1], eulerean_cycle[1])
 	       }
	}

	return(eulerean_cycle)
}

cat(format_cycle(eulerian_path(parse_graph(adjacency_list))))
