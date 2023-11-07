options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
start <- lines[1]
end <- lines[2]
adj_list <- lines[3:length(lines)]

#stored with list where each key is a node
#then each key maps to a list of (target, weight) pairs
parse_weighted_graph <- function(adj_list) {
	graph <- list()
	for(entry in adj_list) {
		spl <- strsplit( entry, "->" )[[1]]
		from <- spl[1]
		back <- strsplit( spl[2], ":", fixed=T )[[1]]
		to <- back[1]
		weight <- back[2]
		
		if( is.null(graph[from]) ) {
			graph[[ from ]] <- list()
		}

		#I don't want the indexing for nested list to matter
		#So I'm going to just assign in order of adding
		graph[[ from ]][[ as.character(length(graph[[ from ]])) ]] <- c(to, weight)

	}
	return(graph)
}

longest_dag <- function(adj_list, start, end) {
	to_consider <- c(start)
	scores <- list()
	#scores is list of score & pred that gave it that score
	scores[[ start ]] <- c(0, NULL)

	#while I've not checked path, update scores for each 'to' node
	while( length(to_consider) > 0 ) {
		from <- to_consider[1]
		tos <- adj_list[[ from ]]
		#to[1] is to node, to[2] is weight of that path
		#updates scores of all targets of from
		#some sort of nonsense w/ casting is happening so I have extra casts
		for( to in tos ) {
			new_score <- as.numeric(scores[[ from ]][1]) + as.numeric(to[2])
			if( is.null(scores[[ to[1] ]]) ) {
				scores[[ to[1] ]] <- c(new_score, from)
			}
			else if( as.numeric(scores[[ to[1] ]][1]) < as.numeric(new_score) ) {
				scores[[ to[1] ]] <- c(new_score, from)
			}
			#add 'to' to to_consider
			to_consider <- c(to_consider, to[1])
		}

		#remove first element (one we checked)
		to_consider <- to_consider[-1]
	}

	#backtrack to find path that got us max_dag
	path <- c()
	curr <- end
	while( curr != start ) {
		path <- c(curr, path)
		curr <- scores[[ curr ]][2]
	}
	path <- c(start, path)

	return(c(scores[[ end ]][1], format_cycle(path)))
}

print(longest_dag(parse_weighted_graph(adj_list), start, end))
