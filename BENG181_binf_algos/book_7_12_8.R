options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

#Go from "(2,4), (3,6), ..." format to R list
#list has the numbers as chars
parse_colored_edges <- function(str) {
	edges <- list()
	points <- strsplit(str, ")")[[1]]
	for( i in 1:length(points) ) {
		points[i] <- strsplit(points[i], "(", fixed=T)[[1]][2]
		nums <- strsplit(points[i], ", ")[[1]]
		edges[[ nums[1] ]] <- nums[2]
	}
	return(edges)
}

#input is edges as R list
graph_to_genome <- function(edges) {
	genome <- list()
	visited <- c()
	for( from in names(edges) ) {
		cycle <- c()
		if(from %in% visited) {
			next
		}
		curr <- from
		repeat {
			to <- as.numeric(edges[[ as.character(curr) ]])
			visited <- c(visited, curr)
			if( to %% 2 == 0 ) { #if to is even
				nxt <- to-1
				nxt_block <- -(to/2)
			}
			else {
				nxt <- to+1
				nxt_block <- (to+1)/2
			}
			curr <- nxt
			cycle <- c(cycle, nxt_block)
			if( nxt == from ) { #stop once we're back at end of cycle
				break
			}
		}
		#this is purely aesthetic so order in order given by examples
		cycle <- c(cycle[length(cycle)], cycle[ 1:(length(cycle)-1) ])
		genome[[ from ]] <- cycle #index cycle by some cycle start
	}
	return(genome)
}

#convert from R list to "(+1 -2 -3)(-4 +5 -6)" format
#This is messy and the naming is bad
print_genome <- function(genome) {
	str <- c()
	for( name in names(genome) ) {
		vec <- genome[[ name ]]
		nums <- c()
		for( i in vec ) {
			if( as.numeric(i) > 0 ) {
				nums <- c(nums, paste0("+", i))
			}
			else {
				nums <- c(nums, as.character(i))
			}
		}
		str <- c(str, paste0("(", paste(nums, collapse=" "),")"))
	}
	return(paste(str, collapse=""))
}

print(print_genome(graph_to_genome(parse_colored_edges(text))))
