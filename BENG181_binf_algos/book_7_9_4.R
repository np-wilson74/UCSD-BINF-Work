options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
P <- lines[1]
Q <- lines[2]

#from (+1 -3 -6 -5)(+2 -4) to R list of vectors
#It's helpful to think of genome as a collection of chromosomes
parse_genome <- function(genome) {
	genome <- strsplit(genome, ")", fixed=T)[[1]]
	genome_list <- list()
	for( i in 1:length(genome) ) {
		chromosome <- genome[i]
		chromosome <- substr(chromosome, 2, nchar(chromosome))
		genome_list[[ as.character(i) ]] <- as.numeric(strsplit(chromosome, " ")[[1]]) 
	}
	return(genome_list)
}

#input is genome as list
#output is list of cycles (convert chromosomes to cycle format)
genome_to_cycles <- function(genome) {
	for( chrom in names(genome) ) {
		chromosome <- genome[[ chrom ]]
		as_cycle <- chromosome_to_cycle( chromosome )
		genome[[ chrom ]] <- as_cycle
	}
	return(genome)
}

#remove edge from list, can go both ways (i.e. can remove if we don't know which to/from is key/value
remove_edge_obsolete <- function(edges, to, from) {
	to <- as.character(to)
	from <- as.character(from)
	if( from %in% names(edges) && length(edges[[ from ]] > 0) ) {
		if( to %in% edges[[ from ]] ) {
			if( length(edges[[ from ]] > 1 ) ) {
				edges[[ from ]] <- edges[[ from ]][ which(edges[[ from ]] != to) ]
			}
			else {
				edges[[ from ]] <- NULL
			}
			if( length(edges[[from]]) < 1) {
				edges[[from]] <- NULL
			}
			if( length(edges[[to]]) < 1) {
				edges[[to]] <- NULL
			}
			return(edges)
		}
	}
	else {
		if( length(edges[[ to ]] > 1) ) {
			edges[[ to ]] <- edges[[ to ]][ which(edges[[ to ]] != from) ]
		}
		else {
			edges[[ to ]] <- NULL
		}
		if( length(edges[[from]]) < 1) {
			edges[[from]] <- NULL
		}
		if( length(edges[[to]]) < 1) {
			edges[[to]] <- NULL
		}
		return(edges)
	}
}

#remove edge but w/ each edge listed in both directions
remove_edge <- function(edges, to, from) {
	edges[[ from ]] <- edges[[ from ]][ which(edges[[ from ]] != to) ]
	edges[[ to ]] <- edges[[ to ]][ which(edges[[ to ]] != from) ]

	if( length(edges[[from]]) < 1) {
		edges[[from]] <- NULL
	}
	if( length(edges[[to]]) < 1) {
		edges[[to]] <- NULL
	}
	return(edges)
}

#return some "to" value if it exists
find_edge_obsolete <- function(edges, from) {
	from <- as.character(from)
	if( from %in% names(edges) && length(edges[[ from ]] ) > 0) {
		return( edges[[ from ]][1] )
	}
	else if ( from %in% unlist(edges) ) {
		#this is the shitty part, go thru list to find where from is
		for( name in names(edges) ) {
			if( from %in% edges[[ name ]] ) {
				return(name)
			}
		}
	}
	else { #there are no more edges we can go to
		return(NULL)
	}
}

#remove empty vectors from list
clean_list <- function(edges) { #OBSOLETE NOW
	for( name in names(edges) ) {
		if(length(edges[[name]]) < 1) {
			edges[[name]] <- NULL
		}
	}
	return(edges)
}

#input is P and Q as list of cycles
two_break_dist <- function(P, Q) {
	#find n_block by finding number of unique syntenty blocks 
	blocks <- c()
	for( cycle in P ) {
		for( i in names(P) ) {
			blocks <- c(blocks, P[[i]])
		}
	}
	for( cycle in Q ) {
		for( i in names(Q) ) {
			blocks <- c(blocks, Q[[i]])
		}
	}
	blocks <- abs(blocks)
	n_block <- length(unique(blocks))

	P_edges <- colored_edges(P)
	Q_edges <- colored_edges(Q)
	#combined graphs
	P_Q <- list()
	for( from in names(P_edges)) {
		P_Q[[ from ]] <- P_edges[[ from ]]
	}
	for( from in names(Q_edges)) {
		if( is.null(P_Q[[ from ]]) ) {
			P_Q[[ from ]] <- Q_edges[[ from ]]
		}
		else {
			P_Q[[ from ]] <- c(P_Q[[ from ]], Q_edges[[ from ]])
		}
	}

	#traverse thru cycles to find n_cycles
	n_cycle <- 0
	#I'm also going to make each edge double sided
	for( from in names(P_Q) ) {
		tos <- P_Q[[ from ]]
		for( to in tos ) {
			if( is.null(P_Q[[ to ]]) ) {
				P_Q[[ to ]] <- from
			}
			else {
				P_Q[[ to ]] <- c(P_Q[[ to ]], from)
			}
		}
	}

	while( length( names(P_Q) > 0 ) ) {
		#pick some edge, first in P_Q
		from <- names(P_Q)[1]
		to <- P_Q[[ from ]][1]
		#traverse edges until cannot
		while( !is.null(to) ) {
			P_Q <- remove_edge(P_Q, to, from)
			from <- to
			to <- P_Q[[ from ]][1]
		}
		#increment cycles
		n_cycle <- n_cycle + 1
	}
	dist <- n_block - n_cycle
	return(dist)
}

P <- parse_genome(P)
Q <- parse_genome(Q)
print(two_break_dist(P, Q))
