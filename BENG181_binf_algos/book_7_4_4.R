options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
P <- strsplit(text, " ")[[1]]
P <- as.numeric(P)

greedy_sort <- function(P) {
	rev_dist <- 0
	for( i in 1:length(P) ) {
		if( i == P[i] ) {
			next
		}
		else if( i != (-P[i]) ) {
			#start reverse at i, end it at place where i is in P
			P_loc <- which(P==i)
			if( length(P_loc) < 1 ) {
				P_loc <- which(P==(-i))
			}
			P <- P_rev(P, i, P_loc)
			rev_dist <- rev_dist + 1
			cat( P_str(P), file="soln_7_4_4.txt", append=T, sep="\n" )
		}
		if( i == (-P[i]) ) {
			P[i] <- -P[i]
			rev_dist <- rev_dist + 1
			cat( P_str(P), file="soln_7_4_4.txt", append=T, sep="\n" )
		}
	}
}

#represent permutation as string
P_str <- function(P) {
	str <- c()
	for( num in P ) {
		if( num > 0 ) {
			str <- c(str, paste0("+", num))
		}
		else {
			str <- c(str, num)
		}
	}
	return( paste(str, collapse=" ") )
}

#reverse some segment of P, from start to end (inclusive)
P_rev <- function(P, start, end) {
	segment <- P[start:end]
	rev_segment <- c()
	for( term in segment ) {
		rev_segment <- c(term, rev_segment)
	}
	P[start:end] <- -rev_segment
	return(P)
}


greedy_sort(P)

