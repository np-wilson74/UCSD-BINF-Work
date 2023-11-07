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

#return # of breakpoints in permutation
n_breakpoints <- function(P) {
	P <- c(0, P, length(P)+1)
	count <- 0
	for( i in 1:(length(P)-1) ) {
		if( P[i] != (P[i+1]-1) ) {
			count <- count + 1
		}
	}
	return(count)
}

print(n_breakpoints(P))
