setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
patterns <- readLines(fname)

prefix <- function(pattern) {
	k <- nchar(pattern)
	pre <- substr(pattern, 1, k-1)
	return(pre)
}

suffix <- function(pattern) {
	k <- nchar(pattern)
	suf <- substr(pattern, 2, k)
	return(suf)
}

#overlap in list format
overlap <- function(patterns) {
	overlap <- list()
	for(pattern in patterns) {	
		to <- patterns[ suffix(pattern) == prefix(patterns) ]
		if( length(to) > 0 ) {
			overlap[[ pattern ]] <- to
		}
	}
	return(overlap)
}

#overlap in 'pattern -> to' format
print_graph <- function(overlap) {
	for(pattern in names(overlap)) {
		cat( paste0(pattern, " -> ", paste(overlap[[pattern]], collapse=","), "\n") )
	}	
}

print_graph(overlap(patterns))

