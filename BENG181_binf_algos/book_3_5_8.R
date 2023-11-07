setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
patterns <- readLines(fname)

de_brujin_from_patterns <- function(patterns) {
	de_brujin <- list()
	#Map suffix to prefix, adding it if prefix already exists in de_brujin
	for(pattern in patterns) {
		if( is.null(de_brujin[[ prefix(pattern) ]]) ) {
			de_brujin[[ prefix(pattern) ]] <- suffix(pattern)
		}
		else {
			de_brujin[[ prefix(pattern) ]] <- c(de_brujin[[ prefix(pattern) ]], suffix(pattern))
		}
	}	
	return(de_brujin)
}

print_graph(de_brujin_from_patterns(patterns))
