options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
cycles <- strsplit(text, ")")[[1]]
chromosomes <- list()
for( i in 1:length(cycles) ) {
	cycles[i] <- substr( cycles[i], 2, nchar(cycles[i]) ) #remove first "("
	chromosomes[[ i ]] <- as.numeric(strsplit(cycles[i], " ")[[1]])
}

#Chromosomes is a list of vectors w/ each chromosomes sytany blocks
colored_edges <- function(chromosomes) {
	edges <- list()
	for( chromosome in chromosomes ) {
		cycle <- chromosome_to_cycle(chromosome)
		for( i in seq( 2, length(cycle)-1, by=2 ) ) {
			edges[[ as.character(cycle[i]) ]] <- as.character(cycle[i+1])
		}
		edges[[ as.character(cycle[length(cycle)]) ]] <- as.character(cycle[1])
	}	
	return(edges)
}

print_colored_edges <- function(edges) {
	str <- c()
	for( i in names(edges) ) {
		str <- c(str, paste0("(", i,", ", edges[[i]], "), "))
	}
	str <- paste(str, collapse='')
	str <- substr(str, 1, nchar(str)-2)
	return(str)
}

print(print_colored_edges(colored_edges(chromosomes)))
