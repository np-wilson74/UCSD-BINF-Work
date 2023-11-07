options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
text_chrs <- strsplit(text,"")[[1]]
text <- paste(text_chrs[2:(length(text_chrs)-1)], collapse='')
cycle <- as.numeric(strsplit(text, " ")[[1]])

#output vector chromosome as "(+1 -2 -3 +4)" form
print_chromosome <- function(chromosome) {
	str <- c()
	for( block in chromosome ) {
		if( block > 0 ) {
			str <- c(str, paste0("+", block))
		}
		else {
			str <- c(str, block)
		}
	}
	str <- paste(str, collapse=" ")
	return(paste0("(", str, ")"))
}

cycle_to_chromosome <- function(cycle) {
	chromosome <- c()
	for( i in seq(1, length(cycle), by=2) ) {
		if( cycle[i] < cycle[i+1] ) {
			chromosome <- c(chromosome, (cycle[i+1]/2))
		}
		else {
			chromosome <- c(chromosome, -(cycle[i]/2))
		}
	}
	return(chromosome)
}

print(print_chromosome(cycle_to_chromosome(cycle)))
