options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

text_to_chromosome <- function(text) {
	text_chrs <- strsplit(text, "")[[1]]
	text_chrs <- text_chrs[2:(length(text_chrs)-1)]
	text <- paste(text_chrs, collapse="")
	chromosome <- as.integer(strsplit(text, " ")[[1]])
	print(chromosome)
	return(chromosome)
}

chromosome_to_cycle <- function(chromosome) {
	cycle <- c()
	for(num in chromosome) {
		if(num < 0) {
			num <- -num
			cycle <- c(cycle, 2*num, (2*num)-1)
		}
		else {
			cycle <- c(cycle, (2*num)-1, 2*num)
		}
	}
	return(cycle)
}

print(paste0("(",paste(chromosome_to_cycle(text_to_chromosome(text)),collapse=' '),")"))
