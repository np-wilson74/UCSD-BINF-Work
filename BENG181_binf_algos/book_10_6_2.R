options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

suffix_array <- function(text, index0=T) {
	suffixes <- c()
	chars <- strsplit(text, "")[[1]]
	for( i in 1:length(chars) ) {
		suffix_chars <- chars[i:length(chars)]
		suffixes <- c(suffixes, paste(suffix_chars, collapse=''))
	}
	#I just found order kinda accidentally but it works
	return( order(suffixes)-as.numeric(index0) ) #subtract 1 for index0
}

print( paste(suffix_array(text), collapse=", ") )
