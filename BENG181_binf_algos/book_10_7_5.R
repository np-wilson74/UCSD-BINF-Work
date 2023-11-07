options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

burrows_wheeler_transform <- function(text) {
	#create cyclic rotations
	chars <- strsplit(text, "")[[1]]
	rotations <- c(text)
	for( i in 2:length(chars) ) {
		prev <- rotations[length(rotations)]
		prev_chars <- strsplit(prev, "")[[1]]
		rotate_chars <- prev_chars[ c(length(prev_chars), 1:(length(prev_chars)-1)) ]
		rotations <- c(rotations, paste(rotate_chars, collapse=''))
	}
	order <- order(rotations)
	rotations <- rotations[order] #order rotations
	#pull out last char from each
	transform <- c()
	for( rotation in rotations ) {
		transform <- c(transform, substr(rotation, nchar(rotation), nchar(rotation)))
	}
	return(paste(transform, collapse=''))
}

print( burrows_wheeler_transform(text) )
