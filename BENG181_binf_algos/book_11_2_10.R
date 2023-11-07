options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

rev_burrows_wheeler <- function(str) {
	chars <- strsplit(str, "")[[1]]
	first <- chars[order(chars)]
	f_labelled <- c()
	c_labelled <- c()
	#label instances each letter of my first and last col
	for( i in 1:length(first) ) {
		f_labelled <- c(f_labelled, first[i])
		c_labelled <- c(c_labelled, chars[i])
		first[i] <- paste0(first[i], length(which(f_labelled==first[i])))
		chars[i] <- paste0(chars[i], length(which(c_labelled==chars[i])))
	}

	#untransform by adding where current char in first maps to chars
	rev <- c(first[ which(chars=="$1") ])
	while( rev[length(rev)] != "$1" ) {
		rev <- c(rev, first[ which(chars==rev[length(rev)]) ])
	}

	#remove numbers from result
	rev_no_nums <- c()
	for( letter in rev ) {
		rev_no_nums <- c(rev_no_nums, substr(letter, 1, 1))
	}

	return( paste(rev_no_nums, collapse='') )
}

print(rev_burrows_wheeler(text))
