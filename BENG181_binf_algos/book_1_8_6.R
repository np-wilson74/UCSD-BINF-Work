setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
pattern <- lines[1]
text <- lines[2]
d <- as.numeric(lines[3])

approx_count <- function(text, pattern, d) {
	#We can simply return number of elements in list from that
	return(length(approx_match(pattern, text, d)))	
}

print(approx_count(text,pattern,d))
