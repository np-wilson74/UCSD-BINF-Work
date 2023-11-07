setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
pattern <- lines[1]
text <- lines[2]
d <- as.numeric(lines[3])

#find starting of pattern or matches w/in distance d of pattern in text
approx_match <- function(pattern, text, d, index0=T) {
	matches <- c()
	n <- nchar(pattern)
	for( ind in 1:(nchar(text)-nchar(pattern)+1) ) {
		subset <- Text(text, ind, n, index0=F)
		if( Hamming(subset, pattern) <= d ) {
			#Need to substract 1 from index if using index0
			matches <- c(matches, ind-as.numeric(index0))	
		}
	}
	return(matches)
}

cat(approx_match(pattern,text,d))
