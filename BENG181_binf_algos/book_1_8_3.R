setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text1 <- lines[1]
text2 <- lines[2]

Hamming <- function(text1, text2) {
	text1_vec <- strsplit(text1, '')[[1]]
	text2_vec <- strsplit(text2, '')[[1]]
	if( length(text1_vec) != length(text2_vec) ) {
		print("Strings must be same length!")
		q()
	}
	dist <- 0
	for( ind in 1:length(text1_vec) ) {
		if(text1_vec[ind] != text2_vec[ind]) {
			dist <- dist + 1
		}
	}
	return(dist)
}

print(Hamming(text1,text2))
