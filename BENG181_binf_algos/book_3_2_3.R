setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])
text <- lines[2]

composition <- function(k, text) {
	pieces <- c()
	for(ind in 0:(nchar(text)-k)) {
		pieces <- c(pieces, Text(text, ind, k))	
	}
	return(pieces)
}
write.table(composition(k,text), file="book_3_2_3.txt", quote=F, sep=" ", row.names=F, col.names=F)
