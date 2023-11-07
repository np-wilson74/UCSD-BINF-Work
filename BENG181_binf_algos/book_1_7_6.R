#Find minimum of skew
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

min_skew <- function(text, index1=F) {
	skew <- Skew_total(text)
	min <- min(skew)
	mins <- c()
	for(ind in 1:length(skew)) {
		if(skew[ ind ] == min) {
			mins <- c(mins, ind-1 + as.numeric(index1) )
		}
	}
	return(mins)
}

min_skew(text)
