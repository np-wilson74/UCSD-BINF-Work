setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
print(text)

#Skew at every position in text
Skew_total <- function(text) {
	skews <- c(0)
	text_vec <- strsplit(text, '')[[1]]

	for(base in text_vec) {
		if(base == "A" || base == "T") {
			skews <- c(skews, skews[ length(skews) ])
		}
		else if(base == "C") {
			skews <- c(skews, skews[ length(skews) ]-1 )
		}
		else if(base == "G") {
			skews <- c(skews, skews[ length(skews) ]+1 )
		}
	}
	return(skews)
}

#index1 = T assumes that the empty string is at position ind=1
Skew <- function(text, ind, index1 = F) {
	#Return item from total at index
	total_skews <- Skew_total(text)
	if(index1) {
		return( total_skews[ ind ] )
	}
	else {
		return( total_skews[ ind+1 ] )	
	}
}

print(Skew_total(text))
