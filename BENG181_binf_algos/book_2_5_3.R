setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
k <- as.numeric(lines[2])
profile_data <- strsplit(lines[3:length(lines)], " ")
profile <- as.data.frame(do.call(rbind, profile_data))
rownames(profile) <- c("A","C","G","T")

#return most probably kmer in text from profile
profile_most_probable <- function(text, k, profile) {
	n <- nchar(text)
	probs <- c()
	#get probability of all kmers in text
	for( i in 1:(n-k+1) ) {
		window <- Text(text, i, k, index0=F)
		probs <- c(probs, str_prob(window, profile))
	}

	#pick out max
	max_ind <- which(probs == max(probs))[1]
	return( Text(text, max_ind, k, index0=F) )
}

#probability of pattern given profile, assumes ncol(profile) == nchar(pattern)
str_prob <- function(pattern, profile) {
	prob <- 1
	pattern_vec <- strsplit(pattern, "")[[1]]
	for( i in 1:nchar(pattern) ) {
		prob <- prob * as.numeric(profile[ pattern_vec[i], ][i])
	}
	return(prob)
}

print(profile_most_probable(text,k,profile))
