options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(strsplit(lines[1], " ")[[1]][1])
t <- as.numeric(strsplit(lines[1], " ")[[1]][2])
dna <- lines[2:length(lines)]

#only 1 instance of randomized_motif_search
randomized_motif_search <- function(dna, k, t) {
	#select random motifs in dna
	motifs <- c()
	for( string in dna ) {
		n <- nchar(string)
		start_ind <- sample(1:(n-k+1), 1)
		motifs <- c(motifs, Text(string, start_ind, k, index0=F))
	}
	best_motifs <- motifs
	best_score <- score(best_motifs)
	while(T) {
		profile <- profile_from_motifs(motifs)
		motifs <- profile_most_probable_dna(dna, k, profile)
		score <- score(motifs)
		if( score < best_score ) {
			best_motifs <- motifs
			best_score <- score
		}
		else {
			return(c(best_motifs, best_score))
		}
	}
}

#repeat randomized_motif_search n times
randomized_motif_search_rep <- function(dna, k, t, n) {
	best_motif <- c()
	best_score <- score(best_motif)
	for( i in 1:n ) {
		new_results <- randomized_motif_search(dna, k, t)
		new_motif <- new_results[1:(length(new_results)-1)]
		new_score <- new_results[length(new_results)]
		if(new_score < best_score) {
			best_motif <- new_motif
			best_score <- new_score
		}
	}
	return(best_motif)
}

#differs from one in book_functions by getting motifs from dna collection
profile_most_probable_dna <- function(dna, k, profile) {
	motifs <- c()
	for( str in dna ) {
		motifs <- c(motifs, profile_most_probable(dna, k, profile))
	}
	return(motifs)
}

#print(randomized_motif_search_rep(dna, k, t, 500))
write(randomized_motif_search_rep(dna, k, t, 750), "soln_2_7_5.txt")
