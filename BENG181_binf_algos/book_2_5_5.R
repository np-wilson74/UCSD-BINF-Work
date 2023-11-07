setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric( strsplit(lines[1], " ")[[1]][1] )
t <- as.numeric( strsplit(lines[1], " ")[[1]][2] )
dna <- lines[2:length(lines)]

greedy_motif_search <- function(dna, k, t) {
	#get all kmer motifs in first string
	first_motifs <- names( frequency_table(dna[1], k) )
	best_motifs <- list()

	#see pseudo-code
	for( f_motif in first_motifs ) {
		temp_motifs <- list()
		temp_motifs[[1]] <- f_motif
		for( i in 2:t ) {
			profile <- profile_from_motifs( unlist(temp_motifs), naive=T )
			greedy_choice <- profile_most_probable(dna[i], k, profile)
			temp_motifs[[i]] <- greedy_choice
		}
		if( score(temp_motifs) < score(best_motifs) ) {
			best_motifs <- temp_motifs
		}
	}
	return(unlist(best_motifs))
}

#assume all motifs same length
#if naive, don't add 1 to each count
#return_count will return the count matrix
profile_from_motifs <- function(motifs, naive=F, return_count=F) {
	#generate count matrix
	k <- nchar(motifs[1])
	#creates 4*k matrix of 0s or 1s
	#if not naive, then adding the 1s increases total_entries in each col by 4
	if(naive) {
		count <- matrix( rep(0, len=k*4), nrow=4 )
		total_entries <- length(motifs)
	} else {
		count <- matrix( rep(1, len=k*4), nrow=4 )
		total_entries <- length(motifs) + 4
	}
	rownames(count) <- c("A","C","G","T")
	for(motif in motifs) {
		bases <- strsplit(motif, "")[[1]]
		for(i in 1:length(bases)) {
			count[ bases[i] ,i] <- count[ bases[i] ,i] + 1
		}
	}

	if(return_count) {return(count)}

	#build profile
	profile <- apply(count, 1:2, function(x) x/total_entries)
	return(profile)
}

#score motif matrix (count number of 'lower case' bases that don't match most freq)
score <- function(motifs) {
	if(length(motifs) < 1) {return(999999)}
	count <- profile_from_motifs(motifs, naive=T, return_count=T)
	col_score <- c()
	
	for( i in 1:ncol(count) ) {
		#in each column, 10*(1-max_freq) is num 'lower case' entries
		col_score <- c(col_score, sum(count[,i]) - max(count[,i]) )
	}
	return(sum(col_score))
}

write(greedy_motif_search(dna,k,t), "soln_2_5_5.txt")
