setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
nums <- lines[1]
strs <- lines[2:length(lines)]
k <- as.numeric(strsplit(nums, " ")[[1]][1])
d <- as.numeric(strsplit(nums, " ")[[1]][2])

#return kmer w/ d mistakes in all strings
motif_enumeration <- function(strs, k, d) {
	patterns <- c()
	
	#get collection of all k-mers in strs
	k_mers <- c()
	for( str in strs ) {
		freq_table <- frequency_table(str, k)		
		k_mers <- c(k_mers, names(freq_table))
	}	
	
	tried <- c()	
	for( k_mer in k_mers ) {
		neighbors <- c(k_mer, neighbors(k_mer, d))
		for( neighbor in neighbors ) {
			if(neighbor %in% tried) {next}
			tried <- c(tried, neighbor)
			in_str <- c()
			for( str in  strs ) {
				freq_table <- frequency_table(str, k)
				in_str <- c(in_str, any(c(neighbor, neighbors(neighbor, d)) %in% names(freq_table)))
			}
			if( all(in_str) ) {
				patterns <- c(patterns, neighbor)
			}
		}
	}

	return(unique(patterns))
}

write(motif_enumeration(strs,k,d), file="soln_2_2_8.txt", sep=" ")
