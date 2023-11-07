setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])
dna <- lines[2:length(lines)]

#return kmer with lowest distance in all dna strings
median_str <- function(dna, k) {
	distance <- 999999
	median <- ""
	#get all k_mers to check - for now I'll just do all of them
	k_mers <- c("A","C","G","T")
	for( i in 2:k ) {
		for( k_mer in k_mers ) {
			for( base in c("A","C","T","G") ) {
				k_mers <- c(k_mers, paste0(k_mer, base)) 
			}
		}
		k_mers <- k_mers[ which(nchar(k_mers) == i) ]
	}
	
	for( k_mer in k_mers ) {
		kmer_dist <- d_sum(k_mer, dna)
		if( distance > kmer_dist ) {
			distance <- kmer_dist
			median <- k_mer
		}
	}
	return( median )
}

print(median_str(dna,k))
