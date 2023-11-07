setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
k_d <- strsplit(lines[2], " ")[[1]]
k <- as.numeric(k_d[1])
d <- as.numeric(k_d[2])

most_freq_with_mismatches <- function(text, k, d) {

	if(F) { #Basically commenting out this block, this was the psuedocode solution
	patterns <- c()
	freq_map <- list()
	n <- nchar(text)
	for(i in 0:(n-k)) {
		pattern <- Text(text, i, k)
		neighborhood <- neighbors(pattern, d)
		for(j in 1:length(neighborhood)) {	
			neighbor <- neighborhood[j]
			if(is.null(freq_map[[ neighbor ]])) {
				freq_map[[ neighbor ]] <- 1
			}
			else {
				freq_map[[ neighbor ]] <- freq_map[[ neighbor ]] + 1
			}
		}
	}
	m <- max_set(freq_map)
	for( kmer in names(freq_map) ) {
		if( freq_map[[ kmer ]] == m ) {
			patterns <- c(patterns, kmer)
		}
	}
	return(patterns)
	} #END OF COMMENT BLOCK

	#back to my stuff
	#To anyone reading, my approach, vaguely speaking, was to create the frequency table and then 
	#Fill in the info for each kmer and neighbor based on that table

	#for each entry in frequency table (i.e. kmers actually in text)	
	freq_table <- frequency_table(text, k)
	og_freq_table <- freq_table	
	kmers <- unique(names(og_freq_table))
	for(kmer in kmers) {
		#find all it's neighbors
		neighbors <- neighbors(kmer, d)
		for(neighbor in neighbors) {
			#and then add frequency of kmer to that neighbor
			if( is.null(freq_table[[ neighbor ]]) ) {
				freq_table[[ neighbor ]] <- 0
			}
			freq_table[[ neighbor ]] <- freq_table[[ neighbor ]] + og_freq_table[[ kmer ]]
		}
	}

	max <- max_set(freq_table)
	maxs <- c()
	for(kmer in names(freq_table)) {
		if(freq_table[[ kmer ]] == max) {
			maxs <- c(maxs, kmer)	
		}
	}	
	return(maxs)
}

#Include with neighbor function
others <- list()
others[[ "A" ]] <- c("T", "C", "G")
others[[ "T" ]] <- c("A", "C", "G")
others[[ "C" ]] <- c("A", "T", "G")
others[[ "G" ]] <- c("A", "T", "C")
#Find all DNA codes w/ up to d mistakes from pattern
neighbors <- function(pattern, d) {
	neighbors <- c()
	pattern_vec <- strsplit(pattern, "")[[1]]
	for(ind in 1:length(pattern_vec)) {
		base <- pattern_vec[ind]
		for( other in others[[ base ]] ) {
			neighbor_vec <- pattern_vec
			neighbor_vec[ind] <- other
			#neighbor is a d1 neighbor
			neighbor <- paste(neighbor_vec, collapse='')
			#effectively the base case
			if(d==1) {
				neighbors <- c(neighbors, neighbor)
			}
			else {
				neighbors <- c(neighbors, neighbors(neighbor, d-1))
			}
		}
	}
	#Remove duplicates and pattern neighbors was called with
	neighbors <- neighbors[ neighbors != pattern ]
	return(unique(neighbors))
}

print(most_freq_with_mismatches(text, k, d))
