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

most_freq_with_mistake_and_comp <- function(text, k, d) {
        #for each entry in frequency table (i.e. kmers actually in text)        
        freq_table <- frequency_table(text, k)
        og_freq_table <- freq_table    
        kmers <- unique(names(og_freq_table))
        for( kmer in kmers ) {
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

	#Add results of complements to each other - addition from 1_8_9 function
	fin_freq_table <- freq_table
	for( kmer in names(fin_freq_table) ) {
		comp <- make_comp(kmer)
		if( is.null(freq_table[[ comp ]]) ) {
			freq_table[[ comp ]] <- 0
		}
		freq_table[[ comp ]] <- freq_table[[ comp ]] + fin_freq_table[[ kmer ]]
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

print(most_freq_with_mistake_and_comp(text,k,d))
