options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
patterns <- strsplit(lines[2], " " )[[1]]

base_counts <- list()
for( char in unique(strsplit(text, "")[[1]]) ) {
	base_counts[[ char ]] <- 0
}

#return count of a symbol in text from 1:i
#arr returns list of count of each char in text from index 1:i
count <- function(i, text, sym, chars) {
	chars <- chars[1:(i-1)]
	return( length(which(chars == sym)) )
}

better_bw_match <- function(text, patterns) {
	chars <- strsplit(text, "")[[1]]
        chars_og <- chars #need an original copy for later
        first <- chars[order(chars)]    

	first_occured <- list()
	#build first_occured list
	for( char in unique(chars) ) {
		first_occured[[ char ]] <- which( first == char )[1]
	}
        
	#need labelled copy of chars to build last_to_first
        f_labelled <- c() 
        c_labelled <- c() 
        #label instances each letter of my first and last col
        for( i in 1:length(first) ) { 
                f_labelled <- c(f_labelled, first[i])
                c_labelled <- c(c_labelled, chars[i])
                first[i] <- paste0(first[i], length(which(f_labelled==first[i])))
                chars[i] <- paste0(chars[i], length(which(c_labelled==chars[i])))
        }   

        last_to_first <- c() 
        for( last in chars ) { 
                last_to_first <- c(last_to_first, which(last==first))
        }   

        chars <- chars_og
        #find occurences of patterns in text
        counts <- c()
	k <- 1
	end <- length(chars)
	print("starting comparisons ... ")
	for( pattern in patterns ) {
		k <- k + 1
                p_chars <- strsplit(pattern, "")[[1]]
                top <- 1
                bottom <- end
                while(top <= bottom) {
                        if( length(p_chars) > 0 ) {
                                sym <- p_chars[length(p_chars)]
                                p_chars <- p_chars[-(length(p_chars))]
                                if( sym %in% chars[top:bottom] ) {
					top <- first_occured[[ sym ]] + count(top, text, sym, chars)
					bottom <- first_occured[[ sym ]] + count(bottom+1, text, sym, chars) - 1
                                }
                                else {
                                        counts <- c(counts, 0)
                                        break
                                }
                        }
                        else {
                                counts <- c(counts, bottom - top + 1)
                                break
                        }
                }
        }
        return(counts)	
}

cat(better_bw_match(text,patterns))
