options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
patterns <- lines[2:length(lines)]

#return counts, a list of count arrays for every C indices
build_counts <- function(chars, C=1) {
	counts <- list()
	base_arr <- list()
	alphabet <- unique(chars)
	for( char in alphabet ) {
		base_arr[[ char ]] <- 0
	}
	counts[[ as.character(0) ]] <- base_arr
	prev_i <- 0
	for( i in seq(1, length(chars), C) ) {
		arr <- list()
		base <- counts[[ as.character(prev_i) ]]
		chars_sub <- chars[ (prev_i+1):i ] #consider removing the -1
		for( sym in alphabet ) {
			arr[[ sym ]] <- length(which(chars_sub==sym)) + base[[ sym ]]
		}
		counts[[ as.character(i) ]] <- arr
	}
	return(counts)
}

partial_suffix_array <- function(text, K) {
	suffix_array <- suffix_array(text)
	is <- which(suffix_array %% K == 0)
	return( setNames(is, suffix_array[is]) ) #This creates a named vector (value of i at suffix array name)
}

#index counts to find to find num occurances of sym until index i
count <- function(counts, i, chars, sym, is) {
	prevs <- is[which(as.numeric(is) <= i)]
	prev <- as.numeric(prevs[length(prevs)]) #this is closest index in counts to i
	base <- counts[[ as.character(prev) ]][[ sym ]]
	if( prev == i ) {
		return( base )
	} 
	else {
		return( base + length(which(chars[(prev+1):i]==sym)) )
	}
}

better_bw_match <- function(text, patterns) {
	text <- paste0(text, "$")
	suffix <- suffix_array(text)
	text <- burrows_wheeler_transform(text)
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

	chars_labelled <- chars
        chars <- chars_og
        #find occurences of patterns in text
	print("building counts ...")
        counts <- build_counts(chars, C=100) #TODO: change C to 100
	is <- names(counts)
	results <- c()
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
					top <- first_occured[[ sym ]] + count(counts, top-1, chars, sym, is)
					bottom <- first_occured[[ sym ]] + count(counts, bottom, chars, sym, is) - 1
                                }
                                else {
                                        #results <- c(results, 0)
                                        break
                                }
                        }
                        else {
                                results <- c(results, suffix[top:bottom]-1 )
                                break
                        }
                }
        }
        return(results)	
}

cat(better_bw_match(text,patterns))
