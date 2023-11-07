options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
patterns <- lines[2]
patterns <- strsplit(patterns, " ")[[1]]

match1 <- function( char1, char2 ) {
	return( substr(char1,1,1) == substr(char2,1,1) )
}

bw_match <- function(text, patterns) {
	chars <- strsplit(text, "")[[1]]
	chars_og <- chars #likewise need  an original copy for later
	first <- chars[order(chars)]	
	first_og <- first #I just need a copy of this

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
	for( pattern in patterns ) {
		p_chars <- strsplit(pattern, "")[[1]]
		top <- 1
		bottom <- length(chars)
		while(top <= bottom) {
			if( length(p_chars) > 0 ) {
				sym <- p_chars[length(p_chars)]
				p_chars <- p_chars[-(length(p_chars))]
				if( sym %in% chars[top:bottom] ) {
					occurances <- which(sym == chars[top:bottom])	
					top_ind <- top - 1 + occurances[1]
					bottom_ind <- top - 1 + occurances[length(occurances)]
					top <- last_to_first[top_ind]
					bottom <- last_to_first[bottom_ind]
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

cat( bw_match(text,patterns) )
