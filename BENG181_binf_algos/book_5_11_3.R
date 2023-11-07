options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

#solve edit distance problem - largely copied from LCS_backtrack
edit_dist <- function(v, w) {
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
	for( i in 2:(nchar(v)+1) ) {
		s[i, 1] <- i-1 
	}
	for( j in 2:(nchar(w)+1) ) {
		s[1, j] <- j-1
	}

        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        mismatch <- 1
                        if( char_at(v, i-1) == char_at(w, j-1) ) { 
                               mismatch <- 0
                        }   
                        s[i, j] <- min(s[i-1, j]+1, s[i, j-1]+1, s[i-1, j-1] + mismatch)
                }   
        }   
        return(s[nrow(s), ncol(s)])
}

print(edit_dist(v,w))
