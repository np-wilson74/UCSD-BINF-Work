options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

#just a helper to get char at certain index in str
#at some point try looking into operator overloading
char_at <- function(str, ind) {
	return(substr(str, ind, ind))
}

#create DAG where each entry is where to come from to maximize length of LCS
LCS_backtrack <- function(v, w) {
	s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
	backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)

	for( i in 2:(nchar(v)+1) ) {
		for( j in 2:(nchar(w)+1) ) {
			match <- 0
			if( char_at(v, i-1) == char_at(w, j-1) ) {
				match <- 1
			}
			s[i, j] <- max(s[i-1, j], s[i, j-1], s[i-1, j-1] + match)
			if( s[i, j] == s[i-1, j] ) {
				backtrack[i, j] <- "d"
			}
			else if( s[i, j] == s[i, j-1] ) {
				backtrack[i, j] <- "r"
			}
			else if( s[i, j] == (s[i-1, j-1] + match) ) {
				backtrack[i, j] <- "dr"
			}
		}
	}
	return(backtrack)
}

#given the backtrack graph, start from end and figure out path of LCS
output_LCS <- function(backtrack, v, i, j) {
	if(F) { #COMMENT BLOCK
	if( i == 1 || j == 1 ) {return("")}
	if( backtrack[i, j] ==  "d" ) {
		return( output_LCS(backtrack, v, i-1, j) )
	}
	else if( backtrack[i, j] ==  "r" ) {
		return( output_LCS(backtrack, v, i, j-1) )
	}
	else {
		return( paste0(output_LCS(backtrack, v, i-1, j-1),  char_at(v, i-1)) )
	}
	} #BLOCK ENDS HERE - CODE WORKS BUT USES WAY TOO MUCH MEMORY

	#procedural method
	str <- c()
	while( i != 1 && j != 1 ) {
		if( backtrack[i, j] == "d") {
			i <- i-1
			next
		}
		else if( backtrack[i, j] == "r") {
			j <- j-1
			next
		}
		else {
			str <- c(char_at(v, i-1), str)
			i <- i-1
			j <- j-1
		}
	}
	return(paste(str, collapse=''))
}

backtrack <- LCS_backtrack(v,w)
print(output_LCS(backtrack, v, nchar(v)+1, nchar(w)+1))
