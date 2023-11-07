options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")
source("pam250.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

local_alignment <- function(v, w) {
	score_matrix <- pam250
        sig <- 5 #indel penalty 
	
	#This is mostly copied from book_5_8_5.R w/ some modifications
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)

        backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)
        rownames(backtrack) <- c( strsplit(v, "")[[1]],"-")
        colnames(backtrack) <- c( strsplit(w, "")[[1]],"-")
	#fill in first row and col of backtrack
        for( i in 2:(nchar(v)+1) ) { 
                backtrack[i, 1] <- "0" 
        }   
        for( j in 2:(nchar(w)+1) ) { 
                backtrack[1, j] <- "0" 
        }  

        #create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(w,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match, 0)
                        if( s[i, j] == s[i-1, j] - sig) {
                                backtrack[i, j] <- "d"
                        }
                        else if( s[i, j] == s[i, j-1] - sig) {
                                backtrack[i, j] <- "r"
                        }
                        else if( s[i, j] == (s[i-1, j-1] + match) ) {
                                backtrack[i, j] <- "dr"
                        }
			else if( s[i, j] == 0 ) {
				backtrack[i, j] <- "0"
			}
                }
        }
	max <- 0
	max_i <- 1
	max_j <- 1
	for( i in 1:(nchar(v)+1)) {
		for( j in 1:(nchar(w)+1) ) {
			if(s[i,j] > max) {
				max <- s[i,j]
				max_i <- i
				max_j <- j
			}	
		}
	}
	s[nchar(v)+1, nchar(w)+1] <- max

	#rebuild strings from backtrack graph
        v_new <- c() 
        w_new <- c() 
	i <- max_i
	j <- max_j
        #every time we cross row/col, we add that letter for cross
        #if we move & don't cross row/col, add a dash
        while( backtrack[i,j] != "" ) {
                if( backtrack[i, j] == "d") {
                        w_new <- c("-", w_new)
                        v_new <- c(char_at(v, i-1), v_new)
                        i <- i-1 
                        next
                }   
                else if( backtrack[i, j] == "r") {
                        v_new <- c("-", v_new)
                        w_new <- c(char_at(w, j-1), w_new)
                        j <- j-1
                        next
                }
                else if( backtrack[i, j] == "dr" ) {
                        v_new <- c(char_at(v, i-1), v_new)
                        w_new <- c(char_at(w, j-1), w_new)
                        i <- i-1
                        j <- j-1
                }
		else {
			i <- 1
			j <- 1
		}
        }
        return(c(s[nrow(s), ncol(s)], paste(v_new, collapse=''), paste(w_new, collapse='')))
}

print(local_alignment(v,w))

