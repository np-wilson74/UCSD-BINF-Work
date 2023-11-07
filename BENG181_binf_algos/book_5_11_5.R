options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

score_matrix <- matrix((-1), nrow=4, ncol=4, dimnames=list(c("A","T","C","G"),c("A","T","C","G")))
for( i in 1:4 ) {
	score_matrix[i,i] <- 1
}

#modify global search, instead of starting at (1,1) and ending at (n,m), 
#I want to be able to start on the top row and end anywhere on bottom row
fit_alignment <- function(v,w) {
	sig <- 1
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)

        backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)
        rownames(backtrack) <- c(strsplit(v, "")[[1]], "-")
        rownames(s) <- c(strsplit(v, "")[[1]], "-")
        colnames(backtrack) <- c(strsplit(w, "")[[1]], "-")
        colnames(s) <- c(strsplit(w, "")[[1]], "-")
        #fill in first row and col of backtrack
        for( i in 2:(nchar(v)+1) ) { 
                backtrack[i, 1] <- "d" 
        }   
        for( j in 2:(nchar(w)+1) ) { 
                backtrack[1, j] <- "" 
        }   

        for( i in 2:(nchar(v)+1) ) { 
                s[i, 1] <- s[i-1, 1] - sig
        }   
        
	#create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(w,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match)
                        if( s[i, j] == s[i-1, j] - sig) {
                                backtrack[i, j] <- "d"
                        }
                        else if( s[i, j] == s[i, j-1] - sig) {
                                backtrack[i, j] <- "r"
                        }
                        else if( s[i, j] == (s[i-1, j-1] + match) ) {
                                backtrack[i, j] <- "dr"
                        }
                }
        }
	#rebuild strings from backtrack graph
        v_new <- c() 
        w_new <- c() 
	#find max score in bottom row then backtrack from there
	max <- max(s[nrow(s),])
	max_j <- which( s[nrow(s),]==max )[1]
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
			next 
                }   
		else if( backtrack[i, j] == "" ) {
			j <- j-1
		}
        }   
        return(c(s[nrow(s), max_j], paste(w_new, collapse=''), paste(v_new, collapse='')))
}

#writing this here to put into book_functions.R
#build score matrix for 2 strings given indel/mismatch penalty
#this version will assume we're going from (1,1) to (m,n) w/o 'free rides' anywhere
build_s <- function(v, w, sig, match, mismatch, score_matrix=NULL, alphabet="ACTG") {
	s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
        rownames(s) <- c(strsplit(v, "")[[1]], "-")
        colnames(s) <- c(strsplit(w, "")[[1]], "-")
       
	if( is.null(score_matrix) ) {
		score_matrix <- build_score_matrix(alphabet, match, mismatch)
	}
 
        for( i in 2:(nchar(v)+1) ) { 
                s[i, 1] <- s[i-1, 1] - sig 
        }   
	for( j in 2:(nchar(w)+1) ) {
		s[1, j] <- s[1, j-1] - sig
	}
    
        #create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(w,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match)
                }   
        }   
}

print(fit_alignment(w,v))
