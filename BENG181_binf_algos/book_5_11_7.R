options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

build_score_matrix <- function(alphabet_str, match, mismatch) {
	alphabet <- strsplit(alphabet_str, "")[[1]]
	score_matrix <- matrix(mismatch, nrow=length(alphabet), ncol=length(alphabet), dimnames=list(alphabet, alphabet))
	for( i in 1:length(alphabet) ) {
		score_matrix[i,i] <- match
	}
	return(score_matrix)
}

overlap_align <- function(v, w) {
	sig <- 2
	score_matrix <- build_score_matrix("ABCDEFGHIJKLMNOPQRSTUVWXYZ", 1, -2)
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
	print(s)
        v_new <- c() 
        w_new <- c() 
        #find max score in bottom row then backtrack from there
        max <- max(s[,ncol(s)])
        max_is <- which( s[,ncol(s)]==max )
	max_i <- max_is[length(max_is)]
	i <- max_i
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
        return(c(max, paste(w_new, collapse=''), paste(v_new, collapse='')))
}

print(overlap_align(w,v))
