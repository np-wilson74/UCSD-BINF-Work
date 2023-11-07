##I will again denote my new comments w/ double hashmark "##"
options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")
source("blosum62.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
w <- lines[1]
v <- lines[2]

global_align <- function(w,v) {
	#score_matrix <- blosum62  ##adjust score matrix as specified by problem
	score_matrix <- build_score_matrix("ATCG", 1, -1)  ##This function in book_functions.R
	sig <- 1 #indel penalty

	#This is mostly copied from book_5_8_5.R w/ some modifications
	s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
	#calc first row & col (all indels)
	for( i in 2:(nchar(v)+1) ) {
		s[i, 1] <- s[i-1, 1] - sig
	}
	for( j in 2:(nchar(w)+1) ) {
		s[1, j] <- s[1, j-1] - sig
	}

        backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)
	rownames(backtrack) <- c( strsplit(v, "")[[1]],"-")
	colnames(backtrack) <- c( strsplit(w, "")[[1]],"-")
	#fill in first row and col of backtrack
	for( i in 2:(nchar(v)+1) ) {
		backtrack[i, 1] <- "V"
	}
	for( j in 2:(nchar(w)+1) ) {
		backtrack[1, j] <- "H"
	}

	##Below is where the majority of my work will be
	##What I want to do is count the number of branching paths as I construct backtrack
	n_paths <- 1

	#create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(w,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match)
			backtrack[i,j] <- "" ##Instead we're now keeping track of all directions a node came from
                        if( s[i, j] == s[i-1, j] - sig) { 
                                backtrack[i, j] <- paste0(backtrack[i,j], "V") ##R is funky and won't let me put a vector in a matrix, so I'm changing the naming of these used and concatenating them
                        }   
                        if( s[i, j] == s[i, j-1] - sig) { ##remove else here
                                backtrack[i, j] <- paste0(backtrack[i,j], "H") 
                        }   
                        if( s[i, j] == (s[i-1, j-1] + match) ) { ##remove else here
                                backtrack[i, j] <- paste0(backtrack[i,j], "D")
                        }   
                }   
        }  

	##I know want to go from right to left, bottom to top thru backtrack
	##If the one below, right, or bottom-right of a given node points to it, then we increment it's score
	##Bottom right corner will have score of 1
	num_paths <- matrix(0, nrow=(nchar(v)+1), ncol=(nchar(w)+1))
	num_paths[ nchar(v)+1, nchar(w)+1 ] <- 1
	##deal with first col and row (bottom and right ones, respectively)
	for(i in (nchar(v)):1 ) { ##This will be last col
		prev_letters <- strsplit(backtrack[i+1, nchar(w)+1], "")[[1]]
		if( "V" %in%  prev_letters ) {
			num_paths[ i, nchar(w)+1 ] <- 1
		}
		else {
			break ##if  we get here there's  no path getting anywhere further  up
		}
	}
	for( j in (nchar(w)):1 ) { ##This is last row
		prev_letters <- strsplit(backtrack[ nchar(v)+1, j+1], "")[[1]]
		if( "H" %in% prev_letters ) {
			num_paths[ nchar(v)+1, j ] <- 1
		}
		else {
			break
		}
	}

	for( i in nchar(v):1 ) {  ##This is starting from node diagonal to bottom right
		for( j in nchar(w):1 ) {
			prev_letters_D <- strsplit(backtrack[ i+1, j+1], "")[[1]]
			prev_letters_H <- strsplit(backtrack[ i, j+1], "")[[1]]
			prev_letters_V <- strsplit(backtrack[ i+1, j], "")[[1]]
			
			D <- 0
			H <- 0  ##paths contributed from respective directions
			V <- 0
			
			if( "D" %in% prev_letters_D ) {
				D <- num_paths[i+1, j+1]
			}
			if( "H" %in% prev_letters_H ) {
				H <- num_paths[i, j+1]
			}
			if( "V" %in% prev_letters_V ) {
				V <- num_paths[i+1, j]
			}
			
			num_paths[i,j] <- sum(D, H, V)
		}
	}
	
	return(num_paths[1,1])
	if(F) { ##I  just don't want any of this stuff running since there's no need
	#rebuild strings from backtrack graph
	v_new <- c()
	w_new <- c() 
	#every time we cross row/col, we add that letter for cross
	#if we move & don't cross row/col, add a dash
	print(i)
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
                else {
                        v_new <- c(char_at(v, i-1), v_new)
                        w_new <- c(char_at(w, j-1), w_new)
                        i <- i-1 
                        j <- j-1 
                }   
        }   
        return(c(s[nrow(s), ncol(s)], paste(v_new, collapse=''), paste(w_new, collapse='')))
	}
}

print(global_align(v,w))
