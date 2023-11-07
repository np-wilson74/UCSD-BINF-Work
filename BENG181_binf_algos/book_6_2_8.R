options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")
source("blosum62.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

align_with_affine_gaps <- function(v, w, score_matrix=blosum62, sig=11, ep=1) {
	lower <- matrix(-999, nrow=nchar(v)+1, ncol=nchar(w)+1)
	middle <- matrix(-999, nrow=nchar(v)+1, ncol=nchar(w)+1)
	upper <- matrix(-999, nrow=nchar(v)+1, ncol=nchar(w)+1)

	middle[1,1] <- 0
	upper[2, 1] <- middle[1,1] - sig
	lower[1, 2] <- middle[1,1] - sig

	#set first row and col
	#I dont particularly care about first row of lower or first col of upper
	for( i in 3:(nchar(v)+1) ) {
		lower[ i, 1 ] <- lower[ i-1, 1 ] - ep
		middle[ i, 1 ] <- lower[ i, 1 ] 
	}
	
	for( j in 3:(nchar(w)+1) ) {
		upper[ 1, j ] <- upper[ 1, j-1 ] - ep
		middle[ 1, j ] <- upper[ 1, j-1 ]
	}

	for( i in 2:(nchar(v)+1) ) {
		for( j in 2:(nchar(w)+1) ) {
			lower[ i, j ] <- max( lower[i-1, j] - ep, middle[i-1, j] - sig )
			upper[ i, j ] <- max( upper[i, j-1] - ep, middle[i, j-1] - sig )
			match <- score_matrix[ char_at(v, i-1), char_at(w, j-1) ]
			middle[ i, j ] <- max( lower[i,j], upper[i,j], middle[i-1, j-1] + match )
			#do middle last	
		}
	}

	#backtrack thru graph
	backtrack <- c()
	i <- nchar(v)+1 #start @ end
	j <- nchar(w)+1
	curr_matx <- "middle"

	# D represents diagonal, H represents horizontal (or mid to upper), V represents vertical (or mid to lower)
	while( i != 1 && j != 1 ) {
		if( curr_matx == "middle" ) {
			match <- score_matrix[ char_at(v, i-1), char_at(w, j-1) ]
			if( middle[i,j] == middle[i-1, j-1] + match ) {
				backtrack <- c("D", backtrack)
				i <- i - 1
				j <- j - 1
				next
			}
			else if( middle[i,j] == upper[i,j] ) {
				curr_matx <- "upper"
				next
			}
			else if( middle[i,j] == lower[i,j] ) {
				curr_matx <- "lower"
				next
			}
			else {
				print("bad shit at curr_matx == middle")
				break
			}
		}
		else if( curr_matx == "upper" ) {
			if( upper[i,j] == upper[i, j-1] - ep ) {
				backtrack <- c("H", backtrack)
				j <- j - 1
				next
			}
			else if( upper[i,j] == middle[i, j-1] - sig ) {
				backtrack <- c("H", backtrack)
				j <- j - 1
				curr_matx <- "middle"
				next
			}
			else {
				print("bad shit at curr_matx == upper")
				break
			}
		}
		else if( curr_matx == "lower" ) {
			if( lower[i,j] == lower[i-1, j] - ep ) {
				backtrack <- c("V", backtrack)
				i <- i - 1
				next
			}
			else if( lower[i,j] == middle[i-1, j] - sig ) {
				backtrack <- c("V", backtrack)
				i <- i - 1
				curr_matx <- "middle"
				next
			}
			else {
				print("bad shit at curr_matx == lower")
				break
			}

		}
		else {
			print(curr_matx)
			print("curr_matx being wonky")
			break
		}
	}
	#backtrack is already in order
	return( c(middle[nchar(v)+1, nchar(w)+1], parse_alignment_path(backtrack, v, w)) )
}

print(align_with_affine_gaps(v,w))
