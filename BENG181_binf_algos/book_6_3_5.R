options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]
x <- lines[3]

#v,w,x are strings; i,j,j are position in matrix;v_id, w_id, x_id are if last char is indel 
mult_score <- function(v, w, x, i, j, k, v_id, w_id, x_id) {
	#build all 3 rows of matrix
	if( v_id ) {
		row_v_str <- substr(v, 1, i-1)
		row_v_vec <- strsplit( row_v_str, '' )[[1]]
		if( length(row_v_vec) == 1) {
			row_v <- c('-')
		}
		else {
			row_v <- c(row_v_vec[ 1:(length(row_v_vec)-1)  ], '-' )
		}
	}
	else {
		row_v_str <- substr(v, 1, i-1)
		row_v <- strsplit( row_v_str, '' )[[1]]
	}

	if( w_id ) {
		row_w_str <- substr(w, 1, j-1)
		row_w_vec <- strsplit( row_w_str, '' )[[1]]
		if( length(row_w_vec) == 1) {
			row_w <- c('-')
		}
		else {
			row_w <- c(row_w_vec[ 1:(length(row_w_vec)-1)  ], '-' )
		}
	}
	else {
		row_w_str <- substr(w, 1, j-1)
		row_w <- strsplit( row_w_str, '' )[[1]]
	}
	
	if( x_id ) {
		row_x_str <- substr(x, 1, k-1)
		row_x_vec <- strsplit( row_x_str, '' )[[1]]
		if( length(row_x_vec) == 1) {
			row_x <- c('-')
		}
		else {
			row_x <- c(row_x_vec[ 1:(length(row_x_vec)-1)  ], '-' )
		}
	}
	else {
		row_x_str <- substr(x, 1, k-1)
		row_x <- strsplit( row_x_str, '' )[[1]]
	}
	#fill in blanks
	max_length <- max( length(row_v), length(row_w), length(row_x) )
	if( length(row_v) < max_length ) {
		for( i in 1:( max_length - length(row_v) ) ) {
			row_v <- c(row_v, "_")
		}
	}
	if( length(row_w) < max_length ) {
		for( i in 1:( max_length - length(row_w) ) ) {
			row_w <- c(row_w, "_")
		}
	}
	if( length(row_x) < max_length ) {
		for( i in 1:( max_length - length(row_x) ) ) {
			row_x <- c(row_x, "_")
		}
	}
	#build and score matrix
	score <- 0
	to_score <- rbind( row_v, row_w, row_x )
	print(row_v)
	print(row_w)
	print(row_x)
	print(to_score)
	for( col in 1:ncol(to_score) ) {
		first_row <- to_score[1, col]
		if( all( to_score[,col] == first_row ) ) { #if all elements in col same as first
			score <- score+1
		}
	}
	print(score)
	return(score)
}

multiple_align <- function(v,w,x) {
	s <- array(-9999, dim=c(nchar(v)+1, nchar(w)+1, nchar(x)+1))
	s[1, 1, 1] <- 0
	#set initial planes
	for( i in 1:(nchar(v)+1) ) {
		for( j in 1:(nchar(w)+1) ) {
			s[i,j,1] <- 0
		}
		for( j in 1:(nchar(x)+1) ) {
			s[i,1,j] <- 0
		}	
	}
	for( i in 1:(nchar(w)+1) ) {
		for( j in 1:(nchar(x)+1) ) {
			s[1,i,j] <- 0
		}
	}

	#fill in 3D array for all other values
	for( i in 2:(nchar(v)+1) ) {
		for( j in 2:(nchar(w)+1) ) {
			for( k in 2:(nchar(x)+1) ) {
				s_i <- s[i-1,j,k] + mult_score( v, w, x, i, j, k , F, T, T )
				s_j <- s[i,j-1,k] + mult_score( v, w, x, i, j, k , T, F, T )
				s_k <- s[i,j,k-1] + mult_score( v, w, x, i, j, k , T, T, F )
				s_ij <- s[i-1,j-1,k] + mult_score( v, w, x, i, j, k , F, F, T )
				s_ik <- s[i-1,j,k-1] + mult_score( v, w, x, i, j, k , F, T, F )
				s_jk <- s[i,j-1,k-1] + mult_score( v, w, x, i, j, k , T, F, F )
				s_ijk <- s[i-1,j-1,k-1] + mult_score( v, w, x, i, j, k , F, F, F )
				s[i, j, k] <- max( s_i, s_j, s_k, s_ij, s_ik, s_jk, s_ijk )
			}
		} 
	}
	
	#backtrack thru	as done previously
	i <- nchar(v)+1
	j <- nchar(w)+1
	k <- nchar(x)+1
	backtrack <- c() #has coord that travelled along
	while( i != 1 && j != 1 && k != 1 ) {
		#handle case where one coord is 1
		if( i == 1 ) {
			if( j == 1 ) {
				backtrack <- c(backtrack, "k")
				k <- k-1
				next
			}
			if( k == 1 ) {
				backtrack <- c(backtrack, "j")
				j <- j-1
				next
			}
			s_j <- s[i,j-1,k] + mult_score( v, w, x, i, j, k , T, F, T )
			s_k <- s[i,j,k-1] + mult_score( v, w, x, i, j, k , T, T, F )
			s_jk <- s[i,j-1,k-1] + mult_score( v, w, x, i, j, k , T, F, F )
		
			if( s[i,j,k] == s_j ) {
				backtrack <- c(backtrack, "j")
				j <- j-1
				next
			}
			else if( s[i,j,k] == s_k ) {
				backtrack <- c(backtrack, "k")
				k <- k-1
				next
			}
			else if( s[i,j,k] == s_jk ) {
				backtrack <- c(backtrack, "jk")
				j <- j-1
				k <- k-1
				next
			}
			else {
				print("issue in i=1 block")
				break
			}		
		}
	
		if( j == 1 ) {
			if( k == 1 ) {
				backtrack <- c(backtrack, "i")
				i <- i-1
				next
			}
			s_i <- s[i-1,j,k] + mult_score( v, w, x, i, j, k , F, T, T )
			s_k <- s[i,j,k-1] + mult_score( v, w, x, i, j, k , T, T, F )
			s_ik <- s[i-1,j,k-1] + mult_score( v, w, x, i, j, k , F, T, F )
		
			if( s[i,j,k] == s_i ) {
				backtrack <- c(backtrack, "i")
				i <- i-1
				next
			}
			else if( s[i,j,k] == s_k ) {
				backtrack <- c(backtrack, "k")
				k <- k-1
				next
			}
			else if( s[i,j,k] == s_ik ) {
				backtrack <- c(backtrack, "ik")
				i <- i-1
				k <- k-1
				next
			}
			else {
				print("issue in j=1 block")
				break
			}		
		}
		
		if( k == 1 ) {
			s_i <- s[i-1,j,k] + mult_score( v, w, x, i, j, k , F, T, T )
			s_j <- s[i,j-1,k] + mult_score( v, w, x, i, j, k , T, F, T )
			s_ij <- s[i-1,j-1,k] + mult_score( v, w, x, i, j, k , F, F, T )
			
			if( s[i,j,k] == s_j ) {
				backtrack <- c(backtrack, "j")
				j <- j-1
				next
			}
			else if( s[i,j,k] == s_i ) {
				backtrack <- c(backtrack, "i")
				i <- i-1
				next
			}
			else if( s[i,j,k] == s_ij ) {
				backtrack <- c(backtrack, "ij")
				i <- i-1
				j <- j-1
				next
			}
			else {
				print("issue in k=1 block")
				break
			}		
		}

		s_i <- s[i-1,j,k] + mult_score( v, w, x, i, j, k , F, T, T )
		s_j <- s[i,j-1,k] + mult_score( v, w, x, i, j, k , T, F, T )
		s_k <- s[i,j,k-1] + mult_score( v, w, x, i, j, k , T, T, F )
		s_ij <- s[i-1,j-1,k] + mult_score( v, w, x, i, j, k , F, F, T )
		s_ik <- s[i-1,j,k-1] + mult_score( v, w, x, i, j, k , F, T, F )
		s_jk <- s[i,j-1,k-1] + mult_score( v, w, x, i, j, k , T, F, F )
		s_ijk <- s[i-1,j-1,k-1] + mult_score( v, w, x, i, j, k , F, F, F )
		#TODO: figure out how switch statements work in R
		if( s[i,j,k] == s_i ) {
			backtrack <- c(backtrack, "i")
			i <- i-1
			next
		}
		else if( s[i,j,k] == s_j ) {
			backtrack <- c(backtrack, "j")
			j <- j-1
			next
		}
		else if( s[i,j,k] == s_k ) {
			backtrack <- c(backtrack, "k")
			k <- k-1
			next
		}
		else if( s[i,j,k] == s_ij ) {
			backtrack <- c(backtrack, "ij")
			i <- i-1
			j <- j-1
			next
		}
		else if( s[i,j,k] == s_ik ) {
			backtrack <- c(backtrack, "ik")
			i <- i-1
			k <- k-1
			next
		}
		else if( s[i,j,k] == s_jk ) {
			backtrack <- c(backtrack, "jk")
			j <- j-1
			k <- k-1
			next
		}
		else if( s[i,j,k] == s_ijk ) {
			backtrack <- c(backtrack, "ijk")
			i <- i-1
			j <- j-1
			k <- k-1
			next
		}
		else {
			print("something wrong in big block")
			break
		}
		
	}
	#reverse backtrack cus I screwed that up
	new_bt <- c()
	for( ind in 1:length(backtrack) ) {
		new_bt <- c(backtrack[ind], new_bt)
	}
	backtrack <- new_bt
	#parse backtrack results
	v_new <- c()
	w_new <- c()
	x_new <- c()
	i <- 1
	j <- 1
	k <- 1
	for( move in backtrack ) {
		if( move=="i" ) {
			v_new <- c(v_new, char_at(v, i))
			w_new <- c(w_new, "-")
			x_new <- c(x_new, "-")
			i <- i+1
			next
		}
		else if( move=="j" ) {
			v_new <- c(v_new, "-")
			w_new <- c(w_new, char_at(w, j))
			x_new <- c(x_new, "-")
			j <- j+1
			next
		}
		else if( move=="k" ) {
			v_new <- c(v_new, "-")
			w_new <- c(w_new, "-")
			x_new <- c(x_new, char_at(x, k))
			k <- k+1
			next
		}
		else if( move=="ij" ) {
			v_new <- c(v_new, char_at(v, i))
			w_new <- c(w_new, char_at(w, j))
			x_new <- c(x_new, "-")
			i <- i+1
			j <- j+1
			next
		}
		else if( move=="ik" ) {
			v_new <- c(v_new, char_at(v, i))
			w_new <- c(w_new, "-")
			x_new <- c(x_new, char_at(x, k))
			i <- i+1
			k <- k+1
			next
		}
		else if( move=="jk" ) {
			v_new <- c(v_new, "-")
			w_new <- c(w_new, char_at(w, j))
			x_new <- c(x_new, char_at(x, k))
			j <- j+1
			k <- k+1
			next
		}
		else if( move=="ijk" ) {
			v_new <- c(v_new, char_at(v, i))
			w_new <- c(w_new, char_at(w, j))
			x_new <- c(x_new, char_at(x, k))
			i <- i+1
			j <- j+1
			k <- k+1
			next
		}
		else {
			print("move wrong")
			break
		} 
	}
	v <- paste(v_new, collapse='')
	w <- paste(w_new, collapse='')
	x <- paste(x_new, collapse='')
	return( c(v,w,x) )
}
	

print( multiple_align(v, w, x) )
