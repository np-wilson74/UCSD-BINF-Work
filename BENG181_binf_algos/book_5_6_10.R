options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
n_m <- strsplit(lines[1], " ")[[1]]
n <- as.numeric(n_m[1])
m <- as.numeric(n_m[2])
down_list <- strsplit(lines[2:(n+1)], " ")
right_list <- strsplit(lines[(n+3):length(lines)], " ")

down <- matrix( as.numeric(unlist(down_list)), nrow=n, byrow=T )
right <- matrix( as.numeric(unlist(right_list)), nrow=n+1, byrow=T )

manhattan <- function(n, m, down, right) {
	#matrix of 0's for each node
	max_mat <- matrix( 0, nrow=n+1, ncol=n+1 )

	#fill top and leftmost col/row
	for( i in 2:(n+1) ) {
		max_mat[i, 1] <- max_mat[i-1, 1] + down[i-1, 1]
	}
	for( i in 2:(m+1) ) {
		max_mat[1, i] <- max_mat[1, i-1] + right[1, i-1]
	}

	#go thru each row
	for( i in 2:(n+1) ) {
		#go thru each col
		for( j in 2:(m+1) ) {
			max_mat[i,j] <- max( max_mat[i-1,j] + down[i-1, j] , max_mat[i, j-1] + right[i, j-1] )
		}
	}	
	return(max_mat[n+1,m+1])
}

print(manhattan(n,m,down,right))
