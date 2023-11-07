options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k_m <- strsplit(lines[1]," ")[[1]]
k <- k_m[1]
m <- k_m[2]

#read in inputs since they're formatted weird
points <- list()
centers <- list()
fill_centers <- T
for( i in 2:length(lines) ) { 
        if( fill_centers ) {
		if( length(strsplit(lines[i], " ")[[1]]) < 2) { #if marker saying we're filling points
			fill_centers <- F
			next
		} 
		else {
			point <- as.numeric(strsplit(lines[i], " ")[[1]])
        		centers[[ as.character(i) ]] <- point
		}
	}
	else {
		point <- as.numeric(strsplit(lines[i], " ")[[1]])
        	points[[ as.character(i) ]] <- point
	}
}

#find square error between points and closest centers
#inputs both lists
#points does not necessarily include centers
square_error <- function( points, centers ) {
	sum <- 0
	for( point in points ) {
		sum <- sum + d(point, centers)^2

	}
	error <- sum / length(points)
	return(error)
}

print(square_error(points, centers))
