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

points <- list()
for( i in 2:length(lines) ) {
	point <- as.numeric(strsplit(lines[i], " ")[[1]])
	points[[ as.character(i-1) ]] <- point
}

#distance from point to closest center
d <- function(point, centers) {
	d <- 1e9
	for( center in centers ) {
		d <- min(d, euclidean_dist(point, center))
	}
	return(d)
}

#find Euclidean dist between 2 points, assume that same dim
euclidean_dist <- function(point1, point2) {
	m <- length(point1)
	square <- 0
	for( i in 1:m ) {
		square <- square + (point1[i] - point2[i])^2
	}
	return( sqrt(square) )	
}

farthest_first_travel <- function(k, m, points) {
	centers <- list()
	i <- 1
	#add first point to centers, remove it from points
	centers[[ as.character(i) ]] <- points[[as.character(1)]]
	points[[ "1" ]] <- NULL
	while( length(centers) < k ) {
		max_dist <- 0
		max_point <- NULL
		max_ind <- 0
		for( j in 1:length(points) ) {
			if( is.null( points[[as.character(j)]] ) ) {
				next
			}
			dist <- d(points[[as.character(j)]], centers)
			if( dist > max_dist ) {
				max_dist <- dist
				max_point <- points[[as.character(j)]]
				max_ind <- j
			}
		}
		i <- i + 1
		centers[[ as.character(i) ]] <- max_point
		points[[ as.character(max_ind) ]] <- NULL
	}
	return( centers ) 
}

print_points <- function(points) {
	for( point in points ) {
		cat(point)
		cat("\n")
	}
}

print_points(farthest_first_travel(k, m, points))
