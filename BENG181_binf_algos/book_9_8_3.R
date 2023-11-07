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

#return true if centers is same as prev_centers ( i.e. if lloyd has converged)
converged <- function( centers, prev_centers ) {
	for( i in 1:length(centers) ) {
		if( centers[[ as.character(i) ]] != prev_centers[[ as.character(i) ]] ) {
			return( F )
		}
	}
	return( T ) 
}

#center wich has distance from point to closest center
d <- function(point, centers) {
        d <- 1e9
	closest <- NULL
        for( i in 1:length(centers) ) {  
		dist <- euclidean_dist(point, centers[[ as.character(i) ]])
                if( d > dist ) {
			d <- dist
			closest <- i
		}
        }    
        return(closest)
}

#return a list of indices of points that are closest to center
assign_clusters <- function(points, centers) {
	clusters <- list()
	i <- 1
	for( point in points ) {
		closest <- d(point, centers) #closest center to point
		if( is.null(clusters[[ as.character(closest) ]]) ) {
			clusters[[ as.character(closest) ]] <- i
		} 
		else {
			clusters[[ as.character(closest) ]] <- c( clusters[[ as.character(closest) ]], i)
		}
		i <- i + 1
	}
	return(clusters)
}

#find center of gravity of points given indices of ones that are in group
#return as vector of coordinates
center_of_gravity <- function(points, indices, m) {
	points_sub <- list()
	coords <- list() #list of coords
	for( i in indices ) {
		points_sub[[ as.character(i) ]] <- points[[ as.character(i) ]] 
	}
	for( point in points_sub ) {
		for( i in 1:m ) {
			if( is.null(coords[[ as.character(i) ]]) ) {
				coords[[ as.character(i) ]] <- point[i]
			} 
			else {
				coords[[ as.character(i) ]] <- c(coords[[ as.character(i) ]], point[i])	
			}
		}
	}
	for( i in 1:m ) {
		coords[[ as.character(i) ]] <- mean( coords[[ as.character(i) ]] )
	}
	return(unlist(coords))
}

#points is ordered 1 - n_points
lloyd <- function(k, m, points) {
	#assign centers
	centers <- list() #list of points
	prev_centers <- list()
	for( i in 1:k ) {
		centers[[ as.character(i) ]] <- points[[ as.character(i) ]]
	}
	clusters <- assign_clusters(points, centers)
	#iterate from clusters to centers to clusters until converges
	repeat {
		prev_centers <- centers
		#cluster to center
		for( i in 1:k ) {
			centers[[ as.character(i) ]] <- center_of_gravity(points, clusters[[ as.character(i) ]], m)
		}	

		#center to clusters
		clusters <- assign_clusters(points, centers)

		if( converged(centers, prev_centers) ) { #condition to exit loop
			break
		}
	}
	return(centers)
}

print_points(lloyd(k,m,points))
