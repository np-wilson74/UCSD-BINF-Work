setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")
source("blosum62.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
v <- lines[1]
w <- lines[2]

#replicating rep function from  base v3.6.2
rep <- function( n_times, str ) {
	vec <- c()
	for(i in 1:n_times) {
		vec <- c(vec, str)
	}
	return(paste(vec, collapse=''))
}

#Linear Space Alignment, followed Stepik psuedocode p closely
lin_space_align <- function(v, w, top, bottom, left, right) {
	output <- c()
	if( left == right ) {
		if( bottom-top == 0 ) {
			return()	
		}
		else {
			vert_path <- rep(bottom-top, "V")
		}
		print(vert_path)
		return(vert_path)
	}
	
	if( top == bottom ) {
		hz_path <- rep(right-left, "H")
		print(hz_path)
		return(hz_path)
	}
	
	middle <- floor( (left+right)/2 )
	#substringing v & w can create a smaller graph
	sub_v <- substr(v, top, bottom-1)
	sub_w <- substr(w, left, right-1 )
	mid_edge <- middle_edge( sub_v, sub_w ) #THIS IS CURRENTLY BROKEN
	mid_node_pair <- strsplit(mid_edge, ";", fixed=T)[[1]]
	mid_node <- strsplit(mid_node_pair[1], ",", fixed=T)[[1]][1]
	mid_node <- as.numeric(mid_node)						#should be 7
	#Since mid_node is calculate from start of substring, must add back index for removed chars
	mid_node <- mid_node + top-1
	print("------------")
	print(paste0("top: ", top))
	print(paste0("bottom: ", bottom))
	print(paste0("left: ", left))
	print(paste0("right: ", right))
#	print(paste0("mid_node: ", mid_node))
#	print(paste0("middle: ", middle))

	#NOTE: middle is L/R, mid_node is T/B
	

	output <- c(output, lin_space_align(v,w, top, mid_node, left, middle))
	
	#get mid edge in convenient variables
	mid_edge_row_1 <- as.numeric(strsplit(mid_node_pair[1], ",", fixed=T)[[1]][1]) 
	mid_edge_row_2 <- as.numeric(strsplit(mid_node_pair[2], ",", fixed=T)[[1]][1]) 
	mid_edge_col_1 <- as.numeric(strsplit(mid_node_pair[1], ",", fixed=T)[[1]][2]) 
	mid_edge_col_2 <- as.numeric(strsplit(mid_node_pair[2], ",", fixed=T)[[1]][2]) 
	
	if( mid_edge_row_1 + 1 == mid_edge_row_2 && mid_edge_col_1 + 1 == mid_edge_col_2 ) {
		mid_edge_dir <- "D"
	}
	else if (mid_edge_row_1 + 1 == mid_edge_row_2 && mid_edge_col_1  == mid_edge_col_2) {
		mid_edge_dir <- "V"
	}
	else if (mid_edge_row_1 == mid_edge_row_2 && mid_edge_col_1 + 1 == mid_edge_col_2) {
		mid_edge_dir <- "H"
	}
	else {
		print("SOMETHING WRONG WITH MID_EDGE_DIR")
		q()
	}
	print(mid_edge_dir)
	output <- c(output, mid_edge_dir)

	if( mid_edge_dir == "H" || mid_edge_dir == "D" ) {
		middle <- middle + 1
	}
	
	if( mid_edge_dir == "V" || mid_edge_dir == "D" ) {
		mid_node <- mid_node + 1
	}
	output <- c(output, lin_space_align(v,w, mid_node, bottom, middle, right))
	return(paste(output, collapse=''))
}

parse_alignment_path <- function(path, v, w) {
	path <- strsplit(path, '')[[1]]
	v_vec <- c()
	v_at <- 1
	w_vec <- c()
	w_at <- 1
	for( letter in path ) {
		if( letter == "H" ) {
			v_vec <- c(v_vec, "_")
			w_vec <- c(w_vec, char_at(w, w_at))
			w_at <- w_at + 1
		}
		else if (letter == "V") {
			w_vec <- c(w_vec, "_")
			v_vec <- c(v_vec, char_at(v, v_at))
			v_at <- v_at + 1

		}
		else {
			w_vec <- c(w_vec, char_at(w, w_at))
			v_vec <- c(v_vec, char_at(v, v_at))
			v_at <- v_at + 1
			w_at <- w_at + 1
		}
	}
	w_new <- paste(w_vec, collapse='')
	v_new <- paste(v_vec, collapse='')
	return( paste0(v_new, ", ", w_new) )
}

print( parse_alignment_path(lin_space_align(v, w, 1, nchar(v)+1, 1, nchar(w)+1), v, w))
#print( lin_space_align(v, w, 1, nchar(v)+1, 1, nchar(w)+1))
