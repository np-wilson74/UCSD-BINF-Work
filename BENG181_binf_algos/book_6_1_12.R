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

middle_edge <- function(v,w, score_matrix=blosum62, sig=5) {
	#NOTE: w is on columns	
	#from source
	from_source <- get_mid_col(v, w, score_matrix=score_matrix, sig=sig)	

	#to_sink
	v_rev <- rev_str(v)
	w_rev <- rev_str(w)
	to_sink <- get_mid_col(v_rev, w_rev, score_matrix=score_matrix, sig=sig, rev=T)

	#sanity checks
	if( length(to_sink) != length(from_source) ) {
		print("SOMETHING WRONG LENGTH OF TO_SOURCE AND TO_SINK DIFFERENT!")
		q()
	}

	#since we calculated to_sink w/ v reversed, we should need to flip it for them to align
	to_sink_rev <- c()
	for( term in to_sink ) {
		to_sink_rev <- c(term, to_sink_rev)
	}
	to_sink <- to_sink_rev #TODO: if code doesn't work, remove this line

	sum <- to_sink + from_source
	#probably not necessary but I want to select the last one with max
	max_is <- which( max(sum) == sum )
	max_i <- max_is[ length(max_is) ]

	#to find next we need to go back thru reverse and find how we entered the max_i node 
	max_i_rev <- (nchar(w)+1)-max_i+1
	dir <- get_mid_col(v_rev, w_rev, score_matrix=score_matrix, sig=sig, rev=T, into_mid=T, max_i=max_i_rev)	

	mid_col <- get_mid_col(v, w, return_col=T)

	#subtract 1 from both indices cus the autograder takes it in index-0
	if( dir == "dr" ) {
		return(paste0(max_i-1 ,",", mid_col-1 , "; ", max_i,",", mid_col))
	}
	if( dir == "d" ) {
		return(paste0(max_i-1 ,",", mid_col-1 , "; ", max_i,",", mid_col-1))
	}
	if( dir == "r" ) {
		return(paste0(max_i-1 ,",", mid_col-1 , "; ", max_i-1,",", mid_col))
	}
}

#reverse strings, not that deep
rev_str <- function(str) {
	str_vec <- strsplit(str, "")[[1]]
	rev_vec <- c()
	for( letter in str_vec ) {
		rev_vec <- c(letter, rev_vec)
	}
	return(paste(rev_vec, collapse=''))
}

#get middle column of s in linear memory, rev indicates if it's a reverse string
#into_mid will instead return how we got into the mid node on the reverse
#max_i should be INDEXED ACCORDING TO HOW IT IS IN THE REVERSE S matrix
#return_col signals to just return the index of middle column
aget_mid_col <- function(v, w, score_matrix=blosum62, sig=5, rev=F, into_mid=F, max_i=NULL, return_col=F) {
	s <- matrix(0, nrow=nchar(v)+1, ncol=2)
        #just to deal with the fact that an even num of cols will give diff mid col when reversed
	if( (nchar(w)+1) %% 2 == 0 ) { #if ncol even
		if( rev ) {
			mid_col <- (nchar(w)+1)/2 + 1
		}
		else {
			mid_col <- (nchar(w)+1)/2
		}
	}
	else { #ncol is odd
		mid_col <- ceiling( (nchar(w)+1)/2 )
	}

	if( return_col ) {
		return(mid_col)
	}

        #set first col & row
        for( i in 2:nrow(s) ) { 
                s[i, 1] <- s[i-1, 1] - sig 
        }   
        s[1,2] <- (-sig) 
	#set second col
        for( i in 2:nrow(s) ) { 
                match <- score_matrix[char_at(v, i-1),char_at(w, 1)]
                s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
        }   
 
	#traverse over to middle column
        #j represents the current column on the right in terms of the whole thing when done
        for( j in 3:mid_col ) { 
                #shift right column to left
		for( i in 1:nrow(s) ) {
			s[i,1] <- s[i,2]
		}
                #set first row
                s[1,2] <- s[1,1] - sig 
                #calc new right column
                for( i in 2:nrow(s) ) { 
                        match <- score_matrix[char_at(v, i-1),char_at(w, j-1)]
                        s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
                }   
        }  
	#At this point s[,2] is the middle column and s[,1] is the one to the right on the reverse
	#The into_mid setting here will let me use what I have to figure out how we got to our mid_edge
	if( into_mid ) {
		if(max_i == 1) {
			return("r")
		} 
		else {
                	match <- score_matrix[char_at(v, max_i-1),char_at(w, mid_col-1)]
		}
		if( s[max_i, 2] == s[max_i-1, 1] + match ) { #came from top-left (bottom-right on reverse)
			return("dr")
		}
		else if( s[max_i, 2] == s[max_i, 1] - sig ) { #came from left (right on reverse)
			return("r")
		}
		else if( s[max_i, 2] == s[max_i-1, 2] - sig ) { #came from top (bottom on reverse)
			return("d")
		}
		else {
			print("something wrong w/ finding how we exited mid")
			q()
		}
	} 
	return(s[,2])
}

print(new_middle_edge(v,w, v_rng=c(1, nchar(v)+1), w_rng=c(1, nchar(w)+1)))
