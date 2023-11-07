##I will denote all new comments for the midterm with the double hashmark "##"

#find largest k-mer in DNA sequence given text file with DNA on first line and k on second
setwd("~/Desktop/School/BENG_181/Practice_Code")
 
#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
k <- as.numeric(lines[2])

#return highest value in map
max_set <- function(map) {
	return( max(unlist(map)) )	
}

##To solve this, I want to check if the k-mer I'm checking is the same as any of the previous k-1 kmers
#build map
frequency_table <- function(text, k) {
	map <- list()
	prevs <- c() ##vector of previous frames
	for( i in 1:(nchar(text) - k + 1) ) {
		frame <-  substr(text, i, i+k-1)
		if( length(prevs) > (k-1) ) { ##Add this so that we remove prevs when appropriate
			prevs <- prevs[-1]
		}
		if( frame %in% names(map) ) {
			if( frame %in% prevs ) { ##added this if/else structure, want blank so prevs keeps correct size
				prevs <- c(prevs, "-")
			}
			else {
				map[[ frame ]] <- map[[ frame ]] + 1
				prevs <- c(prevs, frame) ##append current frame to end of frames
			}
		}
		else { ##if  we've never seen this frame, we don't have to worry about it being in prevs
			map[[ frame ]] <- 0
			prevs <- c(prevs, frame) ##append current frame to end of frames
			##we only want to do this if we're  not skipping it
		}
	}
	return(map)
}

map <- frequency_table(text,k)

#find k-mers that have max
max_kmers <- c()
max <- max_set(map)
for(kmer in names(map)) {
	if( map[[ kmer ]] == max ) {
		max_kmers <- c(max_kmers, kmer)
	}
}
print(max_kmers)
