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

#build map
frequency_table <- function(text, k) {
	map <- list()
	for( i in 1:(nchar(text) - k + 1) ) {
		frame <-  substr(text, i, i+k-1)
		if( frame %in% names(map) ) {
			map[[ frame ]] <- map[[ frame ]] + 1
		}
		else {
			map[[ frame ]] <- 0
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
