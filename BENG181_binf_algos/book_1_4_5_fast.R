setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
nums <- lines[2]
nums_arr <- strsplit(nums, " ")[[1]]
k <- as.numeric(nums_arr[1])
L <- as.numeric(nums_arr[2])
t <- as.numeric(nums_arr[3])

#find k-mers in window L of at least frequency t in text
find_clumps <- function(text, k, L, t) {
	patterns <- c()
	n <- nchar(text)
	
	#Count frequencies in first window
	window <- Text(text, 0, L)
	freq_map <- frequency_table(window, k)
	for( key in names(freq_map) ) {
		if( freq_map[[ key ]] >= t ) {
			patterns <- c(patterns, key)
		}
	}

	#Shift window and make changes to freq_map based on previous first and last element
	#Note: text function uses 0-indexing
	for( i in 1:(n-L) ) {
		prev_first <- Text(text, i-1, k)
		new_last <- Text(text, i+L-k, k)
	
		freq_map[[ prev_first ]] <- freq_map[[ prev_first ]] - 1
		if( !(new_last %in% names(freq_map)) ) {
			freq_map[[ new_last ]] <- 0
		}
		else {
			freq_map[[ new_last ]] <- freq_map[[ new_last ]] + 1
		}
		if( freq_map[[ new_last ]] >= t ) {
			patterns <- c(patterns, key)
		}
	}
	patterns <- unique(patterns)
	return(patterns)
}

print(find_clumps(text,k,L,t))
