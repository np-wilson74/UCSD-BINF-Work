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
	for( i in 0:(n-L) ) {
		window <- Text(text, i, L)
		freq_map <- frequency_table(window, k)
		for( key in names(freq_map) ) {
			if( freq_map[[ key ]] >= t ) {
				patterns <- c(patterns, key)
			}
		}
	}
	patterns <- unique(patterns)
	return(patterns)
}

print(find_clumps(text,k,L,t))
