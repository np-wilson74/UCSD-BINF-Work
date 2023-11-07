setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
nums <- strsplit(lines[1], " ")[[1]]
k <- as.numeric( nums[1] )
d <- as.numeric( nums[2] )
patterns <- lines[2:length(lines)]


string_reconstruction_pair <- function(patterns, k, d) {
	dB <- de_brujin_from_patterns_break(patterns)
	path <- eulerean_path(dB)
	text <- str_from_gapped_pattern(path, k, d)
	return(text)
}

print(string_reconstruction_pair(patterns,k,d))
