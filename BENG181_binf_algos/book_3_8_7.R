setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])
patterns <- lines[2: length(lines)]

string_reconstruction <- function(k, patterns) {
	dB <- de_brujin_from_patterns(patterns)
	path <- eulerean_path(dB)
	text <- string_from_path(path)
	return(text)
}

print(string_reconstruction(k, patterns))
