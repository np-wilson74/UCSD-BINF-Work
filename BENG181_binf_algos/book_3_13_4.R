setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
patterns <- lines[2:length(lines)]
nums <- lines[1]
k <- as.numeric(strsplit(nums, " ")[[1]][1])
d <- as.numeric(strsplit(nums, " ")[[1]][2])

str_from_gapped_pattern <- function(patterns, k, d) {
	first_patterns <- c()
	second_patterns <- c()
	for(pattern in patterns) {
		parts <- strsplit(pattern, "|", fixed=T)[[1]]
		first_patterns <- c(first_patterns, parts[1])	
		second_patterns <- c(second_patterns, parts[2])	
	}
	
	prefix <- string_from_path(first_patterns)
	suffix <- string_from_path(second_patterns) 

	prefix_vec <- strsplit(prefix, "")[[1]]
	suffix_vec <- strsplit(suffix, "")[[1]]

	for(i in (k+d+1):nchar(prefix)) {
		if(prefix_vec[i] != suffix_vec[i-k-d]) {
			return("no string spelled by gapped patterns")
		}
	}
	suffix_part <- substr(suffix, nchar(suffix)-(k+d)+1, nchar(suffix))
	combine <- paste0( prefix, suffix_part )
}

print(str_from_gapped_pattern(patterns,k,d))
