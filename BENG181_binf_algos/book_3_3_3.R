setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
patterns <- readLines(fname)


string_from_path <- function(patterns) {
	#pull out last element
	last <- patterns[ length(patterns) ]
	patterns <- patterns[ 1:(length(patterns)-1) ]
	str_peices <- c()
	#take first letter of each pattern
	for(pattern in patterns) {
		str_peices <- c(str_peices, substr(pattern, 1, 1))
	}
	#add last one back
	first <- paste0(str_peices, collapse='')
	string <- paste0(first, last)
	return(string)
}
write.table(string_from_path(patterns), file="book_3_3_3.txt", quote=F, sep=" ", row.names=F, col.names=F)
