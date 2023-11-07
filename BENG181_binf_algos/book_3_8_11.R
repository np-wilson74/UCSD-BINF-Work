setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])

universal_circ_str <- function(k) {
	#create all bin str of length k
	patterns <- c("1","0")
	for(i in 2:k) {
		for(pattern in patterns) {
			patterns <- c(patterns, paste0(pattern, "0"))
			patterns <- c(patterns, paste0(pattern, "1"))
		}
	}
	patterns <- patterns[ which(nchar(patterns) == k) ]
	
	dB <- de_brujin_from_patterns(patterns)
	cycle <- eulerean_circuit(dB)
	cycle <- cycle[1:(length(cycle)-k+1)]
	text <- string_from_path(cycle) 
	return(text)
}

print(universal_circ_str(k))
