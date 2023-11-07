#create complement of given DNA strand in 5' to 3' direction
#reverse the string and take complement strands
setwd("~/Desktop/School/BENG_181/Practice_Code")
 
#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)	
text <- lines[1]

make_comp <- function(text) {
	letter_vec <- strsplit(text, split="")[[1]]
	comp_vec <- c()
	
	#get vector of complements in 3' > 5'
	for( letter in letter_vec ) {
		if(letter == "T") {
			to_add <- "A"
		}
		if(letter == "A") {
			to_add <- "T"
		}
		if(letter == "C") {
			to_add <- "G"
		}
		if(letter == "G") {
			to_add <- "C"
		}
		comp_vec <- c(comp_vec, to_add)
	}

	#reverse comp_vec such that it face 5' > 3'
	rev_comp_vec <- c()
	for( i in length(comp_vec):0 ) {
		rev_comp_vec <- c(rev_comp_vec, comp_vec[i] )
	}
	
	rev_comp_str <- paste(rev_comp_vec, collapse='')
	return(rev_comp_str)
}
print( make_comp(text) )
