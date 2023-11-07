#print number of times pattern appears in text (text line 1, pattern line 2)
setwd("~/Desktop/School/BENG_181/Practice_Code")
print("Reading data ...")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
dna <- lines[2]
match <- lines[1]
print("Finding position of each match ... ")

#define Text function
Text <- function(pos, len) {
	return( substr(dna, pos+1, pos+len) )
}

match_pos(dna, match) {
	#get position of each match
	pos <- c()
	match_vec <- strsplit(match, "")[[1]]
	for( i in 0:(nchar(dna)-nchar(match)) ) {
		comp <- Text(i, nchar(match))
		comp_vec <- strsplit(comp, "")[[1]]
		is_match = F
		for( letter in 1:length(comp_vec) ) {
			if( comp_vec[letter] != match_vec[letter] ) {
				break
			}
			else if(letter == length(comp_vec)) {
				is_match = T
			}
	
		}
		if(is_match) {
			print("Adding index ...")
			pos <- c(pos, i)
		}
	}
	return(pos)
}
print(paste(match_pos(dna, match), collapse=" "))
