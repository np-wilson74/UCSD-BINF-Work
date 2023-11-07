options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]
patterns <- lines[2:length(lines)]

#see if text is a prefix of trie (made from patterns)
match_trie <- function(trie, text) {
	chars <- strsplit(text, "")[[1]]
	chars <- c(chars, "$") #adding so I can stop w/o out of bounds if text short
	sym <- chars[1]
	ind <- 1
	curr <- trie$head
	repeat {
		if( length(curr$outs) < 1 ) { #if leaf
			return( paste( chars[1:ind], collapse="") )
		}
		else if ( sym %in% curr$out_labels && sym != "$" ) {
			next_ind <- which(curr$out_labels == sym)
                        curr <- curr$outs[[next_ind]]
			ind <- ind + 1
			sym <- chars[ind]
		}
		else {
			return("no match")
		}
	}
}

#match all prefixes of text to trie, return indices where trie has prefix of text
match_trie_prefix <- function(trie, text) {
	matches_ind <- c()
	for( i in 1:nchar(text) ) {
		input_text <- substr(text, i, nchar(text))
		match <- match_trie(trie, input_text)
		if(match != "no match") {
			matches_ind <- c(matches_ind, i)
		}
	}
	return( matches_ind - 1 ) #subtract 1 for 0index
}

trie <- construct_trie(patterns)
print(paste(match_trie_prefix(trie, text), collapse=" "))
