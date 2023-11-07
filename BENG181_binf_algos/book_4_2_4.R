options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")
source("codon_codes.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
rna <- lines[1]

rna_to_protein <- function(rna) {
	seq <- c()
	for( i in seq(from=1, to=(nchar(rna)-2), by=3 ) ) {
		if( codons[[ Text(rna, i, 3, index0=F) ]] == "*") {break}
		seq <- c(seq, codons[[ Text(rna, i, 3, index0=F) ]]) 
	}
	return(paste(seq, collapse=''))
}

print(rna_to_protein(rna))
