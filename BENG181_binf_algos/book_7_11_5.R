options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric(lines[1])
str1 <- lines[2]
str2 <- lines[3]

shared_kmers <- function(str2, str1, k) { #flip inputs so code works
	matches <- list()
	#make 2 lists that have all index-kmer mappings
	str1_map <- list()
	for( i in 1:(nchar(str1)-k+1)) {
		str1_map[[i]] <- Text(str1, i, k, index0=F)
	}
	str2_map <- list()
	str1_kmers <- unlist(str1_map)
	for( i in 1:(nchar(str2)-k+1)) {
		kmer <- Text(str2, i, k, index0=F)
		str2_map[[i]] <- kmer
		#find if there's a match w/ first one
		if( kmer %in% str1_kmers ) {
			matches[[as.character(i)]] <- which( kmer == str1_kmers )
		}
		if( make_comp(kmer) %in% str1_kmers ) {
			match_ind <- which( make_comp(kmer) == str1_kmers )
			if( is.null(matches[[as.character(i)]]) ) {
				matches[[as.character(i)]] <- match_ind
			}
			else {
				matches[[as.character(i)]] <- c(matches[[as.character(i)]], match_ind)
			}
		}
	}
	print(matches)
	#return(matches)
}

#convert from R list to points in "(0, 4)" vector
parse_matches <- function(matches, index0=T) {
	points <- c()
	for( i in names(matches) ) {
		if( is.null( matches[[i]] ) ) {
			next
		}
		else {
			for( y in matches[[i]] ) {
				points <- c(points, paste0("(", as.numeric(i)-as.numeric(index0), ", ", y-as.numeric(index0), ")"))
			}
		}
	}
	return(points)
}


cat( paste(parse_matches(shared_kmers(str1,str2,k)), collapse="\n") )

