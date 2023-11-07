options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
dna <- lines[2: length(lines)]
k <- as.numeric(strsplit(lines[1]," ")[[1]][1])
t <- as.numeric(strsplit(lines[1]," ")[[1]][2])
n <- as.numeric(strsplit(lines[1]," ")[[1]][3])

gibbs <- function(dna, k, t, n) {
	#sample random motifs
	motifs <- c() 
        for( string in dna ) { 
                n <- nchar(string)
                start_ind <- sample(1:(n-k+1), 1)
                motifs <- c(motifs, Text(string, start_ind, k, index0=F))
        }   
        best_motifs <- motifs	
	for( i in 1:n ) {
		rem_ind <- sample(1:t, 1)
		rem_motif <- motifs[rem_ind]
		motifs <- motifs[ -(rem_ind) ]
		profile <- profile_from_motifs(motifs)
		new_motif <- profile_most_probable_rand(dna[rem_ind], k, profile)
		#insert new motif into motifs in correct position
		motifs <- c(motifs, new_motif)
		if(rem_ind == 1) {
			motifs <- motifs[ c(length(motifs), 1:(length(motifs)-1)) ]
		}
		else if(rem_ind != t) {
			motifs <- motifs[ c(1:(rem_ind-1), t, (rem_ind):(t-1)) ]
		}
		#compare old and new motifs
		if( score(motifs) < score(best_motifs) ) {
			best_motifs <- motifs
			print("new best")
		}
	}
	print(score(best_motifs))
	return(best_motifs)
	
}

profile_most_probable_rand <- function(text, k, profile) {
        n <- nchar(text)
        strs <- list()
        for( i in 1:(n-k+1) ) {
                str <- Text(text, i, k, index0=F)
                strs[[ str ]] <- str_prob(str, profile)
        }   
        return( sample( names(strs), 1, prob=unlist(strs) ) )
}

#probability of pattern given profile, assumes ncol(profile) == nchar(pattern)
str_prob <- function(pattern, profile) {
        prob <- 1 
        pattern_vec <- strsplit(pattern, "")[[1]]
        for( i in 1:nchar(pattern) ) {  
                prob <- prob * as.numeric(profile[ pattern_vec[i], ][i])
        }
        return(prob)
}

samp_out <- strsplit("ACGTCCACCGGCGTC AAGCGCACCGGGGTG ACCCTTACCGGGGTG AAGTTCCTCGGGGTG AAGTTTTATGGGGTG AAGTTTACCGGGTGC AAGTTTCGAGGGGTG CTGTTTACCGGGGTA AAGTTGCTCGGGGTG AAACATACCGGGGTG AAGTTTAGGAGGGTG AAGGAAACCGGGGTG AAGTTTACACAGGTG TAGTTTACCGGGGAT CCTTTTACCGGGGTG AAGTGAGCCGGGGTG AAGTCGTCCGGGGTG AAGTTTACCGGACAG AAGTTTACCAATGTG AAGTTTACCGTCATG", " ")[[1]]
print(score(samp_out))

print(gibbs(dna,k,t,n))
#write.table(gibbs(dna,k,t,n), file="soln_2_9_4.txt", quote=F, sep=" ", row.names=F, col.names=F)
