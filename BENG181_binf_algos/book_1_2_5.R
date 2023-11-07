#print number of times pattern appears in text (text line 1, pattern line 2)
setwd("~/Desktop/School/BENG_181/Practice_Code")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
dna <- lines[1]
match <- lines[2]

#define Text function
Text <- function(pos, len) {
	return( substr(dna, pos+1, pos+len) )
}
print(Text(13, 10))

#patternCount algorithm
count <- 0
for( i in 0:(nchar(dna)-nchar(match)) ) {
	if( Text(i, nchar(match)) == match ) {
		count <- count + 1
	}
}

print(count)
