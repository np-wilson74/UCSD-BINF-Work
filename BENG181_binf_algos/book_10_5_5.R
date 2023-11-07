options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

longest_repeat <- function(text) {
	print("constructing tree ...")
	tree <- build_tree(text)
	
	#testing
	#head <- tree$head
	#print(head$outs)
	#q()
	


	#Output here is vector of all repeats, ones that end at leaf end w/ *
	#To solve, remove all repeats ending w/ * and select longest
	print("finding repeats ...")
	all_repeats <-  find_repeats(tree$head) 
	valid_repeats <- c()
	print("subsetting repeats ...")
	for( rep in all_repeats ) {
		if( substr(rep, nchar(rep), nchar(rep)) == "*" ) {
			next
		}
		valid_repeats <- c(valid_repeats, rep)
	}
	return( valid_repeats[ which(nchar(valid_repeats)==max(nchar(valid_repeats))) ] )
}

find_repeats <- function(node) {
	repeats <- c()
	for( out in node$outs ) {
		if( !is.null(out$num) ) {
			repeats <- c(repeats, substr(text, out$pos, out$pos + out$num - 1))
		}
		if( length(out$outs) == 0 ) {
		#	repeats <- c(repeats, paste0(out$label, "*"))
			next
		}
		repeats <- c(repeats, paste0(substr(text, out$pos, out$len + out$pos - 1) , find_repeats(out)))
	}
	return(repeats)
}

print( longest_repeat(text) )

#first solution used before we were told we could use the Trie
#I actually figured it out for a tree
#for( i in nchar(text):1 ) {
#	freq <- frequency_table(text, i)
#	if( any(unlist(freq) > 1) ) {
#		ind <- which( unlist(freq) > 1 )[1]
#		print(names(freq)[ind])
#		break
#	}
#}
