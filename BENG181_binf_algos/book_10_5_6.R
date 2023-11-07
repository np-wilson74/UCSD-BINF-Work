options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text1 <- lines[1]
text2 <- lines[2]

text <- paste0(text1, "#", text2, "$")

longest_shared_substr <- function(text) {
	tree <- build_tree(text)
	color_node(tree$head, text)	
	return(find_longest(tree$head, text))
}

color_node <- function(node, text) {
	#base case
	if( length(node$outs) == 0 ) { #leaf
		true_label <- substr(text, node$pos, node$pos + node$len - 1)
		true_chars <- strsplit(true_label, "")[[1]]
		if( "#" %in% true_chars ) {
			node$color <- "B"
		}
		else {
			node$color <- "R"
		}
		return()
	}
	
	#make sure all children colored
	child_colors <- c()
	for( out in node$outs ) {
		if( is.null(out$color) ) {
			color_node(out, text)
		}
		child_colors <- c(child_colors, out$color)
	}
	
	if( "P" %in% child_colors ) {
		node$color <- "P"
	} else if( "R" %in% child_colors && "B" %in% child_colors ) {
		node$color <- "P"
	} else if( "R" %in% child_colors ) {
		node$color <- "R"
	} else {
		node$color <- "B"
	}
}

find_longest <- function(node, text) {
	substrs <- c()
	for( out  in node$outs ) {
		if( out$color == "P" ) {
			substrs <- c(substrs, paste0(substr(text, out$pos, out$pos + out$len - 1), find_longest(out, text)))
		}
	}	
	return( substrs[which(nchar(substrs)==max(nchar(substrs)))] )
}

print( longest_shared_substr(text) )
