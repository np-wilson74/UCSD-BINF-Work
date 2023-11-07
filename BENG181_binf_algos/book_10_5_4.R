options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
text <- lines[1]

SuffixTreeNode <- R6Class(
	"SuffixTreeNode",
	public = list(
		label=NULL,
		outs=NULL,
		out_labels=NULL,
		num = NULL,
		pos = NULL,
		len = 1,
		initialize = function(label, pos) {
			self$label <- label
			self$pos <- pos
		},
		extend = function(label, pos) {
			self$outs <- c(self$outs, SuffixTreeNode$new(label, pos))
			self$out_labels <- c(self$out_labels, label)
		}
	)
)

SuffixTree <- R6Class(
	"SuffixTree",
	public = list(
		head = NULL,
		initialize = function( head ) {
			self$head <- head
		},
		construct_trie = function( str ) {
			chars <- strsplit(str, "")[[1]]
			for( i in 1:length(chars) ) {
				curr <- self$head
				for( j in i:length(chars) ) {
					sym <- chars[j]
					if( sym %in% curr$out_labels ) {
                                        	next_ind <- which(curr$out_labels == sym)
                                        	curr <- curr$outs[[next_ind]]
					}
					else {
						curr$extend(sym, j)
                                        	next_ind <- which(curr$out_labels == sym)
                                        	curr <- curr$outs[[next_ind]]
					}
				}
				if( length(curr$out_labels) < 1 ) {
					curr$num <- i
				}	
			}
		}
	)
)

#recursive definition
#only call on things that branch
collapse_children <- function( node ) {
	new_conns <- c()
	for( child in node$outs ) {
		len <- 1
		curr <- child
		while( length(curr$outs) == 1 ) {
			curr <- curr$outs[[1]]
			len <- len + 1
		}
		curr$len <- len
		curr$pos <- child$pos
		if( length(curr$outs) > 1 ) {
			new_conns <- c(new_conns, curr)
			collapse_children(curr)
		}
		else { #== 0
			new_conns <- c(new_conns, curr)
		}
	}
	node$outs <- new_conns
}

#build suffix tree
build_tree <- function(text) {
	head <- SuffixTreeNode$new("*", 0)
	tree <- SuffixTree$new(head)
	tree$construct_trie(text)
	collapse_children(tree$head)
	return(tree)
}

print_edge <- function(node, text) {
	edges <- c()
	for( out in node$outs ) {
		edges <- c(edges, substr(text, out$pos, out$pos + out$len - 1))
		edges <- c(edges, print_edge(out, text))
	}
	return(edges)
}

cat(paste(print_edge(build_tree(text)$head, text), collapse="\n"))
