options(warn=-1)
library("R6")
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
strs <- readLines(fname)

Node <- R6Class(
	"Node",
	public = list(
		label = NULL,
		outs = NULL,
		out_labels = NULL,
		num = NULL,
		initialize = function(label, outs, num) {
			self$label <- label
			self$outs <- outs
			self$num <- num
		},
		extend = function(label) {
			self$outs <- c(self$outs, Node$new(label, NULL, self$num+1))
			self$out_labels <- c(self$out_labels, label)
		}
	)
)

Trie <- R6Class(
	"Trie",
	public = list(
		head = NULL,
		n_nodes = 1,
		initialize = function( head ) {
			self$head <- head
		},
		add_str = function( str ) {
			chars <- strsplit(str, "")[[1]]
			curr <- self$head #this is pass by reference
			for( char in chars ) {
				if( char %in% curr$out_labels ) {
					next_ind <- which(curr$out_labels == char)
					curr <- curr$outs[[next_ind]]
				}
				else {
					curr$extend(char)
					next_ind <- which(curr$out_labels == char)
					curr <- curr$outs[[next_ind]]
					curr$num <- self$n_nodes
					self$n_nodes <- self$n_nodes + 1
				}
			}
		}
	)
)

construct_trie <- function(strs) {
	head <- Node$new("*", NULL, 0)
	trie <- Trie$new(head)
	for(str in strs) {
		trie$add_str(str)
	}
	return(trie)
}

#I'm going to recurse thru
parse_trie <- function(trie, head=NULL) {
	results <- c()
	if( is.null(head) ) {
		head <- trie$head
	}
	if( length( head$out_labels ) < 1 ) {
		return()
	}
	for( i in 1:length(head$out_labels) ) {
		results <- c(results, paste0(head$num, "->", head$outs[[i]]$num, ":", head$out_labels[i]))
		results <- c(results, parse_trie(trie, head=head$outs[[i]]))
	}
	return(results)
}

cat( paste(parse_trie(construct_trie(strs)), collapse="\n") )
