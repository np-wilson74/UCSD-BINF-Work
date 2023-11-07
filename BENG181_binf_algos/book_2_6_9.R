setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
k <- as.numeric( strsplit(lines[1], " ")[[1]][1] )
t <- as.numeric( strsplit(lines[1], " ")[[1]][2] )
dna <- lines[2:length(lines)]

write(greedy_motif_search(dna,k,t, naive_count=F), "soln_2_6_9.txt")
