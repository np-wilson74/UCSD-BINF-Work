options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
pattern <- lines[1]
dna <- lines[2:length(lines)]
dna <- strsplit(dna, " ")[[1]]

print(d_sum(pattern, dna))
