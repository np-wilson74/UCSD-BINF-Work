setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
patterns <- readLines(fname)

de_brujin_from_patterns_break <- function(patterns) {
        de_brujin <- list()
        #Map suffix to prefix, adding it if prefix already exists in de_brujin
        for(pattern in patterns) {
		part1 <- strsplit(pattern, "|", fixed=T)[[1]][1]
		part2 <- strsplit(pattern, "|", fixed=T)[[1]][2]
		prefix <- paste0(prefix(part1), ",", prefix(part2))
		suffix <- paste0(suffix(part1), ",", suffix(part2))
                if( is.null(de_brujin[[ prefix ]]) ) { 
                        de_brujin[[ prefix ]] <- suffix
                }   
                else {
                        de_brujin[[ prefix ]] <- c(de_brujin[[ prefix ]], suffix)
                }   
        }    
        return(de_brujin)
}

print(de_brujin_from_patterns_break(patterns))
