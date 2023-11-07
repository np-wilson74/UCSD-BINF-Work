load("automaton2.rdata") #load automaton object
lines <- readLines("DNA.txt")
lines <- lines[2:length(lines)]
#chars <- strsplit(paste(lines, collapse=""), "")[[1]]
#save(chars, file="DNA_as_chars.rdata")
#chars <- c(chars,  "$")
load("DNA_as_chars.rdata")
load("max_str2.rdata")
score_report <- list()

c <- 1
l <- 1
v <- 1

for(i in 1:length(chars)) {
	print(i)
	while(automaton[v, chars[c]] != 0) {
		v <- automaton[v, chars[c]]
		c <- c + 1		
	}
	if(all(automaton[v,] == 0)) { #report match
		str <- max_str[[ as.character(v) ]]
		if( is.null(score_report[[ str ]] ) ) {
			score_report[[ str ]] <- 1
		}
		else {
			score_report[[ str ]] <- score_report[[ str ]] + 1
		}
	}
	l <- l + 1
	c <- l
	v <- 1
}

for(str in unlist(max_str)[!(unlist(max_str) %in% names(score_report))]) {
	score_report[[ str ]] <- 0
}

write.table(t(as.data.frame(score_report)), "score_report2.csv", row.names=T, col.names=F, quote=F)
