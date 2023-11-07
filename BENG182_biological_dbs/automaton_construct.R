#query <- readLines("queries.txt")
query <- readLines("queries2.txt")

automaton <- matrix(0, nrow=1, ncol=5)
colnames(automaton) <- c("A","C","T","G", "$")
max <- 1
max_str <- list()

#construct automaton matrix
for(str in query) {
	curr <- 1
	chars <- strsplit(str, "")[[1]]
	for(char in chars) {
		if(automaton[curr,char] == 0) {
			automaton[curr, char] <- max+1
			automaton <- rbind(automaton, c(0,0,0,0,0))
			curr <- max + 1
			max <- max + 1
		}
		else {
			curr <- automaton[curr, char]
		}
	}
	max_str[[ as.character(max) ]] <- str
}
save(max_str, file="max_str2.rdata")
save(automaton, file="automaton2.rdata")
