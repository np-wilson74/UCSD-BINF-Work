#return subset of text based on starting pos and len
Text <- function(text, pos, len, index0 = T) {
	if(index0) {
        	return( substr(text, pos+1, pos+len) )
	}
	else {
        	return( substr(text, pos, pos+len-1) )
	}
}

#return highest value in map
max_set <- function(map) {
        return( max(unlist(map)) )    
}

#frequency build map
frequency_table <- function(text, k) {
        map <- list()
        for( i in 1:(nchar(text) - k + 1) ) { 
                frame <-  substr(text, i, i+k-1)
                if( frame %in% names(map) ) { 
                        map[[ frame ]] <- map[[ frame ]] + 1 
                }   
                else {
                        map[[ frame ]] <- 1
                }   
        }   
        return(map)
}

#generate complementary DNA strand in 5' > 3' direction
comp <- list()
comp[[ "A" ]] <- "T" 
comp[[ "T" ]] <- "A" 
comp[[ "C" ]] <- "G" 
comp[[ "G" ]] <- "C" 
make_comp <- function(text) {
        letter_vec <- strsplit(text, split="")[[1]]
        comp_vec <- c() 
            
        #get vector of complements in 3' > 5'
        for( letter in letter_vec ) { 
                to_add <- comp[[ letter ]]
		comp_vec <- c(comp_vec, to_add)
        }   

        #reverse comp_vec such that it faces 5' > 3'
        rev_comp_vec <- c() 
        for( i in length(comp_vec):0 ) { 
                rev_comp_vec <- c(rev_comp_vec, comp_vec[i] )
        }   
            
        rev_comp_str <- paste(rev_comp_vec, collapse='')
        return(rev_comp_str)
}

#get position of match in DNA
match_pos <- function(dna, match) {
        pos <- c() 
        match_vec <- strsplit(match, "")[[1]]
        for( i in 0:(nchar(dna)-nchar(match)) ) { 
                comp <- Text(i, nchar(match))
                comp_vec <- strsplit(comp, "")[[1]]
                is_match = F 
                for( letter in 1:length(comp_vec) ) { 
                        if( comp_vec[letter] != match_vec[letter] ) { 
                                break
                        }   
                        else if(letter == length(comp_vec)) {
                                is_match = T 
                        }   
            
                }   
                if(is_match) {
                        pos <- c(pos, i)
                }   
        }   
        return(pos)
}

#find k-mer clumps in L sized window of freq at least t, in text
find_clumps <- function(text, k, L, t) {
        patterns <- c() 
        n <- nchar(text)
        for( i in 0:(n-L) ) { 
                window <- Text(text, i, L)
                freq_map <- frequency_table(window, k)
                for( key in names(freq_map) ) { 
                        if( freq_map[[ key ]] >= t ) { 
                                patterns <- c(patterns, key)
                        }   
                }   
        }   
        patterns <- unique(patterns)
        return(patterns)
}

find_clumps_fast <- function(text, k, L, t) {
        patterns <- c()
        n <- nchar(text)

        #Count frequencies in first window
        window <- Text(text, 0, L)
        freq_map <- frequency_table(window, k)
        for( key in names(freq_map) ) {
                if( freq_map[[ key ]] >= t ) {
                        patterns <- c(patterns, key)
                }
        }

        #Shift window and make changes to freq_map based on previous first and new last element
        #Note: Text() uses 0-indexing by default
        for( i in 1:(n-L) ) {
                prev_first <- Text(text, i-1, k)
                new_last <- Text(text, i+L-k, k)

                freq_map[[ prev_first ]] <- freq_map[[ prev_first ]] - 1
                if( !(new_last %in% names(freq_map)) ) {
                        freq_map[[ new_last ]] <- 0
                }
                else {
                        freq_map[[ new_last ]] <- freq_map[[ new_last ]] + 1
                }
               
		#Check if adding new_last makes a new valid pattern entry
		if( freq_map[[ new_last ]] >= t ) {
                        patterns <- c(patterns, key)
                }
        }

        patterns <- unique(patterns)
        return(patterns)
}

#Skew at every position in text
Skew_total <- function(text) {
        skews <- c(0)
        text_vec <- strsplit(text, '')[[1]]

        for(base in text_vec) {
                if(base == "A" || base == "T") {
                        skews <- c(skews, skews[ length(skews) ])
                }   
                else if(base == "C") {
                        skews <- c(skews, skews[ length(skews) ]-1 )
                }   
                else if(base == "G") {
                        skews <- c(skews, skews[ length(skews) ]+1 )
                }   
        }   
        return(skews)
}

#index1 = T assumes that the empty string is at position ind=1
Skew <- function(text, ind, index1 = F) {
        #Return item from total at index
        total_skews <- Skew_total(text)
        if(index1) {
                return( total_skews[ ind ] ) 
        }   
        else {
                return( total_skews[ ind+1 ] )   
        }
}

#Find hamming distance between 2 strings
Hamming <- function(text1, text2) {
        text1_vec <- strsplit(text1, '')[[1]]
        text2_vec <- strsplit(text2, '')[[1]]
        if( length(text1_vec) != length(text2_vec) ) { 
                print("Strings must be same length!")
		print(text1)
		print(text2)
                q() 
        }   
        dist <- 0
        for( ind in 1:length(text1_vec) ) { 
                if(text1_vec[ind] != text2_vec[ind]) {
                        dist <- dist + 1 
                }   
        }   
        return(dist)
}

#find starting of pattern or matches w/in distance d of pattern in text
approx_match <- function(pattern, text, d, index0=T) {
        matches <- c() 
        n <- nchar(pattern)
        for( ind in 1:(nchar(text)-nchar(pattern)+1) ) { 
                subset <- Text(text, ind, n, index0=F)
                if( Hamming(subset, pattern) <= d ) { 
                        #Need to substract 1 from index if using index0
                        matches <- c(matches, ind-as.numeric(index0))   
                }   
        }   
        return(matches)
}

#Include with neighbor function
others <- list()
others[[ "A" ]] <- c("T", "C", "G")
others[[ "T" ]] <- c("A", "C", "G")
others[[ "C" ]] <- c("A", "T", "G")
others[[ "G" ]] <- c("A", "T", "C")
neighbors <- function(pattern, d) {
        neighbors <- c() 
        pattern_vec <- strsplit(pattern, "")[[1]]
        for(ind in 1:length(pattern_vec)) {
                base <- pattern_vec[ind]
                for( other in others[[ base ]] ) { 
                        neighbor_vec <- pattern_vec
                        neighbor_vec[ind] <- other
                        #neighbor is a d1 neighbor
                        neighbor <- paste(neighbor_vec, collapse='')
                        #effectively the base case
                        if(d==1) {
                                neighbors <- c(neighbors, neighbor)
                        }   
                        else {
                                neighbors <- c(neighbors, neighbors(neighbor, d-1))
                        }   
                }   
        }   
	neighbors <- neighbors[ neighbors != pattern ]
        return(unique(neighbors))
}

most_freq_with_mismatches <- function(text, k, d) {

        if(F) { #Basically commenting out this block, this was the psuedocode solution since it took me a while to fix my way
        patterns <- c() 
        freq_map <- list()
        n <- nchar(text)
        for(i in 0:(n-k)) {
                pattern <- Text(text, i, k)
                neighborhood <- neighbors(pattern, d)
                for(j in 1:length(neighborhood)) {    
                        neighbor <- neighborhood[j]
                        if(is.null(freq_map[[ neighbor ]])) {
                                freq_map[[ neighbor ]] <- 1
                        }   
                        else {
                                freq_map[[ neighbor ]] <- freq_map[[ neighbor ]] + 1 
                        }   
                }   
        }   
        m <- max_set(freq_map)
        for( kmer in names(freq_map) ) { 
                if( freq_map[[ kmer ]] == m ) { 
                        patterns <- c(patterns, kmer)
                }   
        }   
        return(patterns)
	} #END OF COMMENT BLOCK

        #back to my stuff
        #To anyone reading, my approach, vaguely speaking, was to create the frequency table and then 
        #Fill in the info for each kmer and neighbor based on that table

        #for each entry in frequency table (i.e. kmers actually in text)        
        freq_table <- frequency_table(text, k)
        og_freq_table <- freq_table
        kmers <- unique(names(og_freq_table))
        for(kmer in kmers) {
                #find all it's neighbors
                neighbors <- neighbors(kmer, d)
                for(neighbor in neighbors) {
                        #and then add frequency of kmer to that neighbor
                        if( is.null(freq_table[[ neighbor ]]) ) {
                                freq_table[[ neighbor ]] <- 0
                        }
                        freq_table[[ neighbor ]] <- freq_table[[ neighbor ]] + og_freq_table[[ kmer ]]
                }
        }
	
	#Determine max frequency seq
        max <- max_set(freq_table)
        maxs <- c()
        for(kmer in names(freq_table)) {
                if(freq_table[[ kmer ]] == max) {
                        maxs <- c(maxs, kmer)
                }
        }
        return(maxs)
}

string_from_path <- function(patterns) {
        #pull out last element
        last <- patterns[ length(patterns) ]
        patterns <- patterns[ 1:(length(patterns)-1) ]
        str_peices <- c() 
        #take first letter of each pattern
        for(pattern in patterns) {
                str_peices <- c(str_peices, substr(pattern, 1, 1)) 
        }   
        #add last one back
        first <- paste0(str_peices, collapse='')
        string <- paste0(first, last)
        return(string)
}

composition <- function(k, text) {
        pieces <- c() 
        for(ind in 0:(nchar(text)-k)) {
                pieces <- c(pieces, Text(text, ind, k)) 
        }   
        return(pieces)
}

prefix <- function(pattern) {
        k <- nchar(pattern)
        pre <- substr(pattern, 1, k-1)
        return(pre)
}

suffix <- function(pattern) {
        k <- nchar(pattern)
        suf <- substr(pattern, 2, k)
        return(suf)
}

#overlap in list format
overlap <- function(patterns) {
        overlap <- list()
        for(pattern in patterns) {    
                to <- patterns[ suffix(pattern) == prefix(patterns) ]
                if( length(to) > 0 ) { 
                        overlap[[ pattern ]] <- to
                }   
        }   
        return(overlap)
}

#overlap in 'pattern -> to' format
print_graph <- function(overlap) {
        for(pattern in names(overlap)) {
                cat( paste0(pattern, " -> ", paste(overlap[[pattern]], collapse=","), "\n") )
        }    
}

de_brujin <- function(text, k) {
        n <- nchar(text)
        graph <- list()
        #go thru each k-mer frame and map frame's suffix to prefix 
        for(ind in 0:(n-k)) {
                frame <- Text(text, ind, k)
                if( prefix(frame) %in% names(graph) ) { 
                        graph[[ prefix(frame) ]] <- c(graph[[ prefix(frame) ]], suffix(frame))
                }   
                else {
                        graph[[ prefix(frame) ]] <- suffix(frame)
                }   
        }   
        return(graph)
}

#convert adjacency list in "0 -> 3" format to list object
parse_graph <- function(adjacency_list) {
        new_list <- list()
        for(line in adjacency_list) {
                parts <- strsplit(line, " ")[[1]]
                from <- parts[1]
                to <- parts[3]
                if(is.na(to)) {next}
                to <- strsplit(to, ",")[[1]]
                new_list[[ from ]] <- to
        }   
        return(new_list)
}

#input is list
#The addition of end is to accomodate 3_8_6, makes it so that we don't traverse there early
eulerean_circuit <- function(adjacency_list, end=NA) {
        adj_list <- adjacency_list
        curr <- names(adjacency_list)[1]
        cycle <- c() 
        has_edges <- c() 
        list_has_edge <- T
        repeat {
                #Walk cycle
                start <- curr
                repeat {
                        cycle <- c(cycle, curr)
                        has_edges <- c(has_edges, length(adj_list[[ curr ]]) > 1)
                        
			next_ <- adj_list[[ curr ]][1]
	
                        #remove edge 
                        adj_list[[ curr ]] <- adj_list[[ curr ]][ adj_list[[ curr ]] != next_ ]                 
                        curr <- next_
                        #If we're stuck, stop
                        #This should imply we're back at start of our walk
                        if( is.na(curr) || is.null(curr) || length(curr) == 0 ) {
                                break
                        }
                }   
		#Deal with getting back to start

                #Check if there's any edges left to explore     
                if( !any(has_edges) ) {break}

                #remove last element from cycle since it's same as start point
                #This is here so that if the code ends we'll see the last cycle
                cycle <- cycle[ 1:length(cycle)-1 ]    
                has_edges <- has_edges[ 1:length(has_edges)-1 ]    

                #Select new starting point
                new_start <- cycle[ has_edges ][1]
                new_start_ind <- which( cycle == new_start )[1]

                #shift our walk so we started at new_start
                for(ind in 1:(new_start_ind-1)) {
                        cycle <- c(cycle[-1], cycle[1])
                        has_edges <- c(has_edges[-1], has_edges[1])
                }   
                has_edges[1] <- F
                curr <- new_start
        }   
        return(cycle)
}

#change from vector to "1->2->3 ..."
format_cycle <- function(cycle) {
        return(paste(cycle, collapse='->'))
}

de_brujin_from_patterns <- function(patterns) {
        de_brujin <- list()
        #Map suffix to prefix, adding it if prefix already exists in de_brujin
        for(pattern in patterns) {
                if( is.null(de_brujin[[ prefix(pattern) ]]) ) { 
                        de_brujin[[ prefix(pattern) ]] <- suffix(pattern)
                }   
                else {
                        de_brujin[[ prefix(pattern) ]] <- c(de_brujin[[ prefix(pattern) ]], suffix(pattern))
                }   
        }    
        return(de_brujin)
}

#adjacency_list in actual list object
eulerean_path <- function(adjacency_list) {
        #find start and end
        n_in <- list()
        n_out <- list()
        for( node in names(adjacency_list) ) {
                n_out[[ node ]] <- length(adjacency_list[[ node ]])
                for( target in adjacency_list[[ node ]] ) {
                        if(is.null(n_in[[ target ]])) {
                                n_in[[ target ]] <- 0
                        }
                        n_in[[ target ]] <- n_in[[ target ]] + 1
                }
        }
        all_nodes <- unique( c(names(n_in), names(n_out)) )
        end <- NULL
        start <- NULL
        for( node in all_nodes ) {
                if( is.null(n_in[[ node ]]) ) {
                        n_in[[ node ]] <- 0
                }
                if( is.null(n_out[[ node ]]) ) {
                        n_out[[ node ]] <- 0
                }
                if( n_in[[ node ]] > n_out[[ node ]] ) {
                        end <- node
                }
                if( n_in[[ node ]] < n_out[[ node ]] ) {
                        start <- node
                }   
        }
	
	#flag for if start and end can't be found
        if(is.null(start) || is.null(end)) {
                print("yeah we can't find the start/stop")    
        }   
    
        #draw path from end -> start
        adjacency_list[[ end ]] <- c( adjacency_list[[ end ]], start )
    
        #solve eulerean circuit
        eulerean_cycle <- eulerean_circuit(adjacency_list)
    
        #remove last element of circuit because we're shifting
        eulerean_cycle <- eulerean_cycle[1:(length(eulerean_cycle)-1)] 

        #shift circuit so it starts at start and ends at end
        dont_shift <- ( eulerean_cycle[1] == start && eulerean_cycle[length(eulerean_cycle)] == end )
        new_start_candidates <- which( eulerean_cycle == start )
        if(!dont_shift) {
                for(ind in new_start_candidates) {
                        if( ind == 1 ) {next}
                        if( eulerean_cycle[ind-1] == end ) {
                                new_start_ind <- ind
                                break
                        }
                }
                for(ind in 1:(new_start_ind-1)) {
                        eulerean_cycle <- c(eulerean_cycle[-1], eulerean_cycle[1])
               }
        }

        return(eulerean_cycle)
}

#list of k-length patterns
string_reconstruction <- function(k, patterns) {
        dB <- de_brujin_from_patterns(patterns)
        path <- eulerean_path(dB)
        text <- string_from_path(path)
        return(text)
}

universal_circ_str <- function(k) {
        #create all bin str of length k
        patterns <- c("1","0")
        for(i in 2:k) {
                for(pattern in patterns) {
                        patterns <- c(patterns, paste0(pattern, "0"))
                        patterns <- c(patterns, paste0(pattern, "1"))
                }   
        }   
        patterns <- patterns[ which(nchar(patterns) == k) ]
    
        dB <- de_brujin_from_patterns(patterns)
        cycle <- eulerean_circuit(dB)
        cycle <- cycle[1:(length(cycle)-k+1)]
        text <- string_from_path(cycle) 
        return(text)
}

de_brujin_from_patterns_break <- function(patterns) {
        de_brujin <- list()
        #Map suffix to prefix, adding it if prefix already exists in de_brujin
        for(pattern in patterns) {
                part1 <- strsplit(pattern, "|", fixed=T)[[1]][1]
                part2 <- strsplit(pattern, "|", fixed=T)[[1]][2]
                prefix <- paste0(prefix(part1), ".", prefix(part2))
                suffix <- paste0(suffix(part1), ".", suffix(part2))
                if( is.null(de_brujin[[ prefix ]]) ) { 
                        de_brujin[[ prefix ]] <- suffix
                }   
                else {
                        de_brujin[[ prefix ]] <- c(de_brujin[[ prefix ]], suffix)
                }   
        }    
        return(de_brujin)
}

str_from_gapped_pattern <- function(patterns, k, d) {
        first_patterns <- c() 
        second_patterns <- c() 
        for(pattern in patterns) {
                parts <- strsplit(pattern, ".", fixed=T)[[1]]
                first_patterns <- c(first_patterns, parts[1])   
                second_patterns <- c(second_patterns, parts[2]) 
        }   
    
        prefix <- string_from_path(first_patterns)
        suffix <- string_from_path(second_patterns) 

        prefix_vec <- strsplit(prefix, "")[[1]]
        suffix_vec <- strsplit(suffix, "")[[1]]

        for(i in (k+d+1):nchar(prefix)) {
                if(prefix_vec[i] != suffix_vec[i-k-d]) {
                        return("no string spelled by gapped patterns")
                }   
        }   
        suffix_part <- substr(suffix, nchar(suffix)-(k+d)+1, nchar(suffix))
        combine <- paste0( prefix, suffix_part )
}

string_reconstruction_pair <- function(patterns, k, d) {
        dB <- de_brujin_from_patterns_break(patterns)
        path <- eulerean_path(dB)
        text <- str_from_gapped_pattern(path, k, d)
        return(text)
}

#minimum distance of pattern from some part of text
d <- function(pattern, text) {
	#for i in n-k+1, record hamming distance from k-length window to pattern
	distances <- c()
	for( i in 1:(nchar(text)-nchar(pattern)+1) ) {
		window <- Text(text, i, nchar(pattern), index0=F)
		distances <- c(distances, Hamming(pattern, window) )
	}
	#return minimum distance
	return(min(distances))
}

#kmer in text with minimum hamming distance from pattern
motif <- function(pattern, text) {
	#for i in n-k+1, record hamming distance from k-length window to pattern
	distances <- c()
	for( i in 1:(nchar(text)-nchar(pattern)+1) ) {
		window <- Text(text, i, nchar(pattern), index0=F)
		distances <- c(distances, Hamming(pattern, window) )
	}
	#take first minimum
	return( Text(text, which(distances == min(distances))[1], nchar(pattern), index0=F) )	
}

#sum of minimum distance of pattern from some part of all strings in dna
d_sum <- function(pattern, dna) {
	total <- 0
	for(str in dna) {
		total <- total + d(pattern, str)
	}
	return(total)
}

#return all motifs that appear in all strs w/ up to d mistakes
motif_enumeration <- function(strs, k, d) {
        patterns <- c() 
    
        #get collection of all k-mers in strs
        k_mers <- c() 
        for( str in strs ) { 
                freq_table <- frequency_table(str, k)    
                k_mers <- c(k_mers, names(freq_table))
        }    
    
        tried <- c()    
        for( k_mer in k_mers ) { 
                neighbors <- c(k_mer, neighbors(k_mer, d)) 
                for( neighbor in neighbors ) { 
                        if(neighbor %in% tried) {next}
                        tried <- c(tried, neighbor)
                        in_str <- c() 
                        for( str in  strs ) { 
                                freq_table <- frequency_table(str, k)
                                in_str <- c(in_str, any(c(neighbor, neighbors(neighbor, d)) %in% names(freq_table)))
                        }   
                        if( all(in_str) ) { 
                                patterns <- c(patterns, neighbor)
                        }   
                }   
        }   

        return(unique(patterns))
}

#return kmer with lowest distance in all dna strings
median_str <- function(dna, k) {
        distance <- 999999
        median <- ""
        #get all k_mers to check - for now I'll just do all of them
        k_mers <- c("A","C","G","T")
        for( i in 2:k ) { 
                for( k_mer in k_mers ) { 
                        for( base in c("A","C","T","G") ) { 
                                k_mers <- c(k_mers, paste0(k_mer, base)) 
                        }   
                }   
                k_mers <- k_mers[ which(nchar(k_mers) == i) ]
        }   
    
        for( k_mer in k_mers ) { 
                kmer_dist <- d_sum(k_mer, dna)
                if( distance > kmer_dist ) { 
                        distance <- kmer_dist
                        median <- k_mer
                }   
        }   
        return( median )
}

#return most probably kmer in text from profile
profile_most_probable <- function(text, k, profile) {
        n <- nchar(text)
        probs <- c() 
        #get probability of all kmers in text
        for( i in 1:(n-k+1) ) { 
                window <- Text(text, i, k, index0=F)
                probs <- c(probs, str_prob(window, profile))
        }   

        #pick out max
        max_ind <- which(probs == max(probs))[1]
        return( Text(text, max_ind, k, index0=F) )
}

profile_most_probable_rand <- function(text, k, profile) {
        n <- nchar(text)
	strs <- list()
	for( i in 1:(n-k+1) ) {
		str <- Text(text, i, k, index0=F)
		strs[[ str ]] <- str_prob(str, profile)
	}
	return( sample( names(strs), 1, prob=unlist(strs) ) )
}

#probability of pattern given profile, assumes ncol(profile) == nchar(pattern)
str_prob <- function(pattern, profile) {
        prob <- 1
        pattern_vec <- strsplit(pattern, "")[[1]]
        for( i in 1:nchar(pattern) ) { 
                prob <- prob * as.numeric(profile[ pattern_vec[i], ][i])
        }   
        return(prob)
}

#assume all motifs same length
#if naive, don't add 1 to each count
profile_from_motifs <- function(motifs, naive=F, return_count=F) {
        #generate count matrix
        k <- nchar(motifs[1])
        #creates 4*k matrix of 0s or 1s
        #if not naive, then adding the 1s increases total_entries in each col by 4
        if(naive) {
                count <- matrix( rep(0, len=k*4), nrow=4 )
                total_entries <- length(motifs)
        } else {
                count <- matrix( rep(1, len=k*4), nrow=4 )
                total_entries <- length(motifs) + 4
        }
        rownames(count) <- c("A","C","G","T")
        for(motif in motifs) {
                bases <- strsplit(motif, "")[[1]]
                for(i in 1:length(bases)) {
                        count[ bases[i] ,i] <- count[ bases[i] ,i] + 1
                }
        }

        if(return_count) {return(count)}

        #build profile
        profile <- apply(count, 1:2, function(x) x/total_entries)
        return(profile)
}

#score motif matrix (count number of 'lower case' bases that don't match most freq)
score <- function(motifs) {
        if(length(motifs) < 1) {return(999999)}
        count <- profile_from_motifs(motifs, naive=T, return_count=T)
        col_score <- c() 
    
        for( i in 1:ncol(count) ) { 
                col_score <- c(col_score, sum(count[,i]) - max(count[,i]) )
        }   
        return(sum(col_score))
}

greedy_motif_search <- function(dna, k, t, naive_count=F) {
        #get all kmer motifs in first string
        first_motifs <- names( frequency_table(dna[1], k) )
        best_motifs <- list()

        #see pseudo-code
        for( f_motif in first_motifs ) { 
                temp_motifs <- list()
                temp_motifs[[1]] <- f_motif
                for( i in 2:t ) { 
                        profile <- profile_from_motifs( unlist(temp_motifs), naive=naive_count )
                        greedy_choice <- profile_most_probable(dna[i], k, profile)
                        temp_motifs[[i]] <- greedy_choice
                }   
                if( score(temp_motifs) < score(best_motifs) ) { 
                        best_motifs <- temp_motifs
                }   
        }   
        return(unlist(best_motifs))
}

dp_change <- function(money, coins) {
        min_num_coins <- list()
        min_num_coins[[ "0" ]] <- 0
        for( m in 1:money ) { 
                min_num_coins[[ as.character(m) ]] <- 1e20
                for( coin in coins ) { 
                        if( m >= coin ) { 
                                if( min_num_coins[[ as.character(m - coin) ]] + 1 < min_num_coins[[ as.character(m) ]] ) {
                                        min_num_coins[[ as.character(m) ]] <- min_num_coins[[ as.character(m-coin) ]] + 1
                                }   
                        }   
                }   
        }   
        return( min_num_coins[[ as.character(money) ]] )
}

manhattan <- function(n, m, down, right) {
        #matrix of 0's for each node
        max_mat <- matrix( 0, nrow=n+1, ncol=n+1 )

        #fill top and leftmost col/row
        for( i in 2:(n+1) ) { 
                max_mat[i, 1] <- max_mat[i-1, 1] + down[i-1, 1]
        }   
        for( i in 2:(m+1) ) { 
                max_mat[1, i] <- max_mat[1, i-1] + right[1, i-1]
        }   

        #go thru each row
        for( i in 2:(n+1) ) { 
                #go thru each col
                for( j in 2:(m+1) ) { 
                        max_mat[i,j] <- max( max_mat[i-1,j] + down[i-1, j] , max_mat[i, j-1] + right[i, j-1] )
                }   
        }    
        return(max_mat[n+1,m+1])
}

#small helper to find character at ind in str
char_at <- function(str, ind) {
        return(substr(str, ind, ind))
}

#create DAG where each entry is where to come from to maximize length of LCS
LCS_backtrack <- function(v, w) {
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
        backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)

        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- 0
                        if( char_at(v, i-1) == char_at(w, j-1) ) { 
                                match <- 1
                        }   
                        s[i, j] <- max(s[i-1, j], s[i, j-1], s[i-1, j-1] + match)
                        if( s[i, j] == s[i-1, j] ) { 
                                backtrack[i, j] <- "d" 
                        }   
                        else if( s[i, j] == s[i, j-1] ) { 
                                backtrack[i, j] <- "r" 
                        }   
                        else if( s[i, j] == (s[i-1, j-1] + match) ) { 
                                backtrack[i, j] <- "dr"
                        }
                }
        }
	print(s[nrow(s), ncol(s)])
        return(backtrack)
}

#given the backtrack graph, start from end and figure out path of LCS
output_LCS <- function(backtrack, v, i, j) {
        if(F) { #COMMENT BLOCK
        if( i == 1 || j == 1 ) {return("")}
        if( backtrack[i, j] ==  "d" ) { 
                return( output_LCS(backtrack, v, i-1, j) )
        }   
        else if( backtrack[i, j] ==  "r" ) { 
                return( output_LCS(backtrack, v, i, j-1) )
        }   
        else {
                return( paste0(output_LCS(backtrack, v, i-1, j-1),  char_at(v, i-1)) )
        }   
        } #BLOCK ENDS HERE - CODE WORKS BUT USES WAY TOO MUCH MEMORY

        #procedural method
        str <- c() 
        while( i != 1 && j != 1 ) { 
                if( backtrack[i, j] == "d") {
                        i <- i-1 
                        next
                }   
                else if( backtrack[i, j] == "r") {
                        j <- j-1 
                        next
                }   
                else {
                        str <- c(char_at(v, i-1), str)
                        i <- i-1 
                        j <- j-1 
                }   
        }   
        return(paste(str, collapse=''))
}

#stored with list where each key is a node
#then each key maps to a list of (target, weight) pairs
parse_weighted_graph <- function(adj_list) {
        graph <- list()
        for(entry in adj_list) {
                spl <- strsplit( entry, "->" )[[1]]
                from <- spl[1]
                back <- strsplit( spl[2], ":", fixed=T )[[1]]
                to <- back[1]
                weight <- back[2]

                if( is.null(graph[from]) ) {
                        graph[[ from ]] <- list()
                }

                #I don't want the indexing for nested list to matter
                #So I'm going to just assign in order of adding
                graph[[ from ]][[ as.character(length(graph[[ from ]])) ]] <- c(to, weight)

        }
        return(graph)
}

#find longest path in weighted DAG  given start & end
longest_dag <- function(adj_list, start, end) {
        to_consider <- c(start)
        scores <- list()
        #scores is list of score & pred that gave it that score
        scores[[ start ]] <- c(0, NULL)

        #while I've not checked path, update scores for each 'to' node
        while( length(to_consider) > 0 ) { 
                from <- to_consider[1]
                tos <- adj_list[[ from ]]
                #to[1] is to node, to[2] is weight of that path
                #updates scores of all targets of from
                #some sort of nonsense w/ casting is happening so I have extra casts
                for( to in tos ) { 
                        new_score <- as.numeric(scores[[ from ]][1]) + as.numeric(to[2])
                        if( is.null(scores[[ to[1] ]]) ) { 
                                scores[[ to[1] ]] <- c(new_score, from)
                        }   
                        else if( as.numeric(scores[[ to[1] ]][1]) < as.numeric(new_score) ) { 
                                scores[[ to[1] ]] <- c(new_score, from)
                        }   
                        #add 'to' to to_consider
                        to_consider <- c(to_consider, to[1])
                }   

                #remove first element (one we checked)
                to_consider <- to_consider[-1]
        }   

        #backtrack to find path that got us max_dag
        path <- c() 
        curr <- end 
        while( curr != start ) { 
                path <- c(curr, path)
                curr <- scores[[ curr ]][2]
        }   
        path <- c(start, path)

        return(c(scores[[ end ]][1], format_cycle(path)))
}

topological_ordering <- function(dag) {
        order <- c() 
        all <- unique(c(names(dag), unlist(dag)))
        #being in unlist means node has incoming edge
        srcs <- all[ !(all %in% unlist(dag)) ]
        while( length(srcs) > 0 ) { 
                #pick source, add it to ordering, remove it
                src <- srcs[1]
                order <- c(order, src)
                all <- all[ all != src ]
                srcs <- srcs[ srcs != src ]
                dag[[ src ]] <- NULL
    
                #find new sources
                srcs <- all[ !(all %in% unlist(dag)) ]
        }   
        if( length(unlist(dag)) > 0 ) {return("Input graph is not a DAG")}
        return(order)
}

#sig is indel penalty
global_align <- function(w,v, score_matrix=blosum62, sig=5) {
        #This is mostly copied from book_5_8_5.R w/ some modifications
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
        #calc first row & col (all indels)
        for( i in 2:(nchar(v)+1) ) {
                s[i, 1] <- s[i-1, 1] - sig
        }
        for( j in 2:(nchar(w)+1) ) {
                s[1, j] <- s[1, j-1] - sig
        }

        backtrack <- matrix("", nrow=nchar(v)+1, ncol=nchar(w)+1)
        rownames(backtrack) <- c( strsplit(v, "")[[1]],"-")
        colnames(backtrack) <- c( strsplit(w, "")[[1]],"-")
        #fill in first row and col of backtrack
        for( i in 2:(nchar(v)+1) ) {
                backtrack[i, 1] <- "d"
        }
        for( j in 2:(nchar(w)+1) ) {
                backtrack[1, j] <- "r"
        }

	#create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match)
                        if( s[i, j] == s[i-1, j] - sig) {
                                backtrack[i, j] <- "d"
                        }
                        else if( s[i, j] == s[i, j-1] - sig) {
                                backtrack[i, j] <- "r"
                        }
                        else if( s[i, j] == (s[i-1, j-1] + match) ) {
                                backtrack[i, j] <- "dr"
                        }
                }
        }
        #rebuild strings from backtrack graph
        v_new <- c()
        w_new <- c()
        #every time we cross row/col, we add that letter for cross
        #if we move & don't cross row/col, add a dash
        while( backtrack[i,j] != "" ) { 
                if( backtrack[i, j] == "d") {
                        w_new <- c("-", w_new)
                        v_new <- c(char_at(v, i-1), v_new)
                        i <- i-1 
                        next
                }   
                else if( backtrack[i, j] == "r") {
                        v_new <- c("-", v_new)
                        w_new <- c(char_at(w, j-1), w_new)
                        j <- j-1 
                        next
                }   
                else {
                        v_new <- c(char_at(v, i-1), v_new)
                        w_new <- c(char_at(w, j-1), w_new)
                        i <- i-1
                        j <- j-1
                }
        }
        return(c(s[nrow(s), ncol(s)], paste(v_new, collapse=''), paste(w_new, collapse='')))
}

#writing this here to put into book_functions.R
#build score matrix for 2 strings given indel/mismatch penalty
#this version will assume we're going from (1,1) to (m,n) w/o 'free rides' anywhere
build_s <- function(v, w, sig, match, mismatch, score_matrix=NULL, alphabet="ACTG") {
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
        rownames(s) <- c(strsplit(v, "")[[1]], "-")
        colnames(s) <- c(strsplit(w, "")[[1]], "-")
            
        if( is.null(score_matrix) ) { 
                score_matrix <- build_score_matrix(alphabet, match, mismatch)
        }   
 
        for( i in 2:(nchar(v)+1) ) { 
                s[i, 1] <- s[i-1, 1] - sig 
        }   
        for( j in 2:(nchar(w)+1) ) { 
                s[1, j] <- s[1, j-1] - sig 
        }   
            
        #create backtrack graph
        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        match <- score_matrix[char_at(v,i-1), char_at(w,j-1)]
                        s[i, j] <- max(s[i-1, j] - sig, s[i, j-1] - sig, s[i-1, j-1] + match)
                }   
        }   
	return(s)
}

build_score_matrix <- function(alphabet_str, match, mismatch) {
        alphabet <- strsplit(alphabet_str, "")[[1]]
        score_matrix <- matrix(mismatch, nrow=length(alphabet), ncol=length(alphabet), dimnames=list(alphabet, alphabet))
        for( i in 1:length(alphabet) ) { 
                score_matrix[i,i] <- match
        }   
        return(score_matrix)
}

#solve edit distance problem - largely copied from LCS_backtrack
edit_dist <- function(v, w) {
        s <- matrix(0, nrow=nchar(v)+1, ncol=nchar(w)+1)
        for( i in 2:(nchar(v)+1) ) { 
                s[i, 1] <- i-1 
        }   
        for( j in 2:(nchar(w)+1) ) { 
                s[1, j] <- j-1 
        }   

        for( i in 2:(nchar(v)+1) ) { 
                for( j in 2:(nchar(w)+1) ) { 
                        mismatch <- 1
                        if( char_at(v, i-1) == char_at(w, j-1) ) { 
                               mismatch <- 0
                        }   
                        s[i, j] <- min(s[i-1, j]+1, s[i, j-1]+1, s[i-1, j-1] + mismatch)
                }   
        }   
        return(s[nrow(s), ncol(s)])
}

	middle_edge <- function(v,w, score_matrix=blosum62, sig=5, v_rng=NULL, w_rng=NULL) {
		#NOTE: w is on columns  
		#from source

		from_source <- get_mid_col(v, w, score_matrix=score_matrix, sig=sig)    
		#print(from_source)

		#to_sink
		v_rev <- rev_str(v)
		w_rev <- rev_str(w)
		to_sink <- get_mid_col(v_rev, w_rev, score_matrix=score_matrix, sig=sig, rev=T)

		#sanity checks
		if( length(to_sink) != length(from_source) ) { 
			print("SOMETHING WRONG LENGTH OF TO_SOURCE AND TO_SINK DIFFERENT!")
			q() 
		}   

		#since we calculated to_sink w/ v reversed, we should need to flip it for them to align
		to_sink_rev <- c() 
		for( term in to_sink ) { 
			to_sink_rev <- c(term, to_sink_rev)
		}   
		to_sink <- to_sink_rev

		sum <- to_sink + from_source
		#probably not necessary but I want to select the last one with max
		max_is <- which( max(sum) == sum )
		max_i <- max_is[ length(max_is) ]

		#to find next we need to go back thru reverse and find how we entered the max_i node 
		max_i_rev <- (nchar(v)+1)-max_i+1
		dir <- get_mid_col(v_rev, w_rev, score_matrix=score_matrix, sig=sig, rev=T, into_mid=T, max_i=max_i_rev)

		mid_col <- get_mid_col(v, w, return_col=T)

		#subtract 1 from both indices cus the autograder takes it in index-0
		if( dir == "dr" ) {
			return(paste0(max_i ,",", mid_col , "; ", max_i+1,",", mid_col+1))
		}
		if( dir == "d" ) {
			return(paste0(max_i ,",", mid_col , "; ", max_i+1,",", mid_col))
		}
		if( dir == "r" ) {
			return(paste0(max_i ,",", mid_col , "; ", max_i,",", mid_col+1))
		}
	}

	#reverse strings, not that deep
	rev_str <- function(str) {
		str_vec <- strsplit(str, "")[[1]]
		rev_vec <- c()
		for( letter in str_vec ) {
			rev_vec <- c(letter, rev_vec)
		}
		return(paste(rev_vec, collapse=''))
	}

#get middle column of s in linear memory, rev indicates if it's a reverse string
#into_mid will instead return how we got into the mid node on the reverse
#max_i should be INDEXED ACCORDING TO HOW IT IS IN THE REVERSE S matrix
#return_col signals to just return the index of middle column
get_mid_col <- function(v, w, score_matrix=blosum62, sig=5, rev=F, into_mid=F, max_i=NULL, return_col=F, middle_col=NULL) {
        s <- matrix(0, nrow=nchar(v)+1, ncol=2)
        #just to deal with the fact that an even num of cols will give diff mid col when reversed
	#find where middle column is
        if( (nchar(w)+1) %% 2 == 0 ) { #if ncol even
                if( rev ) {
                        mid_col <- (nchar(w)+1)/2 + 1
                }
                else {
                        mid_col <- (nchar(w)+1)/2
                }
        }
        else { #ncol is odd
                mid_col <- ceiling( (nchar(w)+1)/2 )
        }

        if( return_col ) {
                return(mid_col)
        }

        #set first col & row
       	for( i in 2:nrow(s) ) {
               	s[i, 1] <- s[i-1, 1] - sig
        }
	if( !is.null(middle_col) ) {
		mid_col <- middle_col
	}

        s[1,2] <- (s[1,1]-sig)
        #set second col
        for( i in 2:nrow(s) ) {
                match <- score_matrix[char_at(v, i-1),char_at(w, 1)]
                s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
        }   
 	
	#print(mid_col)
	#traverse over to middle column
        #j represents the current column on the right in terms of the whole thing when done
        for( j in 3:mid_col ) { 
		if( j < 3 ) {break}
                #shift right column to left
                for( i in 1:nrow(s) ) { 
                        s[i,1] <- s[i,2]
                }   
                #set first row
                s[1,2] <- s[1,1] - sig 
                #calc new right column
                for( i in 2:nrow(s) ) { 
                        match <- score_matrix[char_at(v, i-1),char_at(w, j-1)]
                        s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
                }   
        }   
        #At this point s[,2] is the middle column and s[,1] is the one to the right on the reverse
        #The into_mid setting here will let me use what I have to figure out how we got to our mid_edge
        if( into_mid ) { 
		if(max_i == 1) {
                        return("r")
                }   
                else {
                        match <- score_matrix[char_at(v, max_i-1),char_at(w, mid_col-1)]
                } 
                if( s[max_i, 2] == s[max_i-1, 1] + match ) { #came from top-left (bottom-right on reverse)
                        return("dr")
                }   
                else if( s[max_i, 2] == s[max_i, 1] - sig ) { #came from left (right on reverse)
                        return("r")
                }   
                else if( s[max_i, 2] == s[max_i-1, 2] - sig ) { #came from top (bottom on reverse)
                        return("d")
                }
                else {
                        print("something wrong w/ finding how we exited mid")
                        q()
                }
        }
        return(s[,2])
}


#I'm rewriting this and more procedurally because my last one was a complete fucking mess
new_middle_edge <- function(v, w, score_matrix=blosum62, sig=5, v_rng, w_rng) {

	mid_col <- floor( (w_rng[1] + w_rng[2])/2 )	
	
	#traverse thru s
	s <- matrix(0, nrow=nchar(v)+1, ncol=2)
        s[1,2] <- (-sig)
	#set first col & row
        for( i in 2:nrow(s) ) {
                s[i, 1] <- s[i-1, 1] - sig
                match <- score_matrix[char_at(v, i-1),char_at(w, 1)]
                s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
        }
	if( mid_col == 1 ) {
		from_source <- s[,1]
	}
	if( mid_col == 2 ) {
		from_source <- s[,2]
	}
	for( j in 3:mid_col ) {
		if(mid_col < 3) {break}
                #shift right column to left
                for( i in 1:nrow(s) ) {
                        s[i,1] <- s[i,2]
                }
                #set first row
                s[1,2] <- s[1,1] - sig
                #calc new right column
                for( i in 2:nrow(s) ) {
                        match <- score_matrix[char_at(v, i-1),char_at(w, j-1)]
                        s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
		}
		from_source <- s[,2]
	}
	#saving for end since i reused mid_col
	mid <- mid_col
	#REPEATING EVERYTHING ABOVE FOR REVERSE
	v_rev <- rev_str(v)
	w_rev <- rev_str(w)
	#column index for reverse
	mid_col <- nchar(w)+1 - mid_col + 1
	s <- matrix(0, nrow=nchar(v)+1, ncol=2)
	s[1,2] <- (-sig)
	#set first col and row
	for( i in 2:nrow(s) ) {
                s[i, 1] <- s[i-1, 1] - sig
                match <- score_matrix[char_at(v_rev, i-1),char_at(w_rev, 1)]
                s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
        }    
        if( mid_col == 1 ) {
                to_sink <- s[,1]
        }    
        if( mid_col == 2 ) {
                to_sink <- s[,2]
        }  
	for( j in 3:mid_col ) {
                if(mid_col < 3) {break}
                #shift right column to left
                for( i in 1:nrow(s) ) {
                        s[i,1] <- s[i,2]
                }    
                #set first row
                s[1,2] <- s[1,1] - sig
                #calc new right column
                for( i in 2:nrow(s) ) {
                        match <- score_matrix[char_at(v_rev, i-1),char_at(w_rev, j-1)]
                        s[i,2] <- max( s[i,1] - sig, s[i-1,2] - sig, s[i-1,1] + match )
                }    
                to_sink <- s[,2]
        }
	to_sink_rev <- c() 
	for( term in to_sink ) { 
		to_sink_rev <- c(term, to_sink_rev)
	}   
	to_sink <- to_sink_rev
	
	
	sum <- to_sink + from_source
	#only want to consider things in  v_rng
	sum <- sum[ v_rng[1]:v_rng[2] ]
	max_i <- which( sum == max(sum) )[1] + v_rng[1] - 1
	if(max_i == 1) { 
                into_mid <- "H"
        }    
        else {
		print(max_i)
		print(s)
                match <- score_matrix[char_at(v_rev, max_i-1), char_at(w_rev, mid_col-1)]
        	if( s[max_i, 2] == s[max_i-1, 1] + match ) { #came from top-left (bottom-right on reverse)
        	        into_mid <- "D"
        	}    
        	else if( s[max_i, 2] == s[max_i, 1] - sig ) { #came from left (right on reverse)
        	        into_mid <- "H"
        	}    
       	 	else if( s[max_i, 2] == s[max_i-1, 2] - sig ) { #came from top (bottom on reverse)
               		into_mid <- "V"
        	}    
        	else {
                	print("something wrong w/ finding how we exited mid")
                	q()  
        	}  
        }
	if( into_mid == "D" ) {
		return( paste0( max_i, ",", mid, ";", max_i+1, ",", mid+1 ) )
	}
	if( into_mid == "H" ) {
		return( paste0( max_i, ",", mid, ";", max_i, ",", mid+1 ) )
	}
	if( into_mid == "V" ) {
		return( paste0( max_i, ",", mid, ";", max_i+1, ",", mid ) )
	}
}	

#as.str is if path is string or vector
parse_alignment_path <- function(path, v, w, as.str=F) {
        if(as.str) {
		path <- strsplit(path, '')[[1]]
	}
	print(v)
	print(w)
	print(path)
        v_vec <- c() 
        v_at <- 1
        w_vec <- c() 
        w_at <- 1
        for( letter in path ) { 
                if( letter == "H" ) { 
                        v_vec <- c(v_vec, "-")
                        w_vec <- c(w_vec, char_at(w, w_at))
                        w_at <- w_at + 1 
                }   
                else if (letter == "V") {
                        w_vec <- c(w_vec, "-")
                        v_vec <- c(v_vec, char_at(v, v_at))
                        v_at <- v_at + 1 

                }   
                else {
                        w_vec <- c(w_vec, char_at(w, w_at))
                        v_vec <- c(v_vec, char_at(v, v_at))
                        v_at <- v_at + 1 
                        w_at <- w_at + 1 
                }   
        }   
        w_new <- paste(w_vec, collapse='')
        v_new <- paste(v_vec, collapse='')
        return( paste0(v_new, ", ", w_new) )
}

#input is text in (+1 -2 -3 +4) format
#output is vector of numeric values
text_to_chromosome <- function(text) {
        text_chrs <- strsplit(text, "")[[1]]
        text_chrs <- text_chrs[2:(length(text_chrs)-1)]
        text <- paste(text_chrs, collapse="")
        chromosome <- as.integer(strsplit(text, " ")[[1]])
        return(chromosome)
}

#input is vector of numbers representing chromosome syntany blocks
#output is cycle as vector
chromosome_to_cycle <- function(chromosome) {
        cycle <- c() 
        for(num in chromosome) {
                if(num < 0) {
                        num <- -(num)
                        cycle <- c(cycle, 2*num, (2*num)-1)
                }   
                else {
                        cycle <- c(cycle, (2*num)-1, 2*num)
                }   
        }   
        return(cycle)
}

#output vector chromosome as "(+1 -2 -3 +4)" form
print_chromosome <- function(chromosome) {
        str <- c() 
        for( block in chromosome ) { 
                if( block > 0 ) { 
                        str <- c(str, paste0("+", block))
                }   
                else {
                        str <- c(str, block)
                }   
        }   
        str <- paste(str, collapse=" ")
        return(paste0("(", str, ")"))
}

#input is cycle as numeric vector
cycle_to_chromosome <- function(cycle) {
        chromosome <- c() 
        for( i in seq(1, length(cycle), by=2) ) { 
                if( cycle[i] < cycle[i+1] ) { 
                        chromosome <- c(chromosome, (cycle[i+1]/2))
                }   
                else {
                        chromosome <- c(chromosome, -(cycle[i]/2))
                }   
        }   
        return(chromosome)
}

#Chromosomes is a list of vectors w/ each chromosomes sytany blocks
colored_edges <- function(chromosomes, dbl=F) {
        edges <- list()
        for( chromosome in chromosomes ) { 
                cycle <- chromosome_to_cycle(chromosome)
                for( i in seq( 2, length(cycle)-1, by=2 ) ) { 
                        edges[[ as.character(cycle[i]) ]] <- as.character(cycle[i+1])
			if( dbl ) {
				edges[[ as.character(cycle[i+1]) ]] <- as.character(cycle[i])
			}
                }   
		if( dbl ) {
			edges[[ as.character(cycle[1]) ]] <- as.character(cycle[length(cycle)])
		}
                edges[[ as.character(cycle[length(cycle)]) ]] <- as.character(cycle[1])
        }    
        return(edges)
}

#convert from R list to (2, 4), (3, 6), ... format
print_colored_edges <- function(edges) {
        str <- c() 
        for( i in names(edges) ) { 
                str <- c(str, paste0("(", i,", ", edges[[i]], "), "))
        }   
        str <- paste(str, collapse='')
        str <- substr(str, 1, nchar(str)-2)
        return(str)
}

#convert from R list to "(+1 -2 -3)(-4 +5 -6)" format
#This is messy and the naming is bad
print_genome <- function(genome) {
        str <- c() 
        for( name in names(genome) ) { 
                vec <- genome[[ name ]]
                nums <- c() 
                for( i in vec ) { 
                        if( as.numeric(i) > 0 ) { 
                                nums <- c(nums, paste0("+", i)) 
                        }   
                        else {
                                nums <- c(nums, as.character(i))
                        }   
                }   
                str <- c(str, paste0("(", paste(nums, collapse=" "),")"))
        }   
        return(paste(str, collapse=""))
}

#Go from "(2,4), (3,6), ..." format to R list
#list has the numbers as chars
#dbl option will make each edge listed in both directions
parse_colored_edges <- function(str, dbl=F) {
        edges <- list()
        points <- strsplit(str, ")")[[1]]
        for( i in 1:length(points) ) { 
                points[i] <- strsplit(points[i], "(", fixed=T)[[1]][2]
                nums <- strsplit(points[i], ", ")[[1]]
		if( is.null(edges[[ nums[1] ]]) ) {
                	edges[[ nums[1] ]] <- nums[2]
		}
		else {
			edges[[ nums[1] ]] <- c(edges[[ nums[1] ]], nums[2])
		}
		if(dbl) {
			if( is.null(edges[[ nums[2] ]]) ) {
				edges[[ nums[2] ]] <- nums[1]
			}
			else {
				edges[[ nums[2] ]] <- c(edges[[ nums[2] ]], nums[1])
			}
		}
        }   
        return(edges)
}

#input is edges as R list
graph_to_genome <- function(edges) {
        genome <- list()
        visited <- c() 
        for( from in names(edges) ) { 
                cycle <- c() 
                if(from %in% visited) {
                        next
                }   
                curr <- from
                repeat {
                        to <- as.numeric(edges[[ as.character(curr) ]]) 
                        visited <- c(visited, curr)
                        if( to %% 2 == 0 ) { #if to is even
                                nxt <- to-1
                                nxt_block <- -(to/2)
                        }   
                        else {
                                nxt <- to+1
                                nxt_block <- (to+1)/2
                        }   
                        curr <- nxt 
                        cycle <- c(cycle, nxt_block)
                        if( nxt == from ) { #stop once we're back at end of cycle
                                break
                        }   
                }   
                #this is purely aesthetic so order in order given by examples
                cycle <- c(cycle[length(cycle)], cycle[ 1:(length(cycle)-1) ])
                genome[[ from ]] <- cycle #index cycle by some cycle start
        }   
        return(genome)
}

#from (+1 -3 -6 -5)(+2 -4) to R list of vectors
#It's helpful to think of genome as a collection of chromosomes
parse_genome <- function(genome) {
        genome <- strsplit(genome, ")", fixed=T)[[1]]
        genome_list <- list()
        for( i in 1:length(genome) ) {
                chromosome <- genome[i]
                chromosome <- substr(chromosome, 2, nchar(chromosome))
                genome_list[[ as.character(i) ]] <- as.numeric(strsplit(chromosome, " ")[[1]])
        }
        return(genome_list)
}

#input is genome as list
#output is list of cycles (convert chromosomes to cycle format)
genome_to_cycles <- function(genome) {
        for( chrom in names(genome) ) { 
                chromosome <- genome[[ chrom ]]
                as_cycle <- chromosome_to_cycle( chromosome )
                genome[[ chrom ]] <- as_cycle
        }   
        return(genome)
}

#remove edge but w/ each edge listed in both directions
remove_edge <- function(edges, to, from) {
	to <- as.character(to)
	from <- as.character(from)
        edges[[ from ]] <- edges[[ from ]][ which(edges[[ from ]] != to) ]
        edges[[ to ]] <- edges[[ to ]][ which(edges[[ to ]] != from) ]

        if( length(edges[[from]]) < 1) {
                edges[[from]] <- NULL
        }   
        if( length(edges[[to]]) < 1) {
                edges[[to]] <- NULL
        }   
        return(edges)
}

#remove empty vectors from list
clean_list <- function(edges) { #OBSOLETE NOW
        for( name in names(edges) ) {
                if(length(edges[[name]]) < 1) {
                        edges[[name]] <- NULL
                }
        }
        return(edges)
}

#input is P and Q as list of cycles
two_break_dist <- function(P, Q) {
        #find n_block by finding number of unique syntenty blocks 
        blocks <- c()
        for( cycle in P ) {
                for( i in names(P) ) {
                        blocks <- c(blocks, P[[i]])
                }
        }   
        for( cycle in Q ) { 
                for( i in names(Q) ) { 
                        blocks <- c(blocks, Q[[i]])
                }   
        }   
        blocks <- abs(blocks)
        n_block <- length(unique(blocks))

        P_edges <- colored_edges(P)
        Q_edges <- colored_edges(Q)
	#combined graphs
	P_Q <- list()
        for( from in names(P_edges)) {
                P_Q[[ from ]] <- P_edges[[ from ]]
        }
        for( from in names(Q_edges)) {
                if( is.null(P_Q[[ from ]]) ) {
                        P_Q[[ from ]] <- Q_edges[[ from ]]
                }
                else {
                        P_Q[[ from ]] <- c(P_Q[[ from ]], Q_edges[[ from ]])
                }   
        }   
        #traverse thru cycles to find n_cycles
        n_cycle <- 0
        #I'm also going to make each edge double sided
        for( from in names(P_Q) ) { 
                tos <- P_Q[[ from ]]
                for( to in tos ) { 
                        if( is.null(P_Q[[ to ]]) ) { 
                                P_Q[[ to ]] <- from
                        }   
                        else {
                                P_Q[[ to ]] <- c(P_Q[[ to ]], from)
                        }   
                }   
        }   
        while( length( names(P_Q) > 0 ) ) { 
                #pick some edge, first in P_Q
                from <- names(P_Q)[1]
                to <- P_Q[[ from ]][1]
                #traverse edges until cannot
                while( !is.null(to) ) { 
                        P_Q <- remove_edge(P_Q, to, from)
                        from <- to
                        to <- P_Q[[ from ]][1]
                }
                #increment cycles
                n_cycle <- n_cycle + 1
        }
        dist <- n_block - n_cycle
	return(dist)
}

library("R6")
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

#I'm going to recurse thru - print in from->to:edge_label format
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

SuffixTreeNode <- R6Class(
        "SuffixTreeNode",
        public = list(
                label=NULL,
                outs=NULL,
                out_labels=NULL,
                num = NULL,
                pos = NULL,
                len = 1,
		color = NULL,
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
                                curr$num <- i
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
		curr_loc <- 1
		num_loc <- NULL
                while( length(curr$outs) == 1 ) { 
			if( !is.null(curr$num) ) {
				num_loc <- curr_loc
			}
                        curr <- curr$outs[[1]]
                        len <- len + 1 
			curr_loc <- curr_loc + 1
                }   
                curr$len <- len 
                curr$pos <- child$pos
		curr$num <- num_loc
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

burrows_wheeler_transform <- function(text) {
        #create cyclic rotations
        chars <- strsplit(text, "")[[1]]
        rotations <- c(text)
        for( i in 2:length(chars) ) { 
                prev <- rotations[length(rotations)]
                prev_chars <- strsplit(prev, "")[[1]]
                rotate_chars <- prev_chars[ c(length(prev_chars), 1:(length(prev_chars)-1)) ]
                rotations <- c(rotations, paste(rotate_chars, collapse=''))
        }   
        order <- order(rotations)
        rotations <- rotations[order] #order rotations
        #pull out last char from each
        transform <- c() 
        for( rotation in rotations ) { 
                transform <- c(transform, substr(rotation, nchar(rotation), nchar(rotation)))
        }   
        return(paste(transform, collapse=''))
}

suffix_array <- function(text, index0=F) {
        suffixes <- c() 
        chars <- strsplit(text, "")[[1]]
        for( i in 1:length(chars) ) { 
                suffix_chars <- chars[i:length(chars)]
                suffixes <- c(suffixes, paste(suffix_chars, collapse=''))
        }   
        #I just found order kinda accidentally but it works
        return( order(suffixes)-as.numeric(index0) ) #subtract 1 for index0
}

rev_burrows_wheeler <- function(str) {
        chars <- strsplit(str, "")[[1]]
        first <- chars[order(chars)]
        f_labelled <- c() 
        c_labelled <- c() 
        #label instances each letter of my first and last col
        for( i in 1:length(first) ) { 
                f_labelled <- c(f_labelled, first[i])
                c_labelled <- c(c_labelled, chars[i])
                first[i] <- paste0(first[i], length(which(f_labelled==first[i])))
                chars[i] <- paste0(chars[i], length(which(c_labelled==chars[i])))
        }   

        #untransform by adding where current char in first maps to chars
        rev <- c(first[ which(chars=="$1") ])
        while( rev[length(rev)] != "$1" ) { 
                rev <- c(rev, first[ which(chars==rev[length(rev)]) ])
        }   

        #remove numbers from result
        rev_no_nums <- c() 
        for( letter in rev ) { 
                rev_no_nums <- c(rev_no_nums, substr(letter, 1, 1)) 
        }   

        return( paste(rev_no_nums, collapse='') )
}

#distance from point to closest center
d <- function(point, centers) {
        d <- 1e9 
        for( center in centers ) { 
                d <- min(d, euclidean_dist(point, center))
        }   
        return(d)
}

#find Euclidean dist between 2 points, assume that same dim
euclidean_dist <- function(point1, point2) {
        m <- length(point1)
        square <- 0
        for( i in 1:m ) { 
                square <- square + (point1[i] - point2[i])^2
        }   
        return( sqrt(square) )   
}

print_points <- function(points) {
        for( point in points ) { 
                cat(point)
                cat("\n")
        }   
}
