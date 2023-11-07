options(warn=-1)
setwd("~/Desktop/School/BENG_181/Practice_Code")
source("book_functions.R")

#reading in data
args <- commandArgs(trailingOnly=TRUE)
fname <- args[1]
lines <- readLines(fname)
money <- as.numeric(lines[1])
coins <- as.numeric(strsplit(lines[2], ",")[[1]])

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

#ignore this I don't think it's a good strategy
dp_change_broken <- function(money, coins) {
	min_num_coins <- list()
	#build min_num_coins to show #of each coin denomination to "fill up" to money as close as possible
	for( coin in coins ) {
		min_num_coins[[ as.character(coin) ]] <- as.integer( money/coin )
		money_rem <- money %% coin 
		largest_coin <- coin
		while( money_rem > 0 ) {
			min_sm_coins <- list()
			#Fill up with smaller coins
			sm_coins <- coins[ coins < largest_coin ]
			for( sm_coin in sm_coins ) {
				min_sm_coins[[ as.character(sm_coin) ]] <- as.integer( money_rem/sm_coin )
			}
		}
	}

}

print(dp_change(money, coins))

