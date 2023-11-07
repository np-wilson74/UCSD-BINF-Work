import argparse
import sys
import numpy as np
from numpy import unravel_index
import math

if __name__ == "__main__":
	#Read in data
	parser = argparse.ArgumentParser()

	parser.add_argument("file")
	parser.add_argument("-m", "--match", help="Match score", default=1, type=int)
	parser.add_argument("-s", "--mismatch", help="Mismatch score", default=-10, type=int)
	parser.add_argument("-d", "--indel", help="Indel score", default=-1, type=int)
	parser.add_argument("-a", "--alignment_only", action='store_true')

	args = parser.parse_args()

	fname = sys.argv[1]
	match = args.match
	mismatch = args.mismatch
	indel = args.indel
	alignment_only = args.alignment_only
	read1 = ""
	read2 = ""

	with open(fname, "r") as f:
		for line in f:
			if line.strip() == "":
				continue
			elif line.strip()[0] == ">":
				continue
			elif read1 == "":
				read1 = line.strip()
			else:	
				read2 = line.strip()
	#pick out shorter of x,y, makes sure I can do more efficient align
	if len(read1) > len(read2):
		v = read2
		w = read1
	else:
		v = read1
		w = read2

def linear_find_max_score(v, w, m, mm, ind):
	match = m
	mismatch = mm
	indel = ind
	first_col = np.zeros(len(v)+1, dtype=int)
	second_col = np.zeros(len(first_col), dtype=int)
	table = np.column_stack((first_col, second_col))
	max_val = 0
	max_i = -1
	max_j = -1
	for j in range(1, len(w)-1):
		table[0, 1] = 0
		for i in range(1, len(v)+1):
			if v[i-1] == w[j-1]:
				match_score = match
			else:
				match_score = mismatch 
			table[i, 1] = max(0, table[i-1, 0]+match_score, table[i-1, 1]+indel, table[i,0]+indel)
			if table[i, 1] > max_val:
				max_val = table[i, 1]
				max_i = i
				max_j = j 
		table[:, 0] = table[:, 1]
	return((max_i, max_j, max_val))



def global_align(v, w, m, mm, ind):
	#PARAMETERS:
	#@ v: str, shorter string to align
	#@ w: str, longer string to align
	#@ m, mm, ind: int, match, mismatch, indel scores, respectively
	#RETURNS:
	#@ actual alignment as 2 strings
	match = m
	mismatch = mm
	indel = ind
	s = np.empty((len(v)+1, len(w)+1), dtype=int)	
	if indel != 0:
		s[:,0] = range(0, indel*(len(v)+1), indel)
		s[0,:] = range(0, indel*(len(w)+1), indel)
	backtrack = np.empty((len(v)+1, len(w)+1), dtype=str)
	#fill first col & row
	backtrack[:,0] = ["V"] * (len(v)+1)
	backtrack[0,:] = ["H"] * (len(w)+1)
	backtrack[0,0] = "S"	

	for i in range(1, len(v)+1):
		for j in range(1, len(w)+1):
			if v[i-1] == w[j-1]:
				match_score = match
			else:
				match_score = mismatch
			s[i,j] = max( s[i-1,j-1]+match_score, s[i-1, j]+indel, s[i, j-1]+indel)
			if s[i, j] == s[i-1,j-1]+match_score:
				backtrack[i,j] = "D"
			elif s[i, j] == s[i-1, j] + indel:
				backtrack[i,j] = "V"
			elif s[i, j] == s[i, j-1] + indel:
				backtrack[i,j] = "H"
	v_align = ""
	w_align = ""
	i = len(v)
	j = len(w)
	while backtrack[i, j] != "S":
		if backtrack[i, j] == "D":
			v_align = v[i-1] + v_align
			w_align = w[j-1] + w_align
			i -= 1
			j -= 1
		elif backtrack[i, j] == "H":
			v_align = "-" + v_align
			w_align = w[j-1] + w_align		
			j -= 1
		else:
			v_align = v[i-1] + v_align
			w_align = "-" + w_align
			i -= 1

	return( ''.join(v_align) + "$" + ''.join(w_align) )

def psuedo_linear_space_align(v, w, m, mm, ind):
	#PARAMETERS:
	#@ v: str, shorter string to align
	#@ w: str, longer string to align
	#@ m: int, match score
	#@ mm: int, mismatch score
	#@ ind: int, indel score
	#RETURNS:
	#@ score of alignment
	#@ length of alignment
	#@ actual alignment as 2 strings
	#DESCRIPTION:
	# - First, use lin space to find where alignment ends
	# - Second, use lin space to find where alignment starts by reversing strings
	# - Third, since we're assuming the alignment is small with respect to the input,
	# just substring the inputs based on start and end and run normal global align
	# I call this pseudo since i could be using middle edges and such for the global align
	# but that makes it like 100x harder 
	max_i, max_j, max_val = linear_find_max_score(v,w, m, mm, ind)
	v_rev = v[::-1]
	w_rev = w[::-1]
	max_i_rev, max_j_rev, max_val_rev = linear_find_max_score(v_rev, w_rev, m, mm, ind)
	max_i_rev = len(v) - max_i_rev
	max_j_rev = len(w) - max_j_rev
	align = global_align(v[max_i_rev:max_i], w[max_j_rev:max_j], m, mm, ind)
	return((align, max_val))
	
if __name__ == "__main__":
	alignment, score = psuedo_linear_space_align(v, w, match, mismatch, indel)
	alignments = alignment.split("$")
	
	if alignment_only:
		print(alignments[0])
		print(alignments[1])
	else:
		print("Score: " + str(score))
		print("Length: " + str(len(alignments[0])))
		print(alignments[0])
		print(alignments[1])
