import sys 
import random

num_seq = int(sys.argv[1])
len_seq = int(sys.argv[2])

nucs = ['A','G','C','T']

for i in range(num_seq):
	dna = ""
	for j in range(len_seq):
		dna += random.sample(nucs,1)[0]
	print(dna)
