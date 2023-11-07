import locAL

seqs = []
with open("q2seqs1000.txt", "r") as f:
	for line in f:
		seqs.append(line.strip())

P1_lens = []
P2_lens = []
P3_lens = []
P4_lens = []
P5_lens = []
P6_lens = []
for i in range(len(seqs)-1):
	P1_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -30, -30)[0].split("$")[0]) )
	P2_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -20, -20)[0].split("$")[0]) )
	P3_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -5, -5)[0].split("$")[0]) )
	P4_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -1, -1)[0].split("$")[0]) )
	P5_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -1, -20)[0].split("$")[0]) )
	P6_lens.append( len(locAL.psuedo_linear_space_align(seqs[i], seqs[i+1], 1, -20, -1)[0].split("$")[0]) )
	print(i)

with open("lens_-30_-30.txt", "w") as f:
	for length in P1_lens:
		f.write(str(length) + "\n")
with open("lens_-20_-20.txt", "w") as f:
	for length in P2_lens:
		f.write(str(length) + "\n")
with open("lens_-5_-5.txt", "w") as f:
	for length in P3_lens:
		f.write(str(length) + "\n")
with open("lens_-1_-1.txt", "w") as f:
	for length in P4_lens:
		f.write(str(length) + "\n")
with open("lens_-1_-20.txt", "w") as f:
	for length in P5_lens:
		f.write(str(length) + "\n")
with open("lens_-20_-1.txt", "w") as f:
	for length in P6_lens:
		f.write(str(length) + "\n")
