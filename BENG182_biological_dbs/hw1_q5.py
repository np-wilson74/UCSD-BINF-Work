def filter_fasta(fname):
	with open(fname) as f:
		lines = f.read().splitlines() #Ik this isn't space efficient but makes peeking easier
	lines = remove(lines, "")
	for i in range(0, len(lines)): 
		line = lines[i]
		if line[0] == ">":
			parts = line.split()
			valid = False
			if ("[Mus" in parts) or ("[Rattus" in parts):
				valid = True
			ID = parts[0].split("|")
			ID = remove(ID, "")
			if len(ID) > 4:
				#organism ID, only seems to be here when no species identifier at end
				org_id = ID[4] 
				if ("RAT" in org_id) or ("MOUSE" in org_id):
					valid = True
			if valid: #then print header + sequence
				print(line)
				#peek ahead to next header				
				curr = i+1
				seq = ""
				while curr != len(lines) and ">" not in lines[curr]:
					seq += lines[curr]
					curr += 1
				#print seq by every 60 chars
				while( len(seq) > 60 ):
					print(seq[0:60])
					seq = seq[60:]
				print(seq)

#idk why there isn't an easy built-in way to do this...
def remove(arr, val):
	while val in arr:
		arr.remove(val)
	return(arr)

filter_fasta("datafile.txt")


