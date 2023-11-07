def index_fasta(fname, outname="data"):
	with open(fname) as f:
		string = ""
		index = []
		for line in f:
			line = line.strip()
			if line == "":
				continue	
			if line[0] == ">":
				string += "@"
				parts = line.split()
				id_info = parts[0].split("|")
				gi = id_info[1]
				index.append(gi + " " + str(len(string)-1))
			else:
				string += line
		else:
			donothing=1
	with open(outname + ".seq", "w") as o:
		o.write(string[1:] + "\n") #remove first @symbol
	with open(outname + ".in", "w") as o:
		for line in index:
			o.write(line + "\n")

index_fasta("datafile.txt")
