def cat(fname):
	with open(fname) as f:
		print(f.readline().strip()) #assume that first line is a header
		count = 0
		for line in f:
			line = line.strip()
			if line == "":
				continue
			if line[0] == ">":
				print(count)
				print(line)	
				count = 0
			else:
				count += len(line)
		else:
			print(count)

cat("datafile.txt")
