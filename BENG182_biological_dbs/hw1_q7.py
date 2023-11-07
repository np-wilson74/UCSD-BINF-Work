def getSeq(query, fname="data"):
	with open(fname + ".seq", "r") as s:
		seq = s.readline().strip()
	with open(fname + ".in", "r") as i:
		index = []
		for line in i:
			index.append(line.strip())
	#convert list to dict (or R list)
	ind = {}
	for line in index:
		vals = line.split()
		ind[vals[1]] = vals[0]
	#I'm using the python built-in implementation, but I could probably do something faster from 181
	#I also wrote that code all in R, so it'd be very tedious to implement here
	query_pos = seq.find(query)
	#I can do this really easily in R, and I'll probably figure out how to do it w/ numpy eventually
	valid_positions = []
	for position in ind.keys():
		if int(position) < int(query_pos):
			valid_positions.append(int(position))
	largest_smaller_pos = max(valid_positions) #find largest seq start smaller than query
	print(ind[str(largest_smaller_pos)])

getSeq("MHIQITDFGTAKVLSPDS")
