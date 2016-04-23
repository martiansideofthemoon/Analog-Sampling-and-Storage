import random
output = "";
data = []
read_point = 0
write_point = 0
for k in range(0, 25):
	i = random.randrange(0, 2)
	addr = 0
	number = 0
	if write_point <= read_point and len(data) < 8192:
		i = 1
	if i == 1:
		#write data
		number = random.randrange(0, 256)
		if len(data) <= write_point:
			data.append(number)
		else:
			data[write_point] = number
		output += '{0:01b}'.format(i)  + ' {0:014b}'.format(write_point) + ' {0:08b}'.format(number) + "\n";
		write_point+=1;
		if write_point >= 8192:
			write_point = 0
	elif i == 0:
		number = data[read_point]
		output += '{0:01b}'.format(i) + ' {0:014b}'.format(read_point) + ' {0:08b}'.format(number) + "\n";
		read_point+=1;
		if read_point >= 8192:
			read_point = 0
f = open('TRACEFILE_TopLevel.txt', 'w')
f.write(output)
f.close()