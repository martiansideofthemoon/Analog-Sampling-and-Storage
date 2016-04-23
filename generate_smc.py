import random
output = "";
data = {}
for k in range(0, 10000):
	i = random.randrange(0, 2)
	addr = 0
	number = 0
	if len(data) == 0:
		i = 1
	if i == 1:
		#write data
		addr = random.randrange(0, 256)
		number = random.randrange(0, 256)
		data[addr] = number
	elif i == 0:
		#read data
		addr = random.choice(data.keys())
		number = data[addr]
	output += '{0:01b}'.format(i)  + ' {0:013b}'.format(addr) + ' {0:08b}'.format(number) + "\n";
f = open('TRACEFILE_SMC.txt', 'w')
f.write(output)
f.close()