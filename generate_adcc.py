import random
output = "";
for k in range(0, 10):
	i = random.randrange(0, 256)
	output += '{0:08b}'.format(i) + "\n";
f = open('TRACEFILE_ADCC.txt', 'w')
f.write(output)
f.close()