from collections import Counter

myfile = open('2.txt', 'r')
contents = myfile.read().strip().splitlines()
myfile.close()

c = [0, 0]
for i in contents:
    a = [j for i,j in Counter(i).most_common()]
    if 3 in a:
        c[0] += 1
    if 2 in a:
        c[1] += 1


print(c[0] * c[1])
