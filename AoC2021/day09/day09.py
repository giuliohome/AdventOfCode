f = open('input.txt')
lines = [l.strip() for l in f.readlines()]

inputs = [[int(j) for j in i] for i in lines]

hlen = len(lines[0]) # 100
vlen = len(lines) # 100

def adj1(l,i):
  if i == 0:
    return [1]
  elif i == l-1: 
    return [l-2]
  else: 
    return [i-1,i+1]
 
def adj(i,j):
  return [(ii,j) 
    for ii in adj1(vlen,i)
    ] + [(i,jj) 
    for jj in adj1(hlen,j)]

min_s = []
for i in range(0,vlen):
  for j in range(0, hlen):
    if inputs[i][j] < min([inputs[ii][jj] for (ii,jj) in adj(i,j)]):
      min_s += [inputs[i][j] + 1]

print(f'part 1: {sum(min_s)}') # 423

def sizebasin(i,j):
  if inputs[i][j]<9:
    inputs[i][j] = 9
    return 1 + sum(sizebasin(ii,jj) for (ii,jj) in adj(i,j))
  else: return 0

basins = []
for i in range(0,vlen):
  for j in range(0, hlen):
    if inputs[i][j] < 9:
      basins += [sizebasin(i,j)]

basins.sort()
print(f'part 2: { basins[-1]*basins[-2]*basins[-3]  }') # 1198704

