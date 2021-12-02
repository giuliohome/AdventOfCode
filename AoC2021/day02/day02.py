

f = open('input.txt','r')
lines = f.readlines()

cmds = [l.split(' ')  for l in lines]

h = 0
v = 0
aim = 0

for (c,str) in cmds:
  n = int(str)
  if c == 'forward':
    h += n
    v += aim * n
  if c == 'down':
    aim += n
  if c == 'up':
    aim += - n

print('part2', h * v)

h = 0
v = 0

for (c,str) in cmds:
  n = int(str)
  if c == 'forward':
    h = h + n
  if c == 'down':
    v = v + n
  if c == 'up':
    v = v - n

print('part1', h * v)


