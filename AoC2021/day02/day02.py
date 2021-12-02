

f = open('input.txt','r')
lines = f.readlines()

cmds = [l.split(' ')  for l in lines]

h = 0	
v = 0
aim = 0

for (c,str) in cmds:
  n = int(str)
  match c:
    case 'forward':
      h += n
      v += aim * n
    case 'down':
      aim += n
    case 'up':
      aim += - n

print('part2', h * v)

h = 0
v = 0

for (c,str) in cmds:
  n = int(str)
  match c:
    case 'forward':
      h = h + n
    case 'down':
      v = v + n
    case 'up':
      v = v - n

print('part1', h * v)

