from collections import deque

p_open = ['(','[','{','<']
p_close = [')',']','}','>']
points = [3, 57, 1197, 25137]

f = open('input.txt')
lines = [l.strip() for l in f.readlines()]


def check_line(line):
  stack = deque()
  for c in line:
    found = False
    for (n,p) in enumerate(p_close):
      if c == p:
        if not stack or stack.pop() != p_open[n]:
          return points[n]
        else:
          found = True
          break;
    if found: 
    	continue
    if c not in p_open:
      raise Exception("unexpected char: *" + c + "*")
    stack.append(c)
  return 0

print(f'part1 = {sum(check_line(l) for l in lines)}')

def completion(line):
  stack = deque()
  for c in line:
    found = False
    for (n,p) in enumerate(p_close):
      if c == p:
        if not stack or stack.pop() != p_open[n]:
          return None
        else:
          found = True
          break;
    if found: 
    	continue
    if c not in p_open:
      raise Exception("unexpected char: *" + c + "*")
    stack.append(c)
  return stack    
        
          
def score(stack):
  s = 0
  if not stack: return 0
  while (stack):
    c = stack.pop()
    if c == '(': 
      s = s*5 + 1 
    if c == '[': 
      s = s*5 + 2
    if c == '{': 
      s = s*5 + 3
    if c == '<': 
      s = s*5 + 4
  return s

scores = [score(completion(l)) for l in lines]
scores = [s for s in scores if s>0]
scores.sort()
print(f'part2 = {scores[len(scores)//2]}')   





















