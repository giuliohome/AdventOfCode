import collections

test = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc""".split('\n')

test = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW""".split('\n')

f = open('input.txt')
test =  f.readlines()

lines = [l.strip().split('-') for l in test]

def search(end):
    return [a for (a,b) in lines if b == end] + [b for (a,b) in lines if a == end] 


def check1(n,path):
    return n == n.upper() or not n in path

def check2(n,path):
    if n == 'end':
        return False
    if n == n.upper(): 
        return True
    else:
        testpath = path + [n]
        lowernodes = [ln for ln in testpath if ln != ln.upper()]
        freq = collections.Counter(lowernodes)
        return len([f for f in freq.values() if f>2]) == 0 and len([f for f in freq.values() if f==2]) <= 1

def solve(checkN):  
  completed = []
  paths = [['end']]
  while (paths):
      path = paths.pop()
      nxt = [n for n in search(path[-1]) if checkN(n,path)]
      for n in nxt:
          if n == 'start':
              completed.append(path + [n])
              # print(path +[n])
          else:
              paths.append(path+[n])
  print(len(completed))

solve(check1)
solve(check2)
