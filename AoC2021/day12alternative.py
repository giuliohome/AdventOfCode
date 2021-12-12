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
    if n == n.upper() or not n in path: 
        return True
    if path[0] == '++':
        return False
    else:
        path.insert(0,'++')
        return True

def solve(checkN):  
  completed = []
  paths = [['end']]
  while (paths):
      path = paths.pop()
      for n in search(path[-1]): 
          if n == 'start':
              completed.append(path + [n])
              # print(path +[n])
              continue
          cpath = path.copy()
          if checkN(n,cpath):
              paths.append(cpath+[n])
  print(len(completed))

solve(check1)
solve(check2)
