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

lmap = {}
def lmap12(x,y):
    if x in lmap:
        lmap[x] += [y]
    else:
        lmap[x] = [y]

for (a,b) in lines:
    lmap12(a,b)
    lmap12(b,a)

def search(end):
    return lmap[end]

def check1(n, path, dupl):
    return n == n.upper() or not n in path, False

def check2(n, path, dupl):
    if n == 'end':
        return False, dupl
    if n == n.upper() or not n in path: 
        return True, dupl
    if dupl:
        return False, True
    else:
        # path.insert(0,'++')
        return True, True

def solve(checkN):  
  completed = []
  paths = [(['end'], False)]
  while (paths):
      path, dupl = paths.pop()
      for n in search(path[-1]): 
          if n == 'start':
              completed.append(path + [n])
              continue
          ok, dupl2 = checkN(n, path, dupl)
          if ok:
              paths.append( (path + [n], dupl2) )
  print(len(completed))

solve(check1)
# 5252
solve(check2)
# 147784

