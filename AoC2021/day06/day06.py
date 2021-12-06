f = open('input.txt','r')
lines = [l.strip() for l in f.readlines()]

llen = len(lines[0].strip())

print(f'llen {llen}')

inputs = [int(i) for i in  lines[0].split(',')]

def round(inputs):
  nexti = []
  for i in inputs:
    if i == 0:
      nexti += [6,8]
    else:
      nexti += [i-1]
  return nexti



nexti = inputs
for r in range(0,80):
  nexti = round(nexti)


lnext = len(nexti)
# 374994

print(f'part 1: {lnext}' )

nexti = inputs

res = {}
def init_res(res): 
  for n in range(0,8+1):
    res['i'+str(n)] = 0
init_res(res)

for n in nexti:
  res['i'+str(n)] += 1


def round2(res):
  nres = {}
  init_res(nres)
  for i in range(0,8+1):
    if i == 0:
      nres['i6'] += res['i0']
      nres['i8'] += res['i0'] 
    else:
      nres['i'+str(i-1)] += res['i'+str(i)]
  return nres

nres = res
for r in range(0,256):
  nres = round2(nres)


l2next = sum(nres.values())

print(f'part 2: {l2next} ' )
# 1686252324092

