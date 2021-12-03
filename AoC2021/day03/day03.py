f = open('input.txt','r')
lines = [l.strip() for l in f.readlines()]

llen = len(lines[0].strip())

print(f'llen {llen}')

def most_frequent(List):
    return max(set(List), key = List.count)

def least_frequent(List):
    return min(set(List), key = List.count)


gamma = [most_frequent([l[i] for l in lines]) for i in range(0,llen) ]

epsilon = [least_frequent([l[i] for l in lines]) for i in range(0,llen) ]


g = int("".join(gamma),2)

e = int("".join(epsilon),2)


print(f'part 1: {g * e}' )


def matching(i, numi): 
  return [line for line in lines 
    if line[0:i+1].strip() == "".join(map(str,numi[0:i+1]))]

def countofdigit(i,c, numi):
  sellines = matching(i - 1, numi) if i >= 1 else lines
  return len([l[i] for l in sellines if l[i] == c])

def buildnum1():
  num = []
  for i in range(0,llen):
    num += [1 if countofdigit(i,'1',num) >= countofdigit(i,'0',num) else 0]
  return num

def maskbit(i,num):
  match(countofdigit(i,'0',num) > 0,countofdigit(i,'1',num) > 0):
  	case (True,False): 
  	 return 0
  	case (False,True): 
  	 return 1
  	case (True,True): 
  	  return 0 if countofdigit(i,'0',num) <= countofdigit(i,'1',num) else 1

def buildnum2():
  num = []
  for i in range(0,llen):
    num += [maskbit(i,num)]
  return num


num1 = buildnum1()
 
num2 = buildnum2()

def findmatch(numi):
  for i in range(0,llen):
    if len(matching(i,numi)) == 1:
      return matching(i,numi)




x1n = int("".join(list(map(str,num1))),2)

x2n = int("".join(list(map(str,num2))),2)

print(f'num1 {num1} {x1n}')

print(f'num2 {num2} {x2n}')

x1 = int(findmatch(num1)[0].strip(),2)

x2 = int(findmatch(num2)[0].strip(),2)

print(f'x1 {findmatch(num1)[0]} {x1}')

print(f'x2 {findmatch(num2)[0]} {x2}')

print(f'part 2: {x1 * x2} ==  {x1n * x2n} ' )
