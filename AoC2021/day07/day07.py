# test = "16,1,2,0,4,2,7,1,2,14"
f = open('input.txt')
test = f.readlines()[0].strip()
inputs = [int(i) for i in test.split(',')]
min_s = -1
min_i = -1
for i in inputs:
   s = 0
   for j in inputs:
     s += abs(j-i)   
   if min_s < 0 or s < min_s:
     min_s = s
     min_i = i

print(f'part 1: {min_i} costs {min_s}')


# test = "16,1,2,0,4,2,7,1,2,14"
f = open('input.txt')
test = f.readlines()[0].strip()
inputs = [int(i) for i in test.split(',')]
min_s = -1
min_i = -1
for i in range(min(inputs),max(inputs)+1):
   s = 0
   for j in inputs:
     diff_abs = abs(j-i) 
     s += diff_abs * (diff_abs + 1) / 2  
   # print(f'part 2: {i} costs {s}') 
   if min_s < 0 or s < min_s:
     min_s = s
     min_i = i

print(f'part 2: {min_i} costs {min_s}')

# part 2: 464 costs 98363777.0

