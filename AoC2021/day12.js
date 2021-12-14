const fs = require('fs')

console.log('input file:\n')
lines = []

fs.readFile('./input.txt', 'utf8' , async (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  await logic( data );
})

lmap = new Map()
const lmap12 = (x,y) => {
  if (lmap.has(x)) {
    lmap.set(x, lmap.get(x).concat(y) ) 
  } else {
    lmap.set(x, [y])
  }
}


const check1 = (n,path) => n === n.toUpperCase() || !path.includes(n)

const check2 = (n,path) => {
  if ( n === 'end' ) { 
    return false;
  } 
  if ( check1(n,path) ) {
    return true;
  }
  if (path[0] == '++') {
    return false;
  } else {
    path.unshift('++');
    return true;
  }
}

const logic = async ( data ) => {
  console.log('start of logic\n');
  data.split(/\r?\n/).forEach(line =>  {
    if (line) lines.push ( line.split( '-' ) );  // .unshift ( line );

  });
  for (x of lines) {
    lmap12(x[0],x[1]);
    lmap12(x[1],x[0]);
  }
  console.log('solve part 1');
  solve(check1);
  console.log('solve part 2');
  solve(check2);
}

const solve = (checkN) => {
  completed = [];
  paths = [['end']];
  while (paths.length) {
    path = paths.pop();
    search = path.slice(-1)[0]
    nxt = lmap.get(search);
    for (n of nxt) {
      if ( n === 'start') {
	completed.push( path.concat( n ) );
      } else {
	cpath = path.slice();
	if ( checkN(n,cpath) ) paths.push( cpath.concat( n ) ); 
      }
    }
  }
  console.log(completed.length);
}

