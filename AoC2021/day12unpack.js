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


const check1 = (n,path,dupl) => [n === n.toUpperCase() || !path.includes(n), false]

const check2 = (n,path,dupl) => {
  if ( n === 'end' ) { 
    return [false,dupl];
  } 
  if ( n === n.toUpperCase() || !path.includes(n) ) {
    return [true, dupl];
  }
  if (dupl) {
    return [false, true];
  } else {
    return [true, true];
  }
}

const logic = async ( data ) => {
  var startDate = new Date();
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
  var endDate   = new Date();
  var seconds = (endDate.getTime() - startDate.getTime()) / 1000;
  console.log('seconds elapsed %f', seconds);
}

const solve = (checkN) => {
  completed = [];
  paths = [  [['end'], false]  ];
  while (paths.length) {
    [path,dupl] = paths.pop();
    search = path[path.length-1]
    // console.log(path,dupl,search,lmap);
    nxt = lmap.get(search) ;
    for (n of nxt) {
      if ( n === 'start') {
	completed.push( [path.concat(n), dupl] );
      } else {
	[ok,dupl2] = checkN(n,path,dupl);
	if ( ok ) paths.push( [path.concat(n),dupl2] ); 
      }
    }
  }
  console.log(completed.length);
}

