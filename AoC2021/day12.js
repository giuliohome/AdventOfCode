const fs = require('fs')

console.log('input file:\n')
lines = []

fs.readFile('./input.txt', 'utf8' , async (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  console.log(data);
  await logic( data );
})

const search = ( end ) => {
  return lines.filter(x => x[0] == end).map( x => x[1] )
    .concat( 
      lines.filter(x => x[1] == end).map( x => x[0] ) 
    );
}

const check1 = (n,path) => n === n.toUpperCase() || !path.includes(n)

/*const check2classic = (n,path) => {
  if ( n === 'end' ) { 
    return false;
  } 
  if ( n === n.toUpperCase() ) {
    return true;
  } else {
    testpath = path.concat( n );
    lowernodes = [...(new Set(testpath.filter( ln => ln != ln.toUpperCase() )) )];
    freq = lowernodes.map(x => { 
      return { 
	val: x, count: testpath.filter(f => f === x).length 
      };
    } );
    // console.log('freq %O', freq);
    return freq.filter(x => x.count > 2).length == 0 && freq.filter(x => x.count == 2).length <=1 ;
  } 
}*/

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
    // console.log(`Line from file: ${line}`);
    if (line) lines.push ( line.split( '-' ) );  // .unshift ( line );

  });
  console.log('lines %O', lines );
  x = search('end');
  console.log('show end %O', x);
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
    // console.log('slice %O', search(path.slice(-1)));
    nxt = search(path.slice(-1)); //.filter( n => checkN(n,path) );
    // console.log('nxt %O', nxt);
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

