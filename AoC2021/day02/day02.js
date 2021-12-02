
// Include fs module
const fs = require('fs');

// Calling the readFileSync() method
// to read 'input.txt' filedata
const data = fs.readFileSync('./input.txt',
			{encoding:'utf8', flag:'r'});

// Display the file data
// console.log(data);
const cmds = data.split('\n').filter(x => x.length > 0).map(l => l.split(' '));


// loop through the data
const [hh,vv] = cmds.reduce((prev,curr,idx,arr) => {
	// match the line with possible commands
	const [h, v] = prev;
	const [cmd, str] = curr;
	const n = Number(str);
	switch (cmd){
	  case 'forward':
	  	return [h+n,v];
	  case 'up':
	  	return [h,v-n];
	  case 'down':
	  	return [h,v+n];
	    return [h,v];
	}}, [0, 0]);

console.log('part 1 %d', vv * hh);

const [hh_2,vv_2, aim_2] = cmds.reduce((prev,curr,idx,arr) => {
	// match the line with possible commands
	const [h, v, aim] = prev;
	const [cmd, str] = curr;
	const n = Number(str);
	switch (cmd){
	  case 'forward':
	  	return [h+n,v+(aim*n),aim];
	  case 'up':
	  	return [h,v,aim-n];
	  case 'down':
	  	return [h,v,aim+n];
	    return [h,v];
	}}, [0, 0, 0]);

console.log('part 2 %d', vv_2 * hh_2);
