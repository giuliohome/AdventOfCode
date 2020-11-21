function aocInput() {
    // $.get(
        // "https://adventofcode.com/2019/day/1/input",{},
        // function(data) {
           // document.getelementbyid("inputtext").value = data;
        // }
    // );

  const proxyurl = "https://cors-anywhere.herokuapp.com/";
  fetch('https://adventofcode.com/2019/day/1/input', {
      //mode: 'no-cors',
      credentials: 'include',
      method: 'GET',
      headers: {
        'Content-Type': 'text/html',
        'Accept': 'text/html',
        'Access-Control-Allow-Origin': 'http://localhost:8080',
        'Access-Control-Allow-Credentials': 'true'
      }
    })
    .then(function (response) { response.json()})
    .then(function (json) { console.log(json)})
    .catch(function(error) { console.log('Authorization failed : ' + error.message)});
    
  // $.ajax({
       // type: 'GET',
       // // beforeSend: function(xhr){
        // // xhr.setRequestHeader('Access-Control-Allow-Origin', '*');
        // // xhr.setRequestHeader('User-Agent','Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'); 
        // // xhr.setRequestHeader('Cookie','_ga=GA1.2.829129693.1605974453; _gid=GA1.2.1215865863.1605974453; session=53616c7465645f5f770e011bc6984a015bb199f28abeb7249b2319f445213e4f1aae7d26abff9b949e43a9bd76d168ea');  
        // // },
       // headers: {
            // 'Access-Control-Allow-Origin': 'http://localhost:8080',
            // 'Access-Control-Allow-Credentials' : true,
            // 'User-Agent':'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko',
            // 'Cookie':'_ga=GA1.2.829129693.1605974453; _gid=GA1.2.1215865863.1605974453; session=53616c7465645f5f770e011bc6984a015bb199f28abeb7249b2319f445213e4f1aae7d26abff9b949e43a9bd76d168ea'
       // },
       // // dataType: 'html',
       // //dataType: 'jsonp',
       // url:  'https://adventofcode.com/2019/day/1/input',
       // cache: false,
        // // NO setCookies option available, set cookie to document
        // //setCookies: "lkfh89asdhjahska7al446dfg5kgfbfgdhfdbfgcvbcbc dfskljvdfhpl",
        // crossDomain: true,
       // success: function(data){
            // document.getElementById("inputText").value = data;
       // }
    // });
}