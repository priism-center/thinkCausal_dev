// fundamental.showData = function(data){
//   //buildTable(data.ratings)
//   fundamental.buildPlot(data)
// }

// fundamental.generateData = function(meanDraw){
//   // generate normal distribution using mean
//   let sd = meanDraw * 0.5
//   let n = 1000
//   let normalDist = []
//   for ( var i = 0; i < n; i++){
//     let randNum = jStat.normal(meanDraw, sd, 1).sample()
//     normalDist[i] = {x : randNum}
//   }
//
//   return normalDist;
// }

fundamental.generateData = function(mean){
  let sd = 50 //mean * 1.5
  let n = 1000
  distribution = d3.range(n).map(function(i){
    num = jStat.normal.sample(mean, sd)
    num = num.toString()
    return {'x': num, "index": i}
  })
  return distribution;
}

fundamental.sampleFrom = function(array){
  return array[Math.floor(Math.random() * array.length)];
}

// show plot
fundamental.showData()

//
// // read in the data
// fundamental.loadData = function() {
//     return Promise.all([
//         d3.csv("data/point-data.csv"),
//         d3.csv("data/line-data.csv")
//     ]).then(datasets => {
//         fundamental.data = {},
//         fundamental.data.scatter = datasets[0];
//         fundamental.data.line = datasets[1];
//         console.log("Loaded data:", fundamental.data)
//         return fundamental.data;
//     })
// }
//
// fundamental.loadData().then(fundamental.showData)
