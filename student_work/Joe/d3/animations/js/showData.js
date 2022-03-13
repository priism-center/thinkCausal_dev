function showData(data){
  //buildTable(data.ratings)
  buildPlot(data)
}

function rnorm(n, mean, sd){
  nums = d3.range(n).map(function(){
    //num = Math.random()*1000
    num = jStat.normal.sample(mean, sd)
    //num = num.toString()
    return num //{'price': num}
  })
  return nums;
}

// function generateData(){
//   let data = []
//   let n = 250
//   data.xName = rnorm(n, 10, 4)
//   data.yName = rnorm(n, 13, 4)
//   return data;
// }

// read in the data
function loadData() {
    return Promise.all([
        d3.csv("/data/point-data.csv"),
        d3.csv("/data/line-data.csv"),
    ]).then(datasets => {
        store = {},
        store.scatter = datasets[0];
        store.line = datasets[1];
        // store.ratings = datasets[1];
        console.log("Loaded data:", store)
        return store;
    })
}

loadData().then(showData)
