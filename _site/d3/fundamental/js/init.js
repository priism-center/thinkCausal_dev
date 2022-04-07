
fundamental.init = function(){
  fundamental.showData()
  fundamental.scrollytellState1()
}
fundamental.init()


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
