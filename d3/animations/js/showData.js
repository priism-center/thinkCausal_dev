function showData(data){
  //buildTable(data.ratings)
  buildPlot(data)
}

// read in the data
function loadData() {
    return Promise.all([
        d3.csv("/thinkCausal/d3/animations/data/point-data.csv"),
        d3.csv("/data/line-data.csv"),
    ]).then(datasets => {
        store = {},
        store.scatter = datasets[0];
        store.line = datasets[1];
        console.log("Loaded data:", store)
        return store;
    })
}

loadData().then(showData)
