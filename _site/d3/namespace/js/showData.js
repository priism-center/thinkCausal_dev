estimands.showData = function(data){
  //buildTable(data.ratings)
  buildPlot(data)
}

// read in the data
estimands.loadData = function() {
    return Promise.all([
        d3.csv("../scrollytell/data/point-data.csv"),
        d3.csv("../scrollytell/data/line-data.csv"),
    ]).then(datasets => {
        store = {},
        store.scatter = datasets[0];
        store.line = datasets[1];
        console.log("Loaded data:", store)
        return store;
    })
}

estimands.loadData().then(estimands.showData)
