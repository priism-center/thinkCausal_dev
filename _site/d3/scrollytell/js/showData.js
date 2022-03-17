function estimands_showData(data){
  //buildTable(data.ratings)
  estimands_buildPlot(data)
}

// read in the data
function estimands_loadData() {
    return Promise.all([
        d3.csv("data/point-data.csv"),
        d3.csv("data/line-data.csv"),
    ]).then(datasets => {
        store = {},
        store.scatter = datasets[0];
        store.line = datasets[1];
        console.log("Loaded data:", store)
        return store;
    })
}

estimands_loadData().then(estimands_showData)
