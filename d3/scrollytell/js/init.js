estimands.initializeData = function(data){
  //buildTable(data.ratings)
  estimands.buildPlot(data, "#estimands-plot-ATE")
  estimands.d3State1()
  estimands.plotATT(data, '#estimands-plot-ATT')
  estimands.plotATC(data, '#estimands-plot-ATC')
}

// read in the data
estimands.loadData = function() {
    return Promise.all([
        d3.csv("data/point-data.csv"),
        d3.csv("data/line-data.csv"),
    ]).then(datasets => {
        estimands.data = {},
        estimands.data.scatter = datasets[0];
        estimands.data.line = datasets[1];
        console.log("Loaded data:", estimands.data)
        return estimands.data;
    })
}

estimands.loadData().then(estimands.initializeData)
