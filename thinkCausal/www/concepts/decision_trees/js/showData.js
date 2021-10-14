function showJS(data){
  buildPlot(store.ratings)
}

// read in the data
function loadData() {
    return Promise.all([
        d3.csv("data/game_predictions.csv"),
        d3.csv("data/team_ratings.csv"),
        d3.csv("data/data.csv"),
    ]).then(datasets => {
        store = {},
        store.predictions = datasets[0];
        store.ratings = datasets[1];
        store.data = datasets[2];
        console.log("Loaded data:", store)
        return store;
    })
    // in leui of data, generate the data instead
}

// run it
loadData().then(showJS)
