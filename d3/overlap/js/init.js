
overlap.init = function(data){
  overlap.showData(data);
}

overlap.loadData = function(){
  return Promise.all([
    d3.csv("data/observations.csv"),
    d3.csv("data/fits.csv"),
    d3.csv("data/credible_intervals_ate.csv"),
    d3.csv("data/credible_intervals_y0.csv"),
    d3.csv("data/credible_intervals_y1.csv"),
    d3.csv("data/draggable_points.csv")
  ]).then(datasets => {
    overlap.data = {},
    overlap.data.observations = datasets[0];
    overlap.data.fits = datasets[1];
    overlap.data.credibleIntervals = datasets[2];
    overlap.data.credibleIntervalsY0 = datasets[3];
    overlap.data.credibleIntervalsY1 = datasets[4];
    overlap.data.draggablePoints = datasets[5];
    console.log("Loaded overlap data:", overlap.data)
    return overlap.data;
  })
}

overlap.loadData().then(overlap.init)
