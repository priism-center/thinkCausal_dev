
bart.init = function(data){
  bart.showData(data);

  // store for new coordinates resulting from draggable points
  bart.functional.movedPoints = bart.data.draggablePoints
  // bart.scrollytellState1();
}

bart.loadData = function(){
  return Promise.all([
      d3.csv("data/observations.csv"),
      d3.csv("data/fits.csv"),
      d3.csv("data/credible_intervals.csv"),
      d3.csv("data/draggable_points.csv")
  ]).then(datasets => {
      bart.data = {},
      bart.data.observations = datasets[0];
      bart.data.fits = datasets[1];
      bart.data.credibleIntervals = datasets[2];
      bart.data.draggablePoints = datasets[3];
      console.log("Loaded bart data:", bart.data)
      return bart.data;
  })
}

bart.loadData().then(bart.init)
