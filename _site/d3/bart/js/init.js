
bart.init = function(data){
  bart.showData(data);

  // store for new coordinates resulting from draggable points
  bart.functional.movedPoints = bart.data.draggablePoints

  // activate buttons in functional form
  bart.functional.showModel('none')
  
  // update slider labels
  bart.overlap.updateSliderLabel();
  // bart.scrollytellState1();
}

bart.loadData = function(){
  return Promise.all([
      d3.csv("data/observations.csv"),
      d3.csv("data/fits.csv"),
      d3.csv("data/credible_intervals_ate.csv"),
      d3.csv("data/credible_intervals_y0.csv"),
      d3.csv("data/credible_intervals_y1.csv"),
      d3.csv("data/draggable_points.csv")
  ]).then(datasets => {
      bart.data = {},
      bart.data.observations = datasets[0];
      bart.data.fits = datasets[1];
      bart.data.credibleIntervals = datasets[2];
      bart.data.credibleIntervalsY0 = datasets[3];
      bart.data.credibleIntervalsY1 = datasets[4];
      bart.data.draggablePoints = datasets[5];
      console.log("Loaded bart data:", bart.data)
      return bart.data;
  })
}

bart.loadData().then(bart.init)
