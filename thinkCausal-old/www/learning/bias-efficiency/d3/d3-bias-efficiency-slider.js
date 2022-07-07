// bins
// var slider_bins = document.getElementById("input-bins");
// var output_bins = document.getElementById("output-bins");
// output_bins.innerHTML = slider_bins.value;
//
// slider_bins.oninput = function() {
//   output_bins.innerHTML = this.value;
// }

// mean
var slider_mean = document.getElementById("input-mean");
var output_mean = document.getElementById("output-mean");
output_mean.innerHTML = slider_mean.value;

slider_mean.oninput = function() {
  output_mean.innerHTML = this.value;
}

// sd
var slider_sd = document.getElementById("input-sd");
var output_sd = document.getElementById("output-sd");
output_sd.innerHTML = slider_sd.value;

slider_sd.oninput = function() {
  output_sd.innerHTML = this.value;
}

// n datapoints
var slider_n_data = document.getElementById("input-n-data");
var output_n_data = document.getElementById("output-n-data");
output_n_data.innerHTML = slider_n_data.value;

slider_n_data.oninput = function() {
  output_n_data.innerHTML = this.value;
}
