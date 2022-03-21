
estimands.abs = function(x){
  return Math.abs(x)
}

estimands.mult = function(x){
  return Math.abs(x * 10)
}

estimands.myVar = 90;

m = function(){
  estimands.myVar = estimands.myVar * 2;
  console.log(estimands.myVar)
}
