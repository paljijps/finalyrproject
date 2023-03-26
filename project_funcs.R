pinc = function(x){
  pic = c()
  for (i in 2:length(x)) {
    pic[i] = x[i]/x[i-1] - 1
  }
  return(100*pic)
}