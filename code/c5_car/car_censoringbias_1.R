set.seed(061120)

simul = function(n){

  diff = round(runif(n, 1, 30))
  time = round(runif(n, 1, 30))

  sapply(seq.int(5,25,5), function(x) {
    s1 = sum(time >= x)/n
    s2 = sum(time >= x)/(sum(diff >= x))
    s2 - s1
  })
}

x = rowMeans(replicate(100, simul(100000)))
names(x) = seq.int(5,25,5)
x
