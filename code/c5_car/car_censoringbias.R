set.seed(42)
library(distr6)

simul = function(n, pd, pc){
  D = Bernoulli$new(prob = pd)$rand(n)
  C = Bernoulli$new(prob = pc)$rand(n)

  dead = D == 1
  alive = D == 0
  censored = C == 1
  uncensored = C == 0

  num = sum(alive & uncensored)
  S1_den = sum(alive & uncensored) + sum(dead & censored) + sum(dead & uncensored)
  S2_den = sum(alive & uncensored) + sum(dead & uncensored)

  S1_prop = num/S1_den
  S2_prop = num/S2_den

  cat(S2_prop - S1_prop,"\n")
}

simul(100000, 0.3, 0.3)
simul(100000, 0.3, 0.6)
simul(100000, 0.3, 0.9)

simul(100000, 0.6, 0.3)
simul(100000, 0.6, 0.6)
simul(100000, 0.6, 0.9)

simul(100000, 0.9, 0.3)
simul(100000, 0.9, 0.6)
simul(100000, 0.9, 0.9)
