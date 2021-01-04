library(survival); library(ggplot2); library(gridExtra)

ibs_rel = function(p, n){
  ibs = c()
  for(i in seq.int(10, 10000, length.out = n)){
    time = round(runif(i, 1, 20))
    data = data.frame(time = time, event = rbinom(i,1,p))
    c = suppressWarnings(suppressMessages(pec::pec(survfit(Surv(time = time, event = event) ~ 1, data = data),
                                                   data = data)))
    ibs = c(ibs, mean(c$AppErr$survfit))
  }
  return(ibs)
}

ibs_vec = c()
for(i in seq.int(0.1,0.9,0.1)){
  ibs_vec = c(ibs_vec, ibs_rel(p = i, n = 100))
}

data = data.frame(y = ibs_vec,
                  x = seq.int(10, 10000, length.out = 100),
                  group = as.character(rep(seq.int(0.1,0.9,0.1), each = 100)))

ggplot(data, aes(x=x,y=y,group=group,fill=group,colour=group)) + geom_line() +
  labs(y= "Integrated Brier Score", x = "Number of Observations",
       colour = "Death%") +
  theme_minimal()

