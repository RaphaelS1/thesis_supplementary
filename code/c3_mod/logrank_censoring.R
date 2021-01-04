library(survival)
library(ggplot2)

simul_indep = function(n){
  data = data.frame(group = integer(n),
                    time = rexp(n, rate = 0.5), event = sample(0:1,n,T))
  chis = c()
  cens = c()
  for(i in 1:(nrow(data)-1)){
    data[i,1] = 1
    chis = c(chis, survdiff(Surv(time, event) ~ group, data = data)$chisq)
    cens = c(cens, 1 - sum(data[1:i, 3])/sum(data[1:i, 1]))
  }

  return(cens[which.max(chis)])
}

simul_dep = function(n){
  data = data.frame(group = integer(n),
                    time = rexp(n, rate = 0.5),
                    event = round(runif(n)))
  chis = c()
  cens = c()
  for(i in 1:(nrow(data)-1)){
    data[i,1] = 1
    data[i,3] = data[i,1] * data[i,3]
    chis = c(chis, survdiff(Surv(time, event) ~ group, data = data)$chisq)
    cens = c(cens, 1 - sum(data[1:i, 3])/sum(data[1:i, 1]))
  }

  return(cens[which.max(chis)])
}

set.seed(1)
dep_cens = replicate(1000, simul_dep(100))
indep_cens = replicate(1000, simul_indep(100))
data = data.frame(group = rep(c("Dependent","Independent"), each = 1000),
     value = c(dep_cens, indep_cens))

ggplot(data, aes(y = value, x = group, fill = group)) +
  geom_violin() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  labs(y = "Proportion of Censoring")

summary(dep_cens)
summary(indep_cens)


ci = function(p, N, z){
  l = p - z * sqrt(p*(1-p)/N)
  u = p + z * sqrt(p*(1-p)/N)
  paste0(round(p,2), " (", round(l,2), " to ", round(u,2), ")")
}
ci(sum(dep_cens >= 0.75)/1000, 1000, 1.96)
ci(sum(indep_cens >= 0.75)/1000, 1000, 1.96)
