pos_scale = function(x) {
  sd = sd(x)
  x = as.numeric(scale(x))
  x = x + 4
  x[x < 1] = 1
  return(x)
}
gen_features = function(n = 1000){
  assert(n > 10)
  m = matrix(c(1.0, 0.5, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.5, 1.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.8, 0.8, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.8, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
               0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
               0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.4, 0.4,
               0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 1.0, 0.2,
               0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.2, 1.0
  ), nrow = 10, ncol = 10)

  data.table(
    sexF = Bernoulli$new(0.5)$rand(n),
    age = DiscreteUniform$new(lower = 20, upper = 50)$rand(n),
    trt = Bernoulli$new(0.7)$rand(n),
    data.table(MASS::mvrnorm(n, rep(0, 10), m, empirical = TRUE))
  )
}
gen_times = function(covs = gen_features(1000),
                     dist = c("weibull", "lnorm", "coxweib", "coxgom", "gomp", "llogis"),
                     sex = 1.5, age = 2, trt = 2, betas = runif(10, -1, 1)) {

  dist = match.arg(dist)
  if (dist == "weibull") {
    shape = (covs$trt + 1) * trt + (covs$sex + 1) * sex + age * covs$age
    scale =  1 + (as.matrix(covs[,4:13]) %*% (as.matrix(betas/10)))

    return(pos_scale(rweibull(nrow(covs), shape = shape, scale = scale)))

  } else if (dist == "lnorm") {

    return(pos_scale(
      rlnorm(nrow(covs),
             meanlog = log(covs$age * age + (as.matrix(covs[,4:13]) %*% (as.matrix(betas)))),
             sdlog = (covs$trt + covs$sex)/2)
    ))

  } else if (dist == "coxweib") {
    scale = (covs$trt + 3) * 3 + (covs$sex + 2) * 2 + (as.matrix(covs[,4:13]) %*% (as.matrix(betas/100)))
    shape = age * covs$age

    return(pos_scale(as.numeric((-log(runif(nrow(covs))) / (5 * exp(scale + shape)))^(1/20))))
  } else if (dist == "gomp") {
    scale = (covs$trt + 1) * trt + (covs$sex + 1) * sex
    shape = age * covs$age + (as.matrix(covs[,4:13]) %*% (as.matrix(betas/100)))

    return(pos_scale(extraDistr::rgompertz(nrow(covs), a =  scale, b = shape)))
  }
}
gen_outcome = function(covs = gen_features(1000), surv_times = gen_times(covs), cens_p = 0.2,
                       cens_type = c("I", "min", "ind"), cutoff = NULL, trt = NULL, dist) {

  cens_type = match.arg(cens_type)
  if (is.null(surv_times)) surv_times = gen_times(covs, ...)
  if (cens_type == "I") {
    cutoff = as.numeric(quantile(surv_times, probs = 1 - cens_p))
    status = as.integer(surv_times <= cutoff)
    surv_times[surv_times > cutoff] = cutoff
    return(list(times = surv_times, status = status))

  } else if (cens_type == "min") {

    cens_times = apply(cbind(gen_times(covs, dist), gen_times(covs, dist), gen_times(covs, dist),
                             gen_times(covs, dist), gen_times(covs, dist), gen_times(covs, dist)),
                       1, min)
    cens_times = ifelse(rbinom(length(surv_times), 1, 1 - cens_p), Inf, cens_times)
    times = ifelse(surv_times <= cens_times, surv_times, cens_times)
    status = ifelse(surv_times <= cens_times, 1, 0)

    return(list(times = times, status = status))
  } else {
    cens_times = apply(cbind(
      pos_scale(rweibull(nrow(covs), 10, 5)), pos_scale(rweibull(nrow(covs), 10, 5)),
      pos_scale(rweibull(nrow(covs), 10, 5)), pos_scale(rweibull(nrow(covs), 10, 5)),
      pos_scale(rweibull(nrow(covs), 10, 5)), pos_scale(rweibull(nrow(covs), 10, 5))),
                       1, min)
    cens_times = ifelse(rbinom(length(surv_times), 1, 1 - cens_p), Inf, cens_times)
    times = ifelse(surv_times <= cens_times, surv_times, cens_times)
    status = ifelse(surv_times <= cens_times, 1, 0)

    return(list(times = times, status = status))
  }
}

load_Sim1 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim2 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim3 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim4 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "min",
                        dist = "coxweib")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim5 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "min",
                        dist = "coxweib")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim6 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "min",
                        dist = "coxweib")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim7 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim8 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim9 = function(n, seed) {
  set.seed(1)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "coxweib")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}

load_Sim10 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim11 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim12 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim13 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "min",
                        dist = "weibull")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim14 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "min",
                        dist = "weibull")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim15 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "min",
                        dist = "weibull")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim16 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim17 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim18 = function(n, seed) {
  set.seed(1000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "weibull")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}

load_Sim19 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim20 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim21 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim22 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "min",
                        dist = "gomp")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim23 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "min",
                        dist = "gomp")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim24 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "min",
                        dist = "gomp")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim25 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim26 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim27 = function(n, seed) {
  set.seed(2000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "gomp")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}

load_Sim28 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim29 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim30 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "I")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim31 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "min",
                        dist = "lnorm")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim32 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "min",
                        dist = "lnorm")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim33 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "min",
                        dist = "lnorm")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim34 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.2, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim35 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.5, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
load_Sim36 = function(n, seed) {
  set.seed(3000)
  covs = gen_features(n)
  surv_times = gen_times(covs, dist = "lnorm")
  set.seed(seed)
  outcome = gen_outcome(covs = covs, surv_times = surv_times, cens_p = 0.8, cens_type = "ind")
  data.frame(covs, time = outcome$times, status = outcome$status)
}
