library(mboost); library(mlr3proba)

Graf = function(){
  ngradient = function(y, f, w = 1) {
    time = y[,1]
    status = y[,2]
    unique_times = sort(unique(as.numeric(time)))
    surv = survival::survfit(y ~ 1)$surv
    survmat = matrix(surv, ncol = length(surv), nrow = nrow(y), byrow = T)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv
    lpmat = matrix(f, ncol = ncol(survmat), nrow = nrow(survmat))
    predmat = survmat ^ exp(lpmat)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv

    resid = c()
    for (k in seq(nrow(y))) {
      rim = 0
      for (j in seq_along(unique_times)) {
        if (time[k] > unique_times[j]) {
          rim = rim + (2 * (1 - predmat[k, j]) * (cens[j])^-1)
        } else if (time[k] <= unique_times[j] & status[k] == 1) {
          rim = rim + (-2 *  predmat[k, j] * (cens[match(time[k], unique_times)])^-1)
        }
      }
      resid = c(resid, -rim/length(unique_times))
    }

    return(resid)
  }

  loss = function(y, f) {
    time = y[,1]
    status = y[,2]
    unique_times = sort(unique(as.numeric(time)))
    surv = survival::survfit(y ~ 1)$surv
    survmat = matrix(surv, ncol = length(surv), nrow = nrow(y), byrow = T)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv
    lpmat = matrix(f, ncol = ncol(survmat), nrow = nrow(survmat))
    predmat = survmat ^ exp(lpmat)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv

    loss = c()
    for (k in seq(nrow(y))) {
      rim = 0
      for (j in seq_along(unique_times)) {
        if (time[k] > unique_times[j]) {
          rim = rim + (((1 - predmat[k, j])^2) * ((cens[j])^-1))
        } else if (time[k] <= unique_times[j] & status[k] == 1) {
          rim = rim + ((predmat[k, j]^2) * (cens[match(time[k], unique_times)])^-1)
        }
      }
      loss = c(loss, rim/length(unique_times))
    }

    return(loss)
  }

  risk = function(y, f, w = 1) {
    mean(loss(y, f), na.rm = TRUE)
  }

  mboost::Family(
    ngradient = ngradient,
    loss = loss,
    risk = risk,
    offset = function(y, w = 1) 0,
    name = "Integrated Graf Score"
  )
}
Logloss = function(){
  ngradient = function(y, f, w = 1) {
    time = y[,1]
    status = y[,2]
    unique_times = sort(unique(as.numeric(time)))
    surv = survival::survfit(y ~ 1)$surv
    survmat = matrix(surv, ncol = length(surv), nrow = nrow(y), byrow = T)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv
    lpmat = matrix(f, ncol = ncol(survmat), nrow = nrow(survmat))
    predmat = survmat ^ exp(lpmat)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv

    resid = c()
    for (k in seq(nrow(y))) {
      rim = 0
      for (j in seq_along(unique_times)) {
        if (time[k] > unique_times[j]) {
          rim = rim + ((predmat[k, j] * cens[j])^-1)
        } else if (time[k] <= unique_times[j] & status[k] == 1) {
          rim = rim + (-(predmat[k, j] * cens[match(time[k], unique_times)])^-1)
        }
      }
      resid = c(resid, -rim/length(unique_times))
    }

    return(resid)
  }

  loss = function(y, f) {
    time = y[,1]
    status = y[,2]
    unique_times = sort(unique(as.numeric(time)))
    surv = survival::survfit(y ~ 1)$surv
    survmat = matrix(surv, ncol = length(surv), nrow = nrow(y), byrow = T)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv
    lpmat = matrix(f, ncol = ncol(survmat), nrow = nrow(survmat))
    predmat = survmat ^ exp(lpmat)
    cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv

    loss = c()
    for (k in seq(nrow(y))) {
      rim = 0
      for (j in seq_along(unique_times)) {
        if (time[k] > unique_times[j]) {
          rim = rim - (log(predmat[k, j])/cens[j])
        } else if (time[k] <= unique_times[j] & status[k] == 1) {
          rim = rim - (log(1 - predmat[k, j])/cens[match(time[k], unique_times)])
        }
      }
      loss = c(loss, rim/length(unique_times))
    }

    return(loss)
  }

  risk = function(y, f, w = 1) {
    mean(loss(y, f), na.rm = TRUE)
  }

  mboost::Family(
    ngradient = ngradient,
    loss = loss,
    risk = risk,
    offset = function(y, w = 1) 0,
    name = "Integrated Log-Loss"
  )
}

task = tsk("rats")
learn_cox = lrn("surv.mboost", family = "coxph", baselearner = "btree", id = "coxboost")
learn_graf = lrn("surv.mboost", family = "custom", custom.family = Graf(),
                 baselearner = "btree", id = "grafboost")
learn_logloss = lrn("surv.mboost", family = "custom", custom.family = Logloss(),
                    baselearner = "btree", id = "logboost")
rsmp = rsmp("cv", folds = 3)
design = benchmark_grid(task, list(learn_cox, learn_graf, learn_logloss, lrn("surv.coxph")), rsmp)
bm = benchmark(design)
bm$aggregate(msr("surv.harrellC"))
bm$aggregate(msr("surv.graf"))
bm$aggregate(msr("surv.intlogloss"))
