library(mlr3proba)
library(paradox)
library(mlr3)
gbmScore = function(X, Y, nu = 0.01, M = 100, eta = 0.5, metric = c("graf", "logloss")) {
  checkmate::assert(nrow(X) == nrow(Y))
  checkmate::assertClass(Y, "Surv")

  metric = match.arg(metric)

  X = as.data.frame(X)
  time = Y[,colnames(Y) == "time"]
  status = Y[,colnames(Y) == "status"]

  # initialize f(x) = 0
  preds = list()
  pred = function(x, preds) {
    hp = numeric(nrow(x))
    if (length(preds)) {
      for(i in seq_along(preds)) {
        hp = hp + nu * predict(preds[[i]], newdata = x)
      }
    }
    return(hp)
  }

  unique_times = sort(unique(as.numeric(time)))
  # KM estimate
  surv = survival::survfit(Y ~ 1)$surv
  survmat = predmat = matrix(surv, ncol = length(surv), nrow = nrow(X), byrow = T)
  # KM estimate of cens distr for gradient
  cens = survival::survfit(survival::Surv(time, 1 - status) ~ 1)$surv

  lpmat = matrix(0, ncol = ncol(survmat), nrow = nrow(survmat))
  vimp = numeric(ncol(X))
  names(vimp) = colnames(X)

  for(m in seq(M)) {
    resid = c()
    for (k in seq(nrow(X))) {
      rm = 0
      for (j in seq_along(unique_times)) {

        if (time[k] > unique_times[j]) {
          if(metric == "graf") {
            rim = 2 * (1 - predmat[k, j]) * (cens[j])^-1
          } else {
            rim = (predmat[k, j] * cens[j])^-1
          }
        } else if (time[k] <= unique_times[j] & status[k] == 1) {
          if (metric == "graf") {
            rim = -2 *  predmat[k, j] * (cens[match(time[k], unique_times)])^-1
          } else {
            rim = -(predmat[k, j] * cens[match(time[k], unique_times)])^-1
          }
        } else {
          rim = NULL
        }

        if(!is.null(rim)){
          rm = rm + rim
        }
      }
      resid = c(resid, -rm/length(unique_times))
    }

    gxp = matrix(nrow = nrow(X), ncol = ncol(X))
    gxl = vector("list", ncol(X))
    for (p in seq(ncol(X))) {
      newd = data.frame(resid = resid,
                        x = X[, p])
      colnames(newd)[2] = colnames(X)[p]
      gxl[[p]] = party::ctree(as.formula(paste0("resid ~ ", colnames(X)[p])), data = newd,
                              subset = sample(nrow(X), nrow(X) * eta),
                              control = ctree_control(mincriterion = 0,
                                                      minsplit = 0,
                                                      minbucket = 0,
                                                      stump = TRUE))
      gxp[,p] = predict(gxl[[p]], newd)
    }

    mse = colSums((matrix(resid, ncol = 3, nrow = length(resid), byrow = FALSE) - gxp)^2)
    pm = which.min(mse)

    preds = c(preds, gxl[pm])

    vimp[pm] = vimp[pm] + mse[pm]

    lpmat = matrix(pred(X, preds), ncol = ncol(survmat), nrow = nrow(survmat))
    predmat = survmat ^ exp(lpmat)
  }

  output = list(fhat = pred, funs = preds, surv = survival::survfit(Y ~ 1), vimp = vimp/M)
  class(output) = "gbmScore"

  output
}
predict.gbmScore = function(object, newdata){
  nr = nrow(newdata)
  nc = length(object$surv$time)

  lp = object$fhat(newdata, object$funs)
  lpmat = matrix(lp, nrow = nr, ncol = nc)

  basehaz = distr6::WeightedDiscrete$new(data = data.frame(x = object$surv$time, cdf = 1 - object$surv$surv))

  survmat = matrix(1 - basehaz$cdf(object$surv$time), nrow = nr, ncol = nc, byrow = T)

  cdf = 1 - (survmat ^ exp(lpmat))
  x = rep(list(data = data.frame(x = object$surv$time, cdf = 0)), nr)

  for(i in seq(nr))
    x[[i]]$cdf = cdf[i,]

  distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                         decorators = c("CoreStatistics", "ExoticStatistics"))

  return(list(lp = lp, distr = distr))
}

LearnerSurvGbmScore = R6::R6Class("LearnerSurvGbmScore", inherit = LearnerSurv,
                                  public = list(
                                    initialize = function() {
                                      super$initialize(
                                        id = "surv.gbmscore",
                                        param_set = ParamSet$new(
                                          list(
                                            ParamDbl$new("nu", default = 0.01, lower = 0, upper = 1, tags = "train"),
                                            ParamInt$new("M", default = 100, lower = 1, tags = "train"),
                                            ParamDbl$new("eta", default = 0.5, lower = 0, upper = 1, tags = "train"),
                                            ParamFct$new("metric", default = "graf", levels = c("graf", "logloss"), tags = "train")
                                          )
                                        ),
                                        predict_types = c("crank", "distr", "lp"),
                                        feature_types = c("logical", "integer", "numeric", "factor")
                                      )
                                    }
                                  ),

                                  private = list(
                                    .train = function(task) {
                                      mlr3misc::invoke(gbmScore,
                                                       X = task$data(cols = task$feature_names),
                                                       Y = task$truth(),
                                                       .args = self$param_set$get_values(tags = "train"))
                                    },

                                    .predict = function(task) {
                                      p = mlr3misc::invoke(predict,
                                                           self$model,
                                                           newdata = task$data(cols = task$feature_names))

                                      PredictionSurv$new(task = task, crank = p$lp, lp = p$lp, distr = p$distr)
                                    }
                                  )
)

set.seed(1)
task = tsk('rats')
gbm = LearnerSurvGbmScore$new()
gbm$param_set$values = list(M = 100, eta = 1, metric = "logloss")
gbm$train(task)
gbm$model$vimp

task = tgen('simsurv')$generate(100)
gbm$train(task)
gbm$model$vimp


mboo = lrn("surv.mboost", baselearner = "btree")
mboo$train(task)
mboo$importance()

rsmp = rsmp("cv", folds = 3)
cox = lrn("surv.coxph")
design = benchmark_grid(task, list(gbm, cox), rsmp)
bm = benchmark(design)
bm$aggregate(msr("surv.harrellC"))
bm$aggregate(msr("surv.graf"))
bm$aggregate(msr("surv.intlogloss"))
bm$aggregate(msr("surv.logloss"))
