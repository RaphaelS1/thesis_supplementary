library(mlr3proba); library(mlr3)

task = tgen("simsurv")$generate(20)
f = lrn("surv.flexible")$train(task)

newdata = task$data()

microbenchmark::microbenchmark(
  f$predict(task),

  summary(f$model, newdata),

  times = 5,
  control = list(warmup = 2)
)
