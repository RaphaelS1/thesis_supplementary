library(mlr3)
library(mlr3learners)

MeasureClassifBrier_SE = R6::R6Class("brier_se", inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function(...) {
      super$initialize(
        id = "brier.se",
        range = c(0, Inf)
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      N <- length(prediction$row_ids)
      y <- as.numeric(prediction$truth == 1)
      p <- (y - prediction$prob[,2])^2
      return(sd(p)/sqrt(N))
    }
  ))

set.seed(051120)

data = list(Sim1 = load_Sim1(n = 1000, seed = 1), Sim7 = load_Sim7(n = 1000, seed = 1))
data$Sim1$status = factor(1 - data$Sim1$status)
data$Sim7$status = factor(1 - data$Sim7$status)
data$Sim1$time = NULL
data$Sim7$time = NULL
tasks = list(TaskClassif$new("Sim1", data$Sim1, "status"),
             TaskClassif$new("Sim7", data$Sim7, "status"))
lrns = lrns(c("classif.featureless", "classif.log_reg"), predict_type = "prob")
design = benchmark_grid(tasks, lrns, rsmp("cv", folds = 5))
bm = benchmark(design)
score = bm$aggregate(list(msr("classif.bbrier"), MeasureClassifBrier_SE$new()))[,c(3,4,7,8)]
score$brier =  paste0(round(score$classif.bbrier, 2), "(",
       round(score$classif.bbrier, 2) - round(1.96 * score$brier.se, 2), ", ",
       round(score$classif.bbrier, 2) + round(1.96 * score$brier.se, 2), ")")
score[,c(1,2,5)]
