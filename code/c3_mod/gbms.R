library(mlr3proba)
set.seed(310820)

learns = list()
learns$gdcox = lrn("surv.gbm", id = "ml_gbm_gdcox", n.trees = 100, interaction.depth = 1,
                   shrinkage = 0.1)

learns$blackboost_cox = lrn("surv.blackboost", id = "ml_gbm_blackboost_coxph",
                family = "coxph", mstop = 100, nu = 0.1)

learns$gamboost = lrn("surv.mboost", id = "ml_gbm_blackboost_coxph",
                            family = "coxph", mstop = 100, baselearner = "btree")

learns$xgb = lrn("surv.xgboost", id = "ml_gbm_xgb", nrounds = 100, eta = 0.1, max_depth = 1,
                 subsample = 1)

task = tgen("simsurv")$generate(1000)
design = benchmark_grid(task, learns, rsmp("cv", folds = 10))
bm = benchmark(design)

bm$aggregate()

df = data.frame(Pkg = "gbm (0.51)", lp = bm$data$prediction[[1]]$test$lp)
df = rbind(df, data.frame(Pkg = "blackboost (0.51)", lp = bm$data$prediction[[11]]$test$lp))
df = rbind(df, data.frame(Pkg = "mboost (0.50)", lp = bm$data$prediction[[21]]$test$lp))
df = rbind(df, data.frame(Pkg = "xgboost (0.49)", lp = bm$data$prediction[[31]]$test$lp))
df$x = rep(1:100, 4)

library(ggplot2)
ggplot(df, aes(x = x, y = lp, group = Pkg, color = Pkg)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "top", legend.box = "horizontal",
        legend.title = element_blank())
