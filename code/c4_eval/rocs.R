library(mlr3)
library(mlr3viz)
library(ggplot2)
library(patchwork)

set.seed(1)
l = lrn("classif.rpart")
l$predict_type = "prob"
t = tsk("pima")
p = l$train(t)$predict(t)
autoplot(p, "roc") + theme_minimal() + theme(legend.position = "n") +
  geom_abline(slope = 1, intercept = 0, color = "blue", alpha = 0.7) +
  labs(title = "")

dev.copy(png, "../images/c4_eval/rocs.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()
