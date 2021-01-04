set.seed(42)
library(mlr3)
library(mlr3proba)
# Task
data("rats",package="survival")
TaskSurv$new(id = "rats", backend = rats, time = "time", event = "status", type = "right")
tsk("lung")
tgen("simsurv")$generate(200)

# learners
LearnerSurvGlmnet$new()$predict_types
lrn("surv.coxph")$predict_types
tsk = ("rats")
learn = lrn("surv.coxph")
learn$train(task)
learn$predict(task)
learn$model

lrn("surv.parametric",dist = "weibull", type = "ph")
lrn("surv.parametric",dist = "gaussian", type = "aft")

# measures
MeasureSurvLogloss$new()
mlr_measures$get("surv.logloss")
msr("surv.logloss")
task= tsk("rats")
pred = lrn("surv.coxph")$train(task)$predict(task)
pred$score()
pred$score(msr("surv.intlogloss"))
meas = lapply(c("surv.logloss","surv.loglossSE"), msr)
pred$score(meas)

meas1 = msr("surv.graf",times = 0:60, id = "Graf60")
meas2 = msr("surv.graf", id = "GrafAll")
pred$score(c(meas1, meas2))

meas1 = msr("surv.graf",method = 1, id = "Graf.M1")
meas2 = msr("surv.graf",method = 2, id = "Graf.M2")
pred$score(c(meas1, meas2))

# crank compositor
cox_norm = lrn("surv.coxph")
cox_comp = crankcompositor(lrn("surv.coxph"), method = "mean")
task = tsk("rats")
cox_norm$train(task)$predict(task)$crank[1:5]
cox_comp$train(task)$predict(task)$crank[1:5]

# distr compositor
glm_norm = lrn("surv.glmnet")
glm_norm$train(task)$predict(task)$distr
glm_comp = distrcompositor(lrn("surv.glmnet"), estimator = "kaplan", form = "ph")
glm_comp$train(task)$predict(task)$distr
glm_comp$graph$plot(html = TRUE)
