#------------------------------------------
# Comparing integrated brier score - Set-up
#------------------------------------------
library(mlr3); library(dplyr); library(survival)
set.seed(42)
task = tsk("rats")

# Train model
train = sample(task$nrow, 2/3 * task$nrow)
data_train = task$data(rows = train)
learn = lrn("surv.coxph")$train(task, row_ids = train)

# Make predictions
test = setdiff(1:task$nrow, train)
data_test = task$data(rows = test)
pred = learn$predict(task, row_ids = test)

# Extract true values, unique survival times, and probability matrix
truth = task$truth(rows = test)
times = sort(unique(truth[,1]))
surv_probs = 1 - pred$distr$cdf(times)

#----------------
# pec::pec - 0.03
#----------------
library(pec)
pec(t(as.matrix(surv_probs)), Surv(time, status)~litter+rx+sex, data = data_test, start = min(times),
         maxtime = max(times), cens.model = "marginal")

#-------------------
# dynpred::pe - 0.03
#-------------------
# Compute censoring distribution with Kaplan-Meier- identical method to surv.graf
cens_dist = survival::survfit(Surv(time, event) ~ 1, data = data.frame(time = truth[,1], event = 1-truth[,2]))
cens_dist = distr6::WeightedDiscrete$new(x = cens_dist$time, cdf = 1 - cens_dist$surv,
                                         decorators = "ExoticStatistics")
cens_probs = 1 - cens_dist$cdf(times)
dynpred::pe(time = truth[,1], status = truth[,2],
            tsurv = truth[,1], survmat = t(as.matrix(surv_probs)),
            tcens = truth[,1], censmat = matrix(cens_probs, nrow = length(truth[,1]), ncol = length(times)),
            FUN = "Brier", tout = times)

dynpred::pe(time = truth[,1], status = truth[,2],
            tsurv = truth[,1], survmat = t(as.matrix(surv_probs)),
            tcens = truth[,1], censmat = matrix(cens_probs, nrow = length(truth[,1]), ncol = length(times)),
            FUN = "KL", tout = times)

#---------------------
# ipred::sbrier - 0.03
#---------------------
ipred::sbrier(truth, as.matrix(surv_probs), times)

#----------------------------------
# mlr3proba::MeasureSurvGraf - 0.03
#----------------------------------
pred$score(msr("surv.graf"))
pred$score(msr("surv.intlogloss"))

#----------------------------------
# survAUC::predErr - 7.9
#----------------------------------
survAUC::predErr(task$truth(rows = train), task$truth(rows = test), learn$model$linear.predictors,
                 pred$lp, times, "brier", "weighted")$ierror

