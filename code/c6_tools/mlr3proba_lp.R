library(mboost)
library(survival)
lung$wt.loss = NULL

set.seed(210120)
train = sample(228, 152)
test = setdiff(seq(228), train)

mboost_conc = function(family) {
  fit = blackboost(Surv(time, status) ~ ., data = lung[train,], family = family)
  pred = as.numeric(predict(fit, lung[test, ], "link"))
  concordance(Surv(lung$time, lung$status)[test] ~ pred)$concordance
}

df = c(mboost_conc(mboost::CoxPH()),
       mboost_conc(mboost::Weibull()),
       mboost_conc(mboost::Loglog()),
       mboost_conc(mboost::Lognormal()),
       mboost_conc(mboost::Gehan()),
       mboost_conc(mboost::Cindex())
)
names(df) = c("CoxPH", "Weibull", "Loglog", "Lognormal", "Gehan", "Cindex")

fit = survreg(Surv(time, status) ~ ., data = lung[train, ], dist = "weibull")
pred = as.numeric(predict(fit, lung[test, ], "link"))
survreg = concordance(Surv(lung$time, lung$status)[test] ~ pred)$concordance

fit = coxph(Surv(time, status) ~ ., data = lung[train, ])
pred = as.numeric(predict(fit, lung[test, ], "lp"))
cph = concordance(Surv(lung$time, lung$status)[test] ~ pred)$concordance

df2 = c(cph, survreg)
names(df2) = c("coxph", "survreg")

df
df2
