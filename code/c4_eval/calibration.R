library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)
library(distr6)
library(ggplot2)
library(patchwork)
library(mlr3viz)

set.seed(080920)
train = tgen("simsurv")$generate(500)
test = tgen("simsurv")$generate(100)

k = lrn("surv.kaplan")$train(train)$predict(test)

p_cox = lrn("surv.coxph")$train(train)$predict(test)

l = ppl("distrcompositor", lrn("surv.svm", gamma.mu = 0.1))
l$train(train)
p_svm = l$predict(test)[[1]]

t = seq.int(0, 5, length.out = 100)
CPH = 1 - as.MixtureDistribution(p_cox$distr)$cdf(t)
SVM = 1 - as.MixtureDistribution(p_svm$distr)$cdf(t)
KM = 1 - k$distr[1]$cdf(t)
df = data.frame(T = t, Distribution = rep(c("CPH", "SVM", "KM"), each = 100),
                S = c(CPH, SVM, KM))

ggplot(df, aes(x = T, y = S, color = Distribution)) +
  geom_line() + theme_minimal() + ylab("S(T)") +
  theme(legend.position = "top")

dev.copy(png, "../images/c4_eval/calib_km.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

p = seq.int(0, 1, length.out = 11)
CPH = sapply(p, function(.x)
  sum(p_cox$truth[, 1L] <= as.numeric(p_cox$distr$quantile(.x)))/100)
SVM = sapply(p, function(.x)
  sum(p_svm$truth[, 1L] <= as.numeric(p_svm$distr$quantile(.x)))/100)
df = data.frame(p = p, Model = rep(c("CPH", "SVM"), each = 11),
                S = c(CPH, SVM))

ggplot(df, aes(x = p, y = S, color = Model)) +
  geom_line() + theme_minimal() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed") +
  labs(x = "True", y = "Predicted") +
  theme(legend.position = "top")

dev.copy(png, "../images/c4_eval/dcalib.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()


k$score(msr("surv.graf"))
k$score(msr("surv.calib_alpha"))
k$score(msr("surv.dcalib"))
k$score(msr("surv.dcalib", chisq = T))

p_cox$score()
p_cox$score(msr("surv.graf"))
p_cox$score(msr("surv.calib_alpha"))
p_cox$score(msr("surv.dcalib"))
p_cox$score(msr("surv.dcalib", chisq = T))

p_svm$score()
p_svm$score(msr("surv.graf"))
p_svm$score(msr("surv.calib_alpha"))
signif(p_svm$score(msr("surv.dcalib")), 3)
p_svm$score(msr("surv.dcalib", chisq = T))

pl_cox = pecs(p_cox)$data
pl_svm = pecs(p_svm)$data
d = data.frame(Time = pl_cox$time,
           Model = rep(c("CPH", "SVM"), each = 69),
           Score = c(pl_cox$graf, pl_svm$graf)
           )
ggplot(data = d, aes(x = Time, y = Score, color = Model)) +
  geom_line() + xlim(0, 4.9) +  theme_minimal() +
  theme(legend.position = "top")

dev.copy(png, "../images/c4_eval/pecs.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()
