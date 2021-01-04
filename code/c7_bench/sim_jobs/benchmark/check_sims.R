library(survival)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(gridExtra)
source("c7_bench/sim_jobs/gen_simulated_data.R")
test_sim = function(i) {

  data = do.call(paste0("load_Sim", i), list(n = 2000, seed = i * 3000))

  ret = list()

  if (sum(coefficients(summary(lm(time ~ .-status, data = data[data$status == 1,])))[,4] < 0.05) >= 3)
    ret$lm_surv_time = "Survival time CAN be predicted"
  else
    ret$lm_surv_time = "!!Survival time canNOT be predicted!!"

  if (sum(coefficients(summary(lm(time ~ .-status, data = data[data$status == 0,])))[,4] < 0.05) >= 3)
    ret$lm_cens_time = "Censoring time CAN be predicted"
  else
    ret$lm_cens_time = "!!Censoring time canNOT be predicted!!"

  ret$cens_prop = paste0(as.numeric(proportions(table(data$status))[1])*100, "%")

  if (cox.zph(coxph(Surv(time, status) ~ ., data = data))$table[14,3] > 0.05)
    ret$ph = "Data is PH"
  else
    ret$ph = "Data is NOT PH"

  if (length(unique(data$time[data$status == 0])) == 1)
    ret$typei = "Type I Censoring"
  else
    ret$typei = "NOT Type I Censoring"

  # if (sum(coefficients(summary(glm(status ~ .-time, data = data, family = "binomial")))[,4] < 0.05) >= 3)
  #   ret$glm_status = "Censoring is informative"
  # else
  #   ret$glm_status = "Censoring is NOT informative"
  #
  # if (visual) {
  #   graphics::par(ask = ask)
  #   plot(sort(data$time))
  #
  # }

  return(ret)
}

vis = function(data) {
  data$sexF = factor(data$sexF)
  data$trt = factor(data$trt)
  data$status = factor(data$status)
  ret = list()
  p = ggplot(data)
  ret$sex = p + geom_bar(aes(y = (..count..)/sum(..count..), x = sexF, fill = sexF))
  ret$trt = p + geom_bar(aes(y = (..count..)/sum(..count..), x = trt, fill = trt))
  ret$age = p + geom_histogram(aes(y = (..count..)/sum(..count..), x = age), color = "black", fill = "blue", alpha = 0.3)

  ret$stat = p + geom_bar(aes(y = (..count..)/sum(..count..), x = status, fill = status))

  ret$time = p + geom_histogram(aes(y = (..count..)/sum(..count..), x = time,
                                    group = status, fill = status, color = status),
                                alpha = 0.3)
  p1 = p + geom_point(aes(x = age, y = time))
  p2 = p + geom_boxplot(aes(x = sexF, y = time))
  p3 = p + geom_boxplot(aes(x = trt, y = time))
  ret$timevs = grid.arrange(p1,p2,p3)

  df = as.matrix(data[,4:13])
  cor = Hmisc::rcorr(df, type = "spearman")
  cor$P[is.na(cor$P)] = 0
  cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
                 ncol = ncol(cor$r), dimnames = dimnames(cor$r))

  meltedcor = reshape2::melt(cor$r)
  meltedP = reshape2::melt(cor$P)
  meltedcor$value[meltedP$value > 0.05] = 0
  meltedP = meltedP[meltedP$value > 0.05, ]

  ret$cor = ggplot(data = meltedcor, aes(x = Var1,y = Var2, fill = value)) +
    geom_tile() + theme_minimal() +
    scale_fill_gradient2(low = "white", high = "red",
                         mid = "pink", midpoint = 0.5,
                         limit = c(0,1), space = "Lab",
                         name="Spearman\nCorrelation") +
    geom_text(data = meltedP, aes(x = Var1, y = Var2, label = "x"),
              color = "red") +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(angle = 45),
          axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7))

  return(ret)

}

test_sim(1) # PH, Type I, 20%
test_sim(2) # PH, Type I, 50%
test_sim(3) # PH, Type I, 80%
test_sim(4) # PH, Right, 20%
test_sim(5) # PH, Right, 50%
test_sim(6) # PH, Right, 80%
test_sim(7) # PH, Indep, 20%
test_sim(8) # PH, Indep, 50%
test_sim(9) # PH, Indep, 80%

test_sim(10) # PH, Type I, 20%
test_sim(11) # PH, Type I, 50%
test_sim(12) # PH, Type I, 80%
test_sim(13) # PH, Right, 20%
test_sim(14) # PH, Right, 50%
test_sim(15) # PH, Right, 80%
test_sim(16) # PH, Indep, 20%
test_sim(17) # PH, Indep, 50%
test_sim(18) # PH, Indep, 80%

test_sim(19) # Non-PH, Type I, 20%
test_sim(20) # Non-PH, Type I, 50%
test_sim(21) # Non-PH, Type I, 80%
test_sim(22) # Non-PH, Right, 20%
test_sim(23) # Non-PH, Right, 50%
test_sim(24) # Non-PH, Right, 80%
test_sim(25) # Non-PH, Indep, 20%
test_sim(26) # Non-PH, Indep, 50%
test_sim(27) # Non-PH, Indep, 80%

test_sim(28) # Non-PH, Type I, 20%
test_sim(29) # Non-PH, Type I, 50%
test_sim(30) # Non-PH, Type I, 80%
test_sim(31) # Non-PH, Right, 20%
test_sim(32) # Non-PH, Right, 50%
test_sim(33) # Non-PH, Right, 80%
test_sim(34) # Non-PH, Indep, 20%
test_sim(35) # Non-PH, Indep, 50%
test_sim(36) # Non-PH, Indep, 80%
