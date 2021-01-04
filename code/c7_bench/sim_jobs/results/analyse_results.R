library(rstatix)
library(ggplot2)
library(patchwork)
# ---------------
# load and prep
# ---------------
set.seed(281020)
# load data
df = read.csv("c7_bench/sim_jobs/results/SimFinal.csv")
# cleanup columns
df$Data = factor(df$Data, levels = unique(df$Data))
df$Model = factor(df$Model, levels = unique(df$Model))

# set Cindex NA to 0 and rmse/mae NA to 100
df$C_Gonen[is.na(df$C_Gonen)] = 0
df$C_Uno[is.na(df$C_Uno)] = 0
df$RMSE[is.na(df$RMSE)] = max(df$RMSE[!is.na(df$RMSE)])
df$MAE[is.na(df$MAE)] = max(df$MAE[!is.na(df$MAE)])
df$VanA[df$VanA > 10] = 10

# ---------------
# create groups
# ---------------

df$Group = factor(rep(c(rep("Class", 8), rep("RSF", 5), rep("GBM", 4),
                      "SSVM", rep("ANN", 6)), 36),
                 levels = c("Class", "RSF", "GBM", "SSVM", "ANN"))
df$Model = rep(c("KM", "Nel", "AE", "CPH", "GLM", "Pen", "Par", "Flex",
                 "RFB", "RFL", "RFC", "RFCIF", "RRT", "GBC",
                 "GBU", "GBG", "COXB", "SVM", "CoxT",
                 "DH", "DS", "LH", "PCH", "DNN"), 36)
df$Model = factor(df$Model, levels = unique(df$Model))

df$Cens.Prop = factor(rep(c(rep("20", 24), rep("50", 24), rep("80", 24)), 12),
                     levels = c("20", "50", "80"))

df$Cens.Type = factor(rep(c(rep("TypeI", 72), rep("Right", 72), rep("Ind", 72)), 4),
                     levels = c("TypeI", "Right", "Ind"))

df$Surv.Dist = factor(rep(c("CoxWeib", "Weib", "Gomp", "Lnorm"), each = 216),
                     levels = c("CoxWeib", "Weib", "Gomp", "Lnorm"))

# ---------------------------
# compute and plot post-hocs function
# --------------------------
hsd = function(y, x, plot = TRUE, data = df, minimise = TRUE) {

  dfhsd = data.frame(tukey_hsd(data, as.formula(paste0(y, "~", x))))[,c(2,3,4,7)]
  dfhsd[,3:4] = round(dfhsd[,3:4], 4)
  out = list(hsd = dfhsd)

  if (plot) {
    dfhsd$group1 = factor(dfhsd$group1, levels = unique(dfhsd$group1))
    dfhsd$group2 = factor(dfhsd$group2, levels = rev(unique(dfhsd$group2)))
    dfhsd$estimate[dfhsd$p.adj > 0.05] = 0

    if (minimise) {
      low = "blue"; high = "red"
    } else {
      low = "red"; high = "blue"
    }

    out$p = ggplot(data = dfhsd, aes(x = group1,y = group2)) +
      geom_tile(aes(fill = estimate), color = "black", size = 0.5) +
      scale_fill_gradient2(low = low, high = high,
                           mid = "white", midpoint = 0,
                           space = "Lab",
                           name = "Y - X est") +
      geom_text(data =  dfhsd[dfhsd$p.adj > 0.05, c(1,2,4)], aes(x = group1, y = group2, label = "x"),
                color = "red") +
      theme(axis.title = element_blank(),
            axis.text.y = element_text(angle = 45),
            axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7),
            panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
            legend.position = "n")
  }
  return(out)
}

# ---------------------------
# ANOVA Uno
# --------------------------
aov = anova_test(
  data = df,
  dv = "C_Uno",
  between = c("Group", "Cens.Prop", "Cens.Type", "Surv.Dist")
)[,]
aov = data.frame(Effect = aov$Effect, p = aov$p)
aov$p = round(p.adjust(aov$p, method = "BH"), 4) * 31
aov$p.signif = ifelse(aov$p <= 0.05, "*", "")
aov[aov$p <= 0.05, ]




# ---------------------------
# ANOVA IGS
# --------------------------
aov = anova_test(
  data = df,
  dv = "IGS",
  between = c("Group", "Cens.Prop", "Cens.Type", "Surv.Dist")
)[,]
aov = data.frame(Effect = aov$Effect, p = aov$p)
aov$p = round(p.adjust(aov$p, method = "BH"), 4) * 31
aov$p.signif = ifelse(aov$p <= 0.05, "*", "")
aov[aov$p <= 0.05, ]

hsd("C_Uno", "Group", minimise = FALSE)$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "Uno's C - Group", size = 4.5) +
  hsd("IGS", "Group")$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "IGS - Group", size = 4.5) +
  hsd("C_Uno", "Surv.Dist", minimise = FALSE)$p +
  geom_text(aes(x = 2.8, y = 3), data = data.frame(),
            label = "Uno's C - Distr", size = 4.5) +
  hsd("IGS", "Surv.Dist")$p +
  geom_text(aes(x = 2.8, y = 3), data = data.frame(),
            label = "IGS - Distr", size = 4.5) +
  plot_annotation(tag_levels = "I")

hsd("VanA", "Surv.Dist")

hsd("IGS", "Cens.Type")

# ---------------------------
# ANOVA Within
# --------------------------

for (j in c("CoxWeib", "Weib", "Gomp", "Lnorm")) {
  for (i in c(5,7)) {
    aov = anova_test(
      data = subset(df, Surv.Dist == j),
      dv = colnames(df)[i],
      between = c("Group", "Cens.Type")
    )[,]
    dfaov = rbind(dfaov, data.frame(Effect = aov$Effect, p = aov$p, Dist = j))
  }
}
dfaov$Measure = rep(colnames(df)[c(5,7)], each = 15)
dfaov$p = round(p.adjust(dfaov$p, method = "BH"), 4) * 31
dfaov$p.signif = ifelse(dfaov$p <= 0.05, "*", "")
dfaov[dfaov$p <= 0.05, ]

hsd("C_Uno", "Group", data = subset(df, Surv.Dist == "CoxWeib"), minimise = FALSE)$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "Uno's C - Cox-Weibull", size = 4.5) +
  hsd("C_Uno", "Group", data = subset(df, Surv.Dist == "Weib"), minimise = FALSE)$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "Uno's C - Weibull", size = 4.5) +
  hsd("C_Uno", "Group", data = subset(df, Surv.Dist == "Gomp"), minimise = FALSE)$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "Uno's C - Gompertz", size = 4.5) +
  hsd("C_Uno", "Group", data = subset(df, Surv.Dist == "Lnorm"), minimise = FALSE)$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "Uno's C - Log-Normal", size = 4.5) +
  hsd("IGS", "Group", data = subset(df, Surv.Dist == "CoxWeib"))$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "IGS - Cox-Weibull", size = 4.5) +
  hsd("IGS", "Group", data = subset(df, Surv.Dist == "Weib"))$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "IGS - Weibull", size = 4.5) +
  hsd("IGS", "Group", data = subset(df, Surv.Dist == "Gomp"))$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "IGS - Gompertz", size = 4.5) +
  hsd("IGS", "Group", data = subset(df, Surv.Dist == "Lnorm"))$p +
  geom_text(aes(x = 3.5, y = 4), data = data.frame(),
            label = "IGS - Log-Normal", size = 4.5) +
  plot_layout(ncol = 2, nrow = 4, FALSE)

ggplot(subset(df, Surv.Dist == "CoxWeib"),
       aes(y = C_Uno, x = Group)) + geom_boxplot()
ggplot(subset(df, Surv.Dist == "CoxWeib"),
       aes(y = IGS, x = Group)) + geom_boxplot()

hsd("C_Uno", "Cens.Type", T, data = subset(df, Surv.Dist == "Lnorm"), minimise = FALSE)
hsd("IGS", "Cens.Type", T, data = subset(df, Surv.Dist == "Lnorm"))

ggplot(subset(df, Surv.Dist == "CoxWeib"),
       aes(y = C_Uno, x = Group)) + geom_boxplot()
ggplot(subset(df, Surv.Dist == "Lnorm"),
       aes(y = C_Uno, x = Group)) + geom_boxplot()

# ---------------
# anova, blocks
# ---------------

ggplot(df,
       aes(x = Group, y = C_Uno, fill = Group)) +
  geom_boxplot() + theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "n", axis.title.x = element_blank()) +

ggplot(df,
       aes(x = Group, y = IGS, fill = Group)) +
  geom_boxplot() +  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +

  plot_layout(guides = "collect") & theme(legend.position = "top")

dev.copy(png, "../images/c7_bench/sim_box.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

ggplot(df,
       aes(x = Data, y = C_Uno, fill = Surv.Dist)) +
  geom_boxplot() + theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +

ggplot(df,
       aes(x = Data, y = IGS, fill = Surv.Dist)) +
  geom_boxplot() + theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  plot_layout(guides = "collect") & theme(legend.position = "top")

dev.copy(png, "../images/c7_bench/box_survdists.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

# ---------------
# anova model effects
# ---------------
# test for significant differences due to model
dfaov_mod = data.frame()
for (i in c(5,7)) {
  aov = anova_test(
    data = df,
    dv = colnames(df)[i],
    between = "Model"
  )[,]
  dfaov_mod = rbind(dfaov_mod, data.frame(Effect = aov$Effect, p = aov$p))
}
dfaov_mod$Effect = colnames(df)[c(5,7)]
dfaov_mod$p = round(p.adjust(dfaov_mod$p, method = "BH"), 4) * 30
dfaov_mod$p.signif = ifelse(dfaov_mod$p <= 0.05, "*", "")
dfaov_mod[dfaov_mod$p <= 0.05, ]

hsd("C_Uno", "Model", minimise = FALSE)
hsd("IGS", "Model", minimise = TRUE)
dev.copy(png, "../images/c7_bench/sim_igs_mod_hsd", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

# ---------------
# visuals
# ---------------

# -------------------------------
# plot models across all datasets
# -------------------------------

# CPH (w. or w/o regularization) consistently best alongside boosting.
# Boosting may be slightly better w.r.t IGS.
# SVM consistency worst. A lot of variability in penalized and parametric,
# probably due to implementation. Variability in RSF and ANN likely
# due to tuning. Surprising;ly baselines all have good MAE. Terrible
# parametric MAE likely due to implementation.

# Uno's C (calibration)
ggplot(df,
       aes(x = Model, y = C_Uno, fill = Group)) +
  geom_boxplot() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45), legend.position = "top")

dev.copy(png, "../images/c7_bench/sim_uno_box.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

# IGS (scoring rule)
ggplot(df,
       aes(x = Model, y = IGS, fill = Group)) +
  geom_boxplot() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45), legend.position = "top")

dev.copy(png, "../images/c7_bench/sim_igs_box.png", width = 14,
         height = 10, units = "cm", res = 300)
dev.off()

# MAE (distance)
ggplot(df,
       aes(x = Model, y = MAE, fill = Group)) +
  geom_boxplot()
# -------------------------------
# plot datasets across all models
# -------------------------------

# groups seem identical for censoring proportion
ggplot(df,
       aes(x = Data, y = C_Uno, fill = Cens.Prop)) +
  geom_boxplot()

# possible 20% higher, which is the 'wrong' direction
ggplot(df,
       aes(x = Data, y = IGS, fill = Cens.Prop)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggplot(df,
       aes(x = Data, y = MAE, fill = Cens.Prop)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Compare censoring types
ggplot(df,
       aes(x = Data, y = C_Uno, fill = Cens.Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggplot(df,
       aes(x = Data, y = IGS, fill = Cens.Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggplot(df,
       aes(x = Data, y = MAE, fill = Cens.Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# groups seem identical for survival distribution

ggplot(df,
       aes(x = Data, y = IGS, fill = Surv.Dist)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggplot(df,
       aes(x = Data, y = MAE, fill = Surv.Dist)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
