m = list(msr("surv.calib_alpha", id = "vanA"), msr("surv.calib_alpha", id = "vanA_se", se = TRUE),
         msr("surv.calib_beta", id = "vanB"), msr("surv.calib_beta", id = "vanB_se", se = TRUE),
         msr("surv.cindex", weight_meth = "G2", id = "C_Uno"),
         msr("surv.cindex", weight_meth = "GH", id = "C_Gonen"),
         msr("surv.graf", id = "IGS"), msr("surv.graf", se = TRUE, id = "IGS_se"),
         msr("surv.intlogloss", id = "ILL"), msr("surv.intlogloss", se = TRUE, id = "ILL_se"),
         msr("surv.logloss", id = "SDLL"), msr("surv.logloss", se = TRUE, id = "SDLL_se"),
         msr("surv.rmse", id = "RMSE"), msr("surv.rmse", se = TRUE, id = "RMSE_se"),
         msr("surv.mae", id = "MAE"), msr("surv.mae", se = TRUE, id = "MAE_se"))

for (i in 1:54) {
  x = readRDS(paste0("~/Scratch/R/Sim", i, "_bmF.rds"))
  write.csv(x$score(m)[,c(5,10:25)], paste0("~/results/Sim", i, ".csv"))
}
