cat("\n-----------------------------\n")
cat("Installing and loading packages")
cat("\n-----------------------------\n")
source("c7_bench/load_packages.R")

source("c7_bench/sim_jobs/gen_simulated_data.R")

cat("\n-----------------\n")
cat("Installing learners")
cat("\n-----------------\n")
source("~/simulation/get_learners.R")

# sim_num - simulation number
# n - number of samples (1000)
# inner_folds - number of folds for inner KCV (3)
# n_evals - iterations for random search (100)
run_simulation = function(sim_num, n = 1000, inner_folds = 3, n_evals = 60) {
  #--------------------------------
  # Set-up task, learners, resample
  #--------------------------------
  cat("\n-----------------\n")
  cat("Loading data and learners")
  cat("\n-----------------\n")
  id = paste0("Sim", sim_num)

  # seeds for reproducible data-sets, separated by 3000.
  # seed_a = sim_num * 3000
  # seed_b = seed_a - 1500
  seed = sim_num * 3000

  task = TaskSurv$new(id, do.call(paste0("load_", id), list(n = n * 2, seed = seed)),
                       "time", "status")

  rr = rsmp("custom")
  rr$instantiate(task, train_sets = list(seq(n)), test_sets = list(seq(n) + n))

  learners = get_learners(folds = inner_folds, n_evals = n_evals)

  # set logger to maximum information - save to scratch
  logfile = paste0("~/Scratch/R/logger_", id, ".txt")
  logger = lgr::get_logger("mlr3")
  logger$set_threshold("debug")
  logger$add_appender(lgr::AppenderJson$new(logfile), name = "json")

  #----------------------------------------------
  # Experiment without ANNs
  #----------------------------------------------

  # Set-up parallelisation on all available cores
  cat("\n-----------------\n")
  cat("Starting parallelisation")
  cat("\n-----------------\n")

  future::plan("multiprocess")

  # benchmark design - excl. anns
  design = benchmark_grid(
    tasks = task,
    learners = learners[1:18],
    resamplings = rr
  )

  # seed for reproducible benchmarking, set for parallelisation
  set.seed(sim_num * 1000, kind = "L'Ecuyer-CMRG")

  # run 1st benchmark
  cat("\n-----------------\n")
  cat("Running experiment excl. ANN")
  cat("\n-----------------\n")
  bm1 = try(benchmark(design), silent = TRUE)

  # save temp experiment
  saveRDS(bm1, paste0("~/Scratch/R/", id, "_bm1.rds"))

  #----------------------------------------------
  # Experiment with ANNs
  #----------------------------------------------

  # turn off parallelisation for ANNs
  cat("\n-----------------\n")
  cat("Turning off parallelisation")
  cat("\n-----------------\n")

  future::plan("sequential")

  # benchmark design anns
  design = benchmark_grid(
    tasks = task,
    learners = learners[19:24],
    resamplings = rr
  )

  # seed for reproducible benchmarking, set for parallelisation
  set.seed(sim_num * 1000, kind = "L'Ecuyer-CMRG")

  # run 2nd benchmark
  cat("\n-----------------\n")
  cat("Running ANN experiment")
  cat("\n-----------------\n")
  bm2 = try(benchmark(design), silent = TRUE)

  # save temp experiment
  saveRDS(bm2, paste0("~/Scratch/R/", id, "_bm2.rds"))

  #----------------------------------------------
  # Combine experiments and save
  #----------------------------------------------
  cat("\n-----------------\n")
  cat("Saving experiments")
  cat("\n-----------------\n")

  if (class(bm1) == "try-error" && class(bm2) == "try-error") {

    cat("\n-----------------\n")
    cat("Both experiments failed.")
    cat("\n-----------------\n")

  } else if (class(bm1) == "try-error") {

    cat("\n-----------------\n")
    cat("Experiment 1 failed.")
    cat("\n-----------------\n")

  } else if (class(bm2) == "try-error") {

    cat("\n-----------------\n")
    cat("Experiment 2 failed.")
    cat("\n-----------------\n")

  } else {

    cat("\n-----------------\n")
    cat("Both experiments ran successfully.")
    cat("\n-----------------\n")

    p = c(bm1, bm2)
    saveRDS(p, paste0("~/Scratch/R/", id, "_bmF.rds"))
    file.remove(paste0("~/Scratch/R/", id, "_bm1.rds"))
    file.remove(paste0("~/Scratch/R/", id, "_bm2.rds"))

    cat("\n-----------------\n")
    cat("Scoring.")
    cat("\n-----------------\n")
    m = list(msr("surv.calib_alpha", id = "vanA"),# msr("surv.calib_alpha", id = "vanA_se", se = TRUE),
         #    msr("surv.calib_beta", id = "vanB"), msr("surv.calib_beta", id = "vanB_se", se = TRUE),
             msr("surv.cindex", id = "C_Harrell"),
             msr("surv.cindex", weight_meth = "G2", id = "C_Uno"),
             msr("surv.cindex", weight_meth = "GH", id = "C_Gonen"),
             msr("surv.graf", id = "IGS"), #msr("surv.graf", se = TRUE, id = "IGS_se"),
             msr("surv.intlogloss", id = "ILL"), # msr("surv.intlogloss", se = TRUE, id = "ILL_se"),
           #  msr("surv.logloss", id = "SDLL"), #msr("surv.logloss", se = TRUE, id = "SDLL_se"),
             msr("surv.rmse", id = "RMSE"), #msr("surv.rmse", se = TRUE, id = "RMSE_se"),
             msr("surv.mae", id = "MAE"))#, msr("surv.mae", se = TRUE, id = "MAE_se"))

    write.csv(p$score(m)[,c(5,10:19)], paste0("~/Scratch/R/", id, "_Score.csv"))
  }


  # unlink logger
  logger$remove_appender("json")
}

cat("\n-----------------\n")
cat("Starting experiment")
cat("\n-----------------\n")
args = as.integer(commandArgs(trailingOnly = TRUE))
run_simulation(sim_num = args[1])
