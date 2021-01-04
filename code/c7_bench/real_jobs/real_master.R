cat("\n-----------------------------\n")
cat("Installing and loading packages")
cat("\n-----------------------------\n")
source("~/simulation/load_packages.R")

cat("\n-----------------\n")
cat("Installing learners")
cat("\n-----------------\n")
source("~/simulation/get_learners.R")

c_real = c("aids2", "ALL", "bmt", "channing", "diabetic", "flchain", "gbsg", "grace",
           "hepatoCellular",
           "kidtran", "lung", "melanoma", "metabric", "mgus", "nafld1", "nki", "nwtco", "ova",
           "patient", "pbc", "pharmacoSmoking", "prostateSurvival", "rats", "support",
           "transplant", "tumor", "udca1", "veteran", "wbc1", "whas")


# id - dataset id
# outer_folds - number of folds for outer KCV (5)
# inner_folds - number of folds for inner KCV (3)
# n_evals - iterations for random search (100)
# seed - passed to set.seed
run_real = function(id, outer_folds = 5, inner_folds = 3, n_evals = 60, seed) {
  checkmate::assert_choice(id, c_real)

  task = TaskSurv$new(id, readRDS(paste0("c7_bench/real_jobs/data/", id, ".rds")),
                      "time", "status")

  learners = get_learners(folds = inner_folds, n_evals = n_evals)

  # set logger to maximum information - save to scratch
  logfile = paste0("~/Scratch/R/logger_", id, ".txt")
  logger = lgr::get_logger("mlr3")
  logger$set_threshold("debug")
  logger$add_appender(lgr::AppenderJson$new(logfile), name = "json")

  rr = rsmp("cv", folds = outer_folds)

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
  set.seed(seed, kind = "L'Ecuyer-CMRG")

  # run 1st benchmark
  cat("\n-----------------\n")
  cat("Running experiment excl. ANN")
  cat("\n-----------------\n")
  bm1 = try(benchmark(design), silent = TRUE)

  # save temp experiment
  saveRDS(bm1, paste0("~/Scratch/R/real_", id, "_bm1.rds"))

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
  set.seed(seed, kind = "L'Ecuyer-CMRG")

  # run 2nd benchmark
  cat("\n-----------------\n")
  cat("Running ANN experiment")
  cat("\n-----------------\n")
  bm2 = try(benchmark(design), silent = TRUE)

  # save temp experiment
  saveRDS(bm1, paste0("~/Scratch/R/real_", id, "_bm2.rds"))

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

    saveRDS(c(bm1, bm2), paste0("~/Scratch/R/real_", id, "_bmF.rds"))
    file.remove(paste0("~/Scratch/R/real_", id, "_bm1.rds"))
    file.remove(paste0("~/Scratch/R/real_", id, "_bm2.rds"))

  }

  # unlink logger
  logger$remove_appender("json")
}

cat("\n-----------------\n")
cat("Starting experiment")
cat("\n-----------------\n")

args = commandArgs(trailingOnly = TRUE)
run_real(id = args[1],
         outer_folds = as.integer(args[2]),
         seed = as.integer(args[3]))


