c_real = c("aids2", "ALL", "bmt", "channing", "diabetic", "flchain", "gbsg", "grace",
           "hepatoCellular",
           "kidtran", "lung", "melanoma", "metabric", "mgus", "nafld1", "nki", "nwtco", "ova",
           "patient", "pbc", "pharmacoSmoking", "prostateSurvival", "rats", "support",
           "transplant", "tumor", "udca1", "veteran", "wbc1", "whas")

folds = rep(5, length(c_real))
folds[c_real %in% c("bmt", "pharmacoSmoking", "veteran")] = 4
folds[c_real == "hepatoCellular"] = 3

for (i in seq_along(c_real)) {
  path = sprintf("c7_bench/real_jobs/job_real_%s.sh", c_real[[i]])
  x = file.copy("c7_bench/real_jobs/TEMP_job_realTemp.sh",
                to = path,
                overwrite = FALSE)
  x = readLines(path)
  x = gsub("<TEMP>", c_real[[i]], x)
  x = gsub("<FOLDS>", folds[[i]], x)
  x = gsub("<SEED>", 200000 + (i * 3000), x)
  cat(x, file = path, sep = "\n")
}
