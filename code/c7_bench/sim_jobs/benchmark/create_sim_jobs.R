for (i in 1:54) {
  path = sprintf("c7_bench/sim_jobs/job_sim%s.sh", i)
  x = file.copy("c7_bench/sim_jobs/TEMP_job_simTemp.sh",
                to = path,
                overwrite = FALSE)
  x = readLines(path)
  x = gsub("<TEMP>", i, x)
  cat(x, file = path, sep = "\n")
}
