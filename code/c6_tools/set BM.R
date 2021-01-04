library(sets); library(set6); library(microbenchmark)
set.seed(42)

microbenchmark::microbenchmark(
  as.list(x),
  cset(x),
  Set$new(x, class = "numeric"),

  setup = {
    x = round(runif(1000, 1, 3000))
  },
  unit = "s"
)

microbenchmark::microbenchmark(
  union(x, y),
  sets::cset_union(x, y),
  setunion(sx, sy),

  unit = "s",

  setup = {
    y = round(runif(1000, 1, 3000))
    cx = cset(x)
    cy = cset(y)
    sx = Set$new(x)
    sy = Set$new(y)
    x = as.list(x)
    y = as.list(y)
  }
)

microbenchmark::microbenchmark(
  setdiff(x, y),
  sets::cset_difference(x, y),
  setcomplement(sx, sy),

  unit = "s",

  setup = {
    y = round(runif(1000, 1, 3000))
    cx = cset(x)
    cy = cset(y)
    sx = Set$new(x)
    sy = Set$new(y)
    x = as.list(x)
    y = as.list(y)
  }
)
