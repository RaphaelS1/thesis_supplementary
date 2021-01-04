library(mlr)
set.seed(1)
lrn = makeLearner("classif.logreg")
y = data.frame(time = sample(10, 100, T))
x = data.frame(age = round(runif(100, 10, 60)), sex = sample(0:1, 100, T))
data = cbind(x, y)
data = reshape2::dcast(data, ...~time)
data[,3:12] = lapply(data[,3:12], as.logical)
colnames(data)[3:12] = paste0("Time", 1:10)
task = makeMultilabelTask("mlc", data = data, target = paste0("Time", 1:10))
br = makeMultilabelBinaryRelevanceWrapper(lrn)
cc = makeMultilabelClassifierChainsWrapper(lrn)
dbr = makeMultilabelDBRWrapper(lrn)
ns = makeMultilabelNestedStackingWrapper(lrn)
st = makeMultilabelStackingWrapper(lrn)

microbenchmark::microbenchmark(
  predict(train(br, task), task),
  predict(train(cc, task), task),
  predict(train(dbr, task), task),
  predict(train(ns, task), task),
  predict(train(st, task), task)
)

format(object.size(train(br, task)), "Mb")
format(object.size(train(cc, task)), "Mb")
format(object.size(train(dbr, task)), "Mb")
format(object.size(train(ns, task)), "Mb")
format(object.size(train(st, task)), "Mb")
