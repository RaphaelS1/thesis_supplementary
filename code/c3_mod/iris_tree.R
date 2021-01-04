library(party)
iris <- ctree(mpg ~ ., data = mtcars,
               controls = ctree_control(maxsurrogate = 3))
plot(iris, type = "simple")

library(randomForestSRC)

tree <- rpart(mpg~., mtcars)
rpart.plot::rpart.plot(tree, type = 1)
