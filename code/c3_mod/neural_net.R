library(nnet)
library(gamlss.add)

fit <- nnet(mtcars[,-1], mtcars[,1], size = 13, rang = 0.4,
            decay = 1e-3, maxit = 200)
plot(fit, rel.rsc = 2, circle.col = "lightblue", alpha.val = 0.3,
     neg.col = "red", max.sp = F)
