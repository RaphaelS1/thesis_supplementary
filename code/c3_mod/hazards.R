library(distr6)
library(ggplot2)
library(patchwork)

t = seq.int(0, 50)
shape = c(0.5, 1, 2, 5)
scale = 20/ (log(2)^(1 / shape))

v = VectorDistribution$new(
  distribution = "Weibull",
  params = data.frame(shape, scale),
  decorators = "ExoticStatistics"
)
data = cbind(t, reshape2::melt(v$hazard(t)))
colnames(data)[1:2] = c("T", "Shape")

weib = ggplot(data, aes(x = T, y = value, group = Shape, color = Shape)) +
  geom_line() + ylim(0, 0.1) + theme_minimal() +
  labs(x = "T", y = "Weibull Hazard, h(T)") +
  scale_color_discrete(labels = as.character(shape))

shape = c(0.0001, 0.025, 0.05, 0.5)
scale = log((-1 / shape) * log(1 / 2) + 1) / 20

v = VectorDistribution$new(
  distribution = "Gompertz",
  params = data.frame(shape, scale),
  decorators = "ExoticStatistics"
)
data = cbind(t, reshape2::melt(v$hazard(t)))
colnames(data)[1:2] = c("T", "Shape")

gomp = ggplot(data, aes(x = T, y = value, group = Shape, color = Shape)) +
  geom_line() + theme_minimal() +
  labs(x = "T", y = "Gompertz Hazard, h(T)") +
  scale_color_discrete(labels = as.character(shape)) + ylim(0, 1)

gridExtra::grid.arrange(weib, gomp, nrow = 2)

dev.copy(png, "../images/c3_mod/hazards.png", width = 14,
         height = 10, units = "cm", res = 300)
dev.off()

t = seq.int(0, 5, length.out = 100)
shape = c(0.5, 1, 1.5, 3, 7)
scale = rep(1, 5)

v = VectorDistribution$new(
  distribution = "Loglogistic",
  params = data.frame(shape, scale),
  decorators = "ExoticStatistics"
)
data = cbind(t, reshape2::melt(v$hazard(t)))
colnames(data)[1:2] = c("T", "Shape")

ggplot(data, aes(x = T, y = value, group = Shape, color = Shape)) +
  geom_line() + theme_minimal() +
  labs(x = "T", y = "Log-logistic Hazard, h(T)") +
  scale_color_discrete(labels = as.character(shape))

dev.copy(png, "../images/c3_mod/llog_hazard.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()
