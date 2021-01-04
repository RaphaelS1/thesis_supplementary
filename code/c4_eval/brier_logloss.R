library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)

pred = seq.int(0,1,0.01)
brier0 = (0 - pred)^2
brier1 = (1 - pred)^2
logloss0 = -log(1 - pred)
logloss1 = -log(pred)
df = data.frame(pred, brier0, brier1, logloss0, logloss1)
df = reshape2::melt(df, id.vars = "pred")
p1 = df %>% filter(grepl("brier", variable)) %>%
  ggplot(aes(x = pred, y = value, colour = variable)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Prediction", y = "Brier", title = "") +
  geom_label(aes(x = 0.1, y = 1, label = "Y = 1")) +
  geom_label(aes(x = 0.9, y = 1, label = "Y = 0", colour = "brier0")) +
  theme(legend.position = "n") +
  geom_hline(yintercept = 0.25, linetype = "dashed")

p2 = df %>% filter(grepl("logloss", variable)) %>%
  ggplot(aes(x = pred, y = value, colour = variable)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Prediction", y = "Log loss", title = "") +
  geom_label(aes(x = 0.15, y = 3, label = "Y = 1")) +
  geom_label(aes(x = 0.85, y = 3, label = "Y = 0", colour = "logloss0")) +
  theme(legend.position = "n") +
  geom_hline(yintercept = -log(0.5), linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.69, 1, 2, 3),
                     labels = c(0, 0.69, 1, 2, 3),
                     limits = c(0, 3))

grid.arrange(p1, p2, nrow = 1)
dev.copy(png, "~/Desktop/brier_logloss.png", width = 15,
         height = 8, units = "cm", res = 300)
dev.off()
