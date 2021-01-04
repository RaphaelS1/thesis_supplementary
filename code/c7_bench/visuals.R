source("c7_bench/sim_jobs/gen_simulated_data.R")
library(checkmate)
library(data.table)
library(ggplot2)

set.seed(1)
feat = gen_features()
feat$sexF = factor(feat$sexF)
feat$trt = factor(feat$trt)

p = ggplot(data = feat) + theme_minimal()
p_sex = p +
  geom_bar(aes(y = (..count..)/sum(..count..), x = sexF, fill = sexF)) +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  labs(y = "Probability")
p_trt = p + geom_bar(aes(y = (..count..)/sum(..count..), x = trt, fill = trt)) +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  labs(y = "Probability")
p_age = p + geom_histogram(aes(x = age, y = ..density..), bins = 15,
                   fill = "white", color = "black") +
  theme(legend.position = "none") + labs(y = "Density")
grid.arrange(p_sex, p_trt, p_age)
dev.copy(png, "../images/c7_bench/feats_demo.png", width = 14,
         height = 14, units = "cm", res = 300)
dev.off()

melted = melt(feat[,4:13])
ggplot(data = melted) + geom_boxplot(aes(y = value, group = variable)) +
  theme_minimal()
dev.copy(png, "../images/c7_bench/feats_box.png", width = 14,
         height = 9, units = "cm", res = 300)
dev.off()

cormat = rcorr(as.matrix(feat[,4:13]))
diag(cormat$P) = 1

melcor = reshape2::melt(round(cormat$r,2))
ggplot(data = melcor) +
  geom_tile(aes(x=Var1,y=Var2,fill=value), color = "gray") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  scale_fill_gradient2(low = "white", high = "red", mid = "orange", midpoint = 0.5,
                       limit = c(0,1), space = "Lab", name="Pearson\nCorrelation\n")
