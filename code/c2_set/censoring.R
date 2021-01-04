ggplot(data.frame(x = c(1, 2, 5, 7, 10), y = 1:5), aes(x = x, y = y)) +
  geom_blank() +
  geom_vline(xintercept = 8) +
  geom_segment(y = 1, yend = 1, x = 0, xend = 7) +
  geom_segment(y = 2, yend = 2, x = 0, xend = 8) +
  geom_segment(y = 3, yend = 3, x = 0, xend = 4) +
  geom_segment(y = 3, yend = 3, x = 4, xend = 6, linetype = 2) +
  geom_segment(y = 4, yend = 4, x = 0, xend = 1) +
  geom_segment(y = 4, yend = 4, x = 1, xend = 9, linetype = 2) +
  geom_segment(y = 5, yend = 5, x = 0, xend = 8) +
  geom_segment(y = 5, yend = 5, x = 8, xend = 9, linetype = 2) +
  geom_point(x = 4, y = 3, shape = 21, colour = "black", fill = "white", size = 4) +
  geom_point(x = 6, y = 3, shape = 18, size = 4) +
  geom_point(x = 7, y = 1, shape = 18, size = 4) +
  geom_point(x = 8, y = 2, shape = 18, size = 4) +
  geom_point(x = 9, y = 5, shape = 18, size = 4) +
  geom_point(x = 8, y = 5, shape = 21, size = 4, fill = "white") +
  geom_point(x = 1, y = 4, shape = 21, size = 4, fill = "white") +
  geom_point(x = 9, y = 4, shape = 18, size = 4) +
  labs(y = "Subject", x = "Time") +
  scale_x_continuous(labels = as.character(1:10), breaks = 1:10) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

dev.copy(png, "../images/c2_set/censoring.png", width = 14,
         height = 8, units = "cm", res = 300)
dev.off()

