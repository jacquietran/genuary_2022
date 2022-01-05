library(ggforce)
library(ggplot2)

beziers <- data.frame(
  x = c(1, 2, 3, 4, # Specify coordinates to 3 or 4 pairs per group
        1, 3, 5, 7,
        1, 2, 4),
  y = c(0, 2, 0, -2,
        -3, 1, 3, 2,
        0.5, 5, 2),
  group = c(rep("group1", 4),
            rep("group2", 4),
            rep("group3", 3)))

ggplot() +
  geom_bezier(
    data = beziers, aes(x = x, y = y, group = group))