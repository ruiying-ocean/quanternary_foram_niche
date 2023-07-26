# Load required libraries
library(tidyverse)
library(vegan)
library(ggpubr)

# Set seed for reproducibility
set.seed(88)
# Create the data frame
df <- data.frame(
  age = factor(rep(c("x", "y"), each = 1000)),
  sst = c(rnorm(1000, 10, sd=5), rnorm(1000, 11, sd=5))
)

# Extract distributions
x <- df$sst[df$age == "x"]
y <- df$sst[df$age == "y"]

# Density functions
dx <- density(x)
dy <- density(y)

# Calculate mean values
mean_x <- mean(x)
mean_y <- mean(y)

# Hellinger distance
h <- vegdist(rbind(dx$y, dy$y), method = "hellinger")

# Set the global theme for text properties
theme_set(
  theme_bw() +
    theme(
      text = element_text(
        size = 15             # Font size
      )
    )
)

# Create the density plot and disable legend
p1 <- ggdensity(df, x = "sst",
                add = "mean", rug = FALSE,
                color = "age", fill = "age", size=1, alpha=0.1,
                palette = c("#33AFFF", "#FF5733")) +
  theme(legend.position = "none")

# Add Hellinger distance and mean annotations to the density plot
p1 <- p1 + geom_text(aes(x = 7, y = 0.1, label = sprintf("Hellinger distance = %.2f", h)),
                     hjust = 0) +
  geom_text(aes(x = mean_x-2, y = 0.05, label = sprintf("%.1f", mean_x)),
            color = "#33AFFF") +
  geom_text(aes(x = mean_y+2, y = 0.05, label = sprintf("%.1f", mean_y)),
            color = "#FF5733") +
  xlab("Sea surface temperature (°C)") + ylab("Probability")

# plot2
niche_correlation <- readRDS("data/RY_realaysis.RDS")

p2 <- ggscatter(niche_correlation, x = "delta_temp", y = "delta_pe",
                fill = "bin", shape = 21, size = 4,
                add = "reg.line",
                add.params = list(color = "black", fill = "lightgray"),
                conf.int = TRUE,
                cor.coef = TRUE,
                cor.coeff.args = list(method = "pearson", label.x = 1, label.sep = "\n"),
                xlab = "Δ Ocean Temperature (°C)", ylab = "Δ Foraminiferal Optimal Temperature (°C)")

p2 <- p2 + theme(legend.position = c(0.9, 0.25)) +
  scale_fill_viridis_b(name = "Age (ka)")
## p2 <- p2+ scale_fill_gradientn(colors = scales::brewer_pal(palette = "YlGnBu")(6))
# Arrange plots side by side
plot_arranged <- ggarrange(p1, p2, ncol = 2, labels = c("a", "b"))

# Save the plot to a file
ggsave("output/fig3.jpg", plot_arranged, dpi = 300, width = 10, height = 4)
