library(tidyverse)
library(ggpubr)

## load in the data
df <- read_csv("data/niche_hab.csv")

## linear regression plot (Fig. 2)
df %>%filter(n>15) %>% ggscatter( x = "delta_temp", y = "delta_pe",
                                        fill = "bin", shape = 21, size = 4,
                                        add = "reg.line",
                                        add.params = list(color = "black", fill = "lightgray"),
                                        conf.int = TRUE,
                                        cor.coef = TRUE,
                                        cor.coeff.args = list(method = "pearson", label.x = -2, label.sep = "\n"),
                                        xlab = "∆ Habitat ocean temperature (°C)", ylab = "∆ Foraminiferal optimal temperature (°C)")

fig2 <- fig2 +  scale_fill_viridis_b(name = "Age (ka)") +
  theme(legend.position = "right")

fig2 %>% ggsave("output/fig2.png",., dpi=400, width = 6, height = 4)


## Diagnose the effect of minimum number of samples
thresholds <- seq(0, 30, 1)
p_values <- c()
sample_n <- c()

for (min_sample in thresholds) {
  loc_df <- df %>% filter(n > min_sample)
  
  # Check if there are enough samples to perform linear regression
  if (nrow(loc_df) > 1) {
    # Conduct linear regression
    lm_result <- lm(delta_pe ~ delta_temp, data = loc_df)
    
    # Extract the p-value and concatenate it to the result vector
    p_values <- c(p_values, summary(lm_result)$coefficients[2, 4])
    sample_n <- c(sample_n, nrow(loc_df))
  }
}

## plot in ggplot2
df_lm <- data.frame(thresholds, p_values, sample_n)
coeff <- 20000

figs9 <- ggplot(df_lm, aes(x=thresholds)) +
  geom_line(aes(y=p_values), linewidth=0.8, color='#f8766d') + 
  geom_line(aes(y=sample_n/coeff), linewidth=0.8, color='#00bfc4') +
  scale_y_continuous(
    name = "P-value",
    sec.axis = sec_axis(~.*coeff, name="Sample number")
  ) +
  xlab("Minimum number of samples") +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed") +
  geom_text(aes(x = 25, y = 0.055, label = "P-value = 0.05")) +
  theme_publication()

## change y axis color
figs9 <- figs9 + theme(axis.text.y.right = element_text(color = "#00bfc4"),
          axis.title.y.right = element_text(color = "#00bfc4"),
          axis.text.y.left = element_text(color = "#f8766d"),
          axis.title.y.left = element_text(color = "#f8766d"))

figs9 %>% ggsave("output/figs9.png", dpi=400, width = 6, height = 4)

## Plot Fig. S8
##### plot time series, bin -> age, pe -> optimal temperature
# niche_hab %>% 
#   ggplot() + geom_line(aes(x=bin,y=pe,color=sp)) +
#   facet_wrap(~reorder(sp, pe)) + 
#   geom_line(aes(x=bin,y=opt_ym_temp)) +
#   theme_bw()+
#   theme(legend.position = "none") +
#   labs(x="Age (ka)", y="Species optimal temperature  (°C)")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(face = "italic"))
# 
# ggsave("output/figs8.png", dpi=400, width = 7, height = 5)



## conbime the two dataframes
df_ecogroup <- df %>% left_join(trait_info,by='sp',relationship = "many-to-many")

