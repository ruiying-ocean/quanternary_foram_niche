library(tidyverse)
library(ggpubr)

## load in the data
df <- read_csv("data/niche_hab.csv")

## --------------------------------------
## Plot timeseris of optimal temperatures
## --------------------------------------

## Plot Fig. S8
##### plot time series, bin -> age, pe -> optimal temperature
df %>% 
  ggplot() + geom_line(aes(x=bin,y=pe,color=sp)) +
  facet_wrap(~reorder(sp, pe)) + 
  geom_line(aes(x=bin,y=opt_ym_temp)) +
  theme_bw()+
  theme(legend.position = "none") +
  labs(x="Age (ka)", y="Species optimal temperature  (°C)")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "italic"))

ggsave("output/figs8.png", dpi=400, width = 7, height = 5)

## --------------------------------------
## Diagnose the effect of minimum number of samples
## on linear regression result
## --------------------------------------

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

## --------------------------------------
## Final linear regression plot (Fig. 2)
## --------------------------------------

## eliminate the data with less than 15 samples
## although this does not affect the significance of the linear regression
## see next code section
df_final <- df %>% filter(n>15)

## get maximum delta_pe
df_final %>% summary() ## delta_pe: (-12.5, 11.1); bin: (4, 700)

lm(df_final$delta_pe ~ df_final$delta_temp) %>% summary() ## slope: 1.1

## bootstrapped linear regression slope
# helper function to return lm coefficients as a list
lm_coeffs <- function(x, y) {
  coeffs = as.list(coefficients(lm(y~x)))
  names(coeffs) = c('i', "s")
  return(coeffs)
}

## generate bootstrap samples of slope ('s') and intercept ('i')
library(data.table)
nboot <- 1000
mtboot <- lapply(seq_len(nboot), function(i) {
  sampled_data <- df_final[sample(nrow(df_final), replace = TRUE), ]
  lm_coeffs(sampled_data$delta_temp, sampled_data$delta_pe)
})
mtboot <- rbindlist(mtboot)

## confidence interval of slope based on bootstrapped samples
quantile(mtboot$s, c(0.025, 0.975)) ## (0.5, 1.7)

## plot the raw data
fig2 <- df_final %>%
    ggscatter( x = "delta_temp", y = "delta_pe",
              fill = "bin",
              shape = 21, size = 4,
              conf.int = FALSE,
              cor.coef = TRUE,
              cor.coeff.args = list(method = "pearson", label.x = -2, label.sep = "\n"),
              xlab = "∆ species habitat temperature (°C)", ylab = "∆ species optimal temperature (°C)")

## plot the bootstrapped linear regression result
fig2 <- fig2 +
    geom_abline(aes(intercept=i, slope=s), data = mtboot, size=0.1, color='grey', alpha=0.1) +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2)

fig2 <- fig2 +  scale_fill_viridis_b(name = "Age (ka)") +
    theme(legend.position = "right")

## use Helvetica font
theme_publication <- function(base_size = 14, base_family = "helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family)
  + theme(
      plot.title = element_text(face = "bold"),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      #axis.title = element_text(face = "plain", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "plain")
    ))
}

fig2 <- fig2 + theme_publication()

fig2 %>% ggsave("output/fig2.png",., dpi=400, width = 6, height = 4)

## --------------------------------------
## Use trait to explain the linear regression result
## --------------------------------------
