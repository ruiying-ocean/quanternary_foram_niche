library(tidyverse)
library(ggpubr)

## load in the data
df <- read_csv("data/niche_hab.csv")

## calculate the change of temperature and foram's optimal tempeature
## only get the nearest time slice (smallest bin = 8 ka)
delta_temp <- rep(NA, nrow(df))
delta_pe <- rep(NA, nrow(df))

for (i in 1:nrow(df)) {
  sp_i <- df$sp[i]
  bin_i <- df$bin[i]
  temp_i <- df$opt_ym_temp[i]
  pe_i <- df$pe[i]
  
  ## get the same species
  sub_df <- df %>%
    filter(sp == sp_i)

  ## only get the nearest time slice (smallest bin = 8 ka)
  sub_df <- sub_df %>% arrange(bin) %>%
    filter((bin == bin_i + 8))
 
  if (nrow(sub_df) == 0) {
    print('No nearest time slice found')
  } else {
    delta_temp[i] <- sub_df$opt_ym_temp - temp_i
    delta_pe[i] <- sub_df$pe - pe_i
    }
}


## form dataframe
## containing 135 NAs: 1643 -> 1,508
df <- df %>% mutate(delta_pe = delta_pe, delta_temp = delta_temp)

## diagnose linear regression by changing the minimum number of samples
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

# Value used to transform the data
coeff <- 20000

ggplot(df_lm, aes(x=thresholds)) +
  
  geom_line( aes(y=p_values), linewidth=0.5) + 
  geom_line( aes(y=sample_n/coeff), linewidth=0.5) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "P-value",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Sample number")
  ) +
  xlab("Minimum number of samples") +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed") +
  geom_text(aes(x = 25, y = 0.055, label = "P-value = 0.05")) +
  theme_publication()


## bootstraped linear regression, get the slope confidence interval
# Load the 'boot' package if you haven't already
library(boot)

# Define a function to calculate the linear regression slope
lm_slope <- function(data, indices) {
  sampled_data <- data[indices, ]
  lm_result <- lm(delta_pe ~ delta_temp, data = sampled_data)
  return(coef(lm_result)[2])  # Extract the slope coefficient
}

# Set the number of bootstrap samples
n_boot_samples <- 1000

# Perform bootstrap resampling
boot_results <- boot(data = na.omit(df), statistic = lm_slope, R = n_boot_samples)

# Calculate confidence intervals
confidence_intervals <- boot.ci(boot_results, type = "perc")

# Print the confidence intervals
print(confidence_intervals)

## linear regression plot
p2 <- df %>%filter(n>15)%>%  ggscatter( x = "delta_temp", y = "delta_pe",
                                    fill = "bin", shape = 21, size = 4,
                                    add = "reg.line",
                                    add.params = list(color = "black", fill = "lightgray"),
                                    conf.int = TRUE,
                                    cor.coef = TRUE,
                                    cor.coeff.args = list(method = "pearson", label.x = -2, label.sep = "\n"),
                                    xlab = "∆ Habitat ocean temperature (°C)", ylab = "∆ Foraminiferal optimal temperature (°C)")

p2 <- p2 +  scale_fill_viridis_b(name = "Age (ka)") +
  theme(legend.position = "right")


confidence_interval <- function(vector, interval) {
  ## remove NA
  vector <- vector[!is.na(vector)]
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

confidence_interval(df$delta_pe, 0.95)


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
