library(tidyverse)
library(ggpubr)
library(viridis)

custom_palette <- viridis(5)
source("code/lib.R")

## load in the data
df <- read_csv("data/niche_hab.csv")

## sum n by bin
## total sample: 42233
df %>% group_by(bin) %>% summarise(total_sample = sum(n))

## use consistent species name
df <- df %>% mutate(sp=recode(sp,
                              "G. menardii"="G. cultrata",
                              "G. tenella"="G. tenellus",
                              "G. ruber"="G. ruber total"
                              ))

## --------------------------------------
## add symbiont and spine information to species
## -----------------------------------------------
## merge the two data frames
df <- left_join(df, trait_info, by = "sp")

## add ecogroup definition
## if symb
df <- df %>% mutate(ecogroup = paste(Symbiosis, Spine, sep = " "))

## --------------------------------------
## Plot timeseris of optimal temperatures
## --------------------------------------

## Plot Fig. S8
##### plot time series, bin -> age, pe -> optimal temperature
## ext_fig5 <- ggplot() +
##     geom_line(data=df, aes(x=bin,y=pe,color=sp)) +
##     geom_line(data=df, aes(x=bin,y=opt_ym_temp)) +
##     facet_wrap(~reorder(sp, pe))

## ext_fig5 <- ext_fig5 + theme_bw()+
##   labs(x="Age (ka)", y="Species optimal temperature  (°C)")+
##   theme(strip.background = element_blank(),
##         strip.text = element_text(face = "italic"),
##         legend.position = "none")

## ggsave("output/ext_fig5.png", dpi=400, width = 7, height = 5)

## --------------------------------------
## Diagnose the effect of minimum number of samples
## on linear regression result
## --------------------------------------

thresholds <- seq(6, 30, 1)
p_values <- c()
sample_n <- c()
max_pe <- c()
min_pe <- c()

for (min_sample in thresholds){
  loc_df <- df %>% filter(n > min_sample)
  
  # Check if there are enough samples to perform linear regression
  if (nrow(loc_df) > 1) {
    # Conduct linear regression
    lm_result <- lm(delta_pe ~ delta_temp, data = loc_df)
    
    # Extract the p-value and concatenate it to the result vector
    p_values <- c(p_values, summary(lm_result)$coefficients[2, 4])
    sample_n <- c(sample_n, nrow(loc_df))

    min_pe <- c(min_pe, min(loc_df$delta_pe, na.rm=TRUE))
    max_pe <- c(max_pe, max(loc_df$delta_pe, na.rm=TRUE))      
  }
}

## save in a data frame (sensitivity analysis)
df_sa <- data.frame(thresholds, p_values, sample_n, min_pe, max_pe)

## plot the sensitivity analysis
## pivot longer the data
df_sa <- df_sa %>% pivot_longer(cols = c(p_values, sample_n, min_pe, max_pe),
                                names_to = "variable", values_to = "value")

## rename the variable
df_sa <- df_sa %>% mutate(variable = recode(variable,
                                            "p_values" = "p-value",
                                            "sample_n" = "available samples",
                                            "min_pe" = "min ∆optimal temperature",
                                            "max_pe" = "max ∆optimal temperature"))

## plot the sensitivity analysis
fig_sa <- ggplot(df_sa, aes(x = thresholds, y = value, color = variable)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y")

fig_sa <- fig_sa + theme_bw() + theme(legend.position = "none") +
    labs(x = "Minimum sample size to build kernel density estimator", y = "Value")

ggsave("output/fig_sa.png",fig_sa, dpi=400, width = 7, height = 5)

## --------------------------------------
## Final linear regression plot (Fig. 2)
## --------------------------------------

## eliminate the data with less than 15 samples
## although this does not affect the significance of the linear regression
## see next code section
df_final <- df %>% filter(n>15)

## get 95% confidence interval of delta_pe
t.test(df_final$delta_pe)$conf.int

## anova analysis
df_final %>% aov(delta_pe ~ Spine +Symbiosis, data = .) %>% summary()

## get maximum delta_pe
df_final %>% summary() ## delta_pe: (-12.5, 11.1); bin: (4, 700)

fig2 <- plot_lm(df_final, "delta_temp", "delta_pe", label_pos=c(-0.2, -14, 0.35, -15.5),
                label_size=4, marginal_plot = T,
                fill="bin", shape = 21, size = 4)
fig2
fig2 <- fig2 + theme_publication() +  theme(legend.position = "right")
fig2 <- fig2 + labs(x = "∆ species habitat temperature (°C)", y = "∆ species thermal optimum (°C)")

fig2 <- fig2 + scale_fill_stepsn(
    colours = custom_palette,
    breaks = seq(0, 800, 200),
    name = "Age (ka)"
  )


fig2 %>% ggsave("output/fig2.png",., dpi=400, width = 6, height = 4)
