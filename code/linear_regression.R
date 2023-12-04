library(tidyverse)
library(ggpubr)
source("code/lib.R")

## load in the data
df <- read_csv("data/niche_hab.csv")

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
ext_fig5 <- ggplot() +
    geom_line(data=df, aes(x=bin,y=pe,color=sp)) +
    geom_line(data=df, aes(x=bin,y=opt_ym_temp)) +
    facet_wrap(~reorder(sp, pe))

ext_fig5 <- ext_fig5 + theme_bw()+
  labs(x="Age (ka)", y="Species optimal temperature  (°C)")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = "none")

ggsave("output/ext_fig5.png", dpi=400, width = 7, height = 5)

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
    sec.axis = sec_axis(~.*coeff, name="Samples left")
  ) +
  xlab("Sample number thereshold") +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed") +
  geom_text(aes(x = 25, y = 0.055, label = "P-value = 0.05")) +
  theme_publication()

## change y axis color
figs9 <- figs9 + theme(axis.text.y.right = element_text(color = "#00bfc4"),
          axis.title.y.right = element_text(color = "#00bfc4"),
          axis.text.y.left = element_text(color = "#f8766d"),
          axis.title.y.left = element_text(color = "#f8766d"))

ggsave("output/figs2.png", figs9, dpi=400, width = 6, height = 4)

## --------------------------------------
## Final linear regression plot (Fig. 2)
## --------------------------------------

## eliminate the data with less than 15 samples
## although this does not affect the significance of the linear regression
## see next code section
df_final <- df %>% filter(n>15)

## anova analysis
df_final %>% aov(delta_pe ~ Spine +Symbiosis, data = .) %>% summary()

## get maximum delta_pe
df_final %>% summary() ## delta_pe: (-12.5, 11.1); bin: (4, 700)


fig2 <- plot_lm(df_final, "delta_temp", "delta_pe", label_pos=c(-0.2, -14, 0.35, -15.5),
                label_size=4, 
                fill="bin", shape = 21, size = 4)
fig2 <- fig2 + theme_publication() +  theme(legend.position = "right")
fig2 <- fig2 + labs(x = "∆ species habitat temperature (°C)", y = "∆ species thermal optimum (°C)")

library(viridis)
custom_palette <- viridis(5)

fig2 <- fig2 + scale_fill_stepsn(
    colours = custom_palette,
    breaks = seq(0, 800, 200),
    name = "Age (ka)"
  )


fig2 %>% ggsave("output/fig2.png",., dpi=400, width = 6, height = 4)
