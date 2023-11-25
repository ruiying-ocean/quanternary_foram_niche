## This script plots Fig. S9 of Ying et al. (2023)

library(tidyverse)
source("code/custom_theme.R")

niche_data <- read_csv("data/niche_hab.csv")

## use consistent species name
niche_data <- niche_data %>% mutate(sp=recode(sp,
                              "G. menardii"="G. cultrata",
                              "G. tenella"="G. tenellus",
                              "G. ruber"="G. ruber total"
                              ))


niche_data <- niche_data %>% filter(n > 15)

## get only 4 ka and 20 ka
lgm_niche <- niche_data %>% filter(bin %in% c(4, 20))

## change 4 to Holocene and 20 to LGM (note the data is binned every 8 ka)
## which means 4 ka is 0-8 ka and 20 ka is 16-28 ka
lgm_niche$bin <- ifelse(lgm_niche$bin == 4, "Holocene", "LGM")

## set order so that LGM is plotted in left
lgm_niche$bin <- factor(lgm_niche$bin, levels = c("LGM", "Holocene"))

## only sp occurs in both Holocene and LGM
lgm_niche <- lgm_niche %>%
  group_by(sp) %>%
  filter(n_distinct(bin) == 2) %>%
  ungroup()

## calculate the optimal temperature difference between Holocene and LGM
lgm_niche <- lgm_niche %>%
  arrange(sp, bin) %>%
  group_by(sp) %>%
  mutate(hol_minus_lgm = pe - lag(pe, n = 1, order_by = sp)) %>%
  ungroup()

## plot Holocene and LGM and their error bar (crossbar) using ggpubr
p <- lgm_niche %>% ggplot(aes(x = bin)) +
  geom_crossbar(aes(y = m, ymin = m - sd, ymax = m + sd, fill = bin), alpha = 0.3, width = 0.2) +
  geom_point(aes(y = pe, fill = bin), shape = 23) +
  labs(x = "", y = "Species temperature distribution (°C)") +
  facet_wrap(~sp, nrow = 3)

## label pe difference in each subplot
p <- p + geom_text(
  data = lgm_niche %>% distinct(sp, hol_minus_lgm) %>% drop_na(),
  aes(
    x = 1.5, y = -1,
    label = paste0("Δopt=", round(hol_minus_lgm, 1), "°C"),
    size = .5, vjust = 0
  )
)

p <- p + theme_publication() +
  scale_fill_manual(values = c("#0C4876", "#699c79")) +
  theme(legend.position = "none")

## save fig
ggsave("output/ext_fig6.png", p, width = 8, height = 6, units = "in", dpi = 300)
