## This script plots Fig. S9 of Ying et al. (2023)

library(tidyverse)
niche_data <- readRDS("data/RY_realaysis.RDS")

## get only 4 ka and 20 ka
lgm_niche <- niche_data %>% filter(bin %in% c(4, 20))

## change 4 to Holocene and 20 to LGM (note the data is binned every 8 ka)
## which means 4 ka is 0-8 ka and 20 ka is 16-28 ka
lgm_niche$bin <- ifelse(lgm_niche$bin == 4, "Holocene", "LGM")

## only sp occurs in both Holocene and LGM
lgm_niche <- lgm_niche %>% group_by(sp) %>%
  filter(n_distinct(bin) == 2) %>%
  ungroup()

## calculate the optimal temperature difference between Holocene and LGM
lgm_niche <- lgm_niche %>%
  group_by(sp) %>%
  mutate(hol_minus_lgm = lag(pe,n=1,order_by=sp)-pe)%>%
  ungroup()

## plot Holocene and LGM and their error bar

p <- lgm_niche %>%  ggplot(aes(x=bin)) +
  geom_crossbar(aes(y=m, ymin = m - sd, ymax = m + sd, fill=bin),alpha=0.3, width = 0.3)+
  geom_point(aes(y = pe, fill = bin), size = 3, shape=23) +
  labs(x = "", y = "Temperature (°C)") +
  theme_bw() +
  facet_wrap(~sp, nrow=2)+
  theme(legend.position = "none")

## add pe difference in each subplot
p <- p + geom_text(data = lgm_niche %>% distinct(sp, hol_minus_lgm) %>% drop_na(), 
              aes(x=1.5,y = 0.5,
                  label = paste0("Δopt=", round(hol_minus_lgm, 1), "°C"),
                  size = 1, vjust = 0))

## save fig
ggsave("output/lgm_niche.png", p, width = 8, height = 5, units = "in", dpi = 300)
