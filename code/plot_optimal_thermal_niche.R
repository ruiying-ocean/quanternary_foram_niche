library(tidyverse)
library(ggpubr)
library(showtext)
library(broom)

## change species name
## Add a abbreviation column
species_abbrev <- function(full_name, sep_string = ". ") {
  name_parts <- str_split(full_name, " ")[[1]]
  genus_name <- name_parts[1]
  species_name <- name_parts[2]
  
  if (length(name_parts) > 2) {
    subspecies_name <- name_parts[3]
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
    abbrev <- paste(abbrev, subspecies_name, sep=" ")
  } else {
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
  }
  
  return(abbrev)
}

subset_mat <- function(age, depth, lat) {
  col <- paste("temp_ym", depth, lat, sep = "_")
  
  lat_split <- str_split(lat, pattern = "_")
  
  if (length(lat_split[[1]]) > 1) {
    # Calculate average of the range
    col1 <- paste("temp_ym", depth, lat_split[[1]][1], sep = "_")
    col2 <- paste("temp_ym", depth, lat_split[[1]][2], sep = "_")
    
    mat_total %>%
      mutate(mat = (get(col1) + get(col2)) / 2) %>%
      select(bin, mat) %>%
      filter(bin == age)%>%
      pull(mat)
    
  } else {
    # Single latitude column
    mat_total %>%
      select(bin, all_of(col)) %>%
      rename(mat = col) %>%
      filter(bin == age) %>%
      pull(mat)
  }
}

# Set the global theme for text properties
theme_set(
  theme_bw() +
    theme(
      text = element_text(
        family = "Roboto",    # Font family
        size = 15             # Font size
      )
    )
)

## foram niche over habitat depth
niche_hab <- read_csv("data/niche-sumry-metrics_SJ-ste_hab_2020-11-15.csv")

## find the optimal year mean temperature
## MAT data: 0m ,surface (40 m in the AOGCM), surface–subsurface (78 m), or subsurface (164 m)
mat    <- read_csv("data/global-MAT_10-deg-grid_8ka.csv")%>% 
  rename(temp_ym_0m_gl = temp_ym_0m,
         temp_ym_surf_gl = temp_ym_surf,
         temp_ym_surfsub_gl = temp_ym_surfsub,
         temp_ym_sub_gl = temp_ym_sub)

mat_ll <- read_csv("data/global-MAT_10-deg-grid_8ka_ll.csv") %>% 
  rename(temp_ym_0m_ll = temp_ym_0m,
         temp_ym_surf_ll = temp_ym_surf,
         temp_ym_surfsub_ll = temp_ym_surfsub,
         temp_ym_sub_ll = temp_ym_sub)

mat_hl <- read_csv("data/global-MAT_10-deg-grid_8ka_hl.csv") %>% 
  rename(temp_ym_0m_hl = temp_ym_0m,
         temp_ym_surf_hl = temp_ym_surf,
         temp_ym_surfsub_hl = temp_ym_surfsub,
         temp_ym_sub_hl = temp_ym_sub)

mat_ml <- read_csv("data/global-MAT_10-deg-grid_8ka_ml.csv") %>% 
  rename(temp_ym_0m_ml = temp_ym_0m,
         temp_ym_surf_ml = temp_ym_surf,
         temp_ym_surfsub_ml = temp_ym_surfsub,
         temp_ym_sub_ml = temp_ym_sub)

mat_total <- mat %>%
  left_join(mat_ll, by = "bin")%>%
  left_join(mat_hl, by = "bin") %>%
  left_join(mat_ml, by = "bin")

subset_mat(4, "surf","gl")
subset_mat(4, "surf","ml_ll")

## use short name
niche_hab$sp <- map_vec(niche_hab$sp, species_abbrev)

## update the species name first
niche_hab <- niche_hab %>%
  mutate(sp = recode(sp, "M. menardii" = "G. menardii",
                     "T. truncatulinoides" = "G. truncatulinoides",
                     "T. crassaformis" = "G. crassaformis",
                     "H. scitula" = "G. scitula",
                     "H. hirsuta" = "G. hirsuta"))

## species depth ecology from Rebotim et al. (2017) Biogeosciences
living_depth <- read_csv("data/Rebotim2017.csv") %>%
  rename(sp=Species, ald=`average living depth (m)`) %>%
  select(sp, ald)

## also use short name
living_depth$sp <- map_vec(living_depth$sp, species_abbrev)

living_depth <-  living_depth %>%
  mutate(sp = recode(sp, "G. ruber pink" = "G. ruber ruber",
                     "G. ruber white" = "G. ruber albus")) %>% distinct()

## fill missing data using Ralf Schiebel & Christoph Hemleben (2017) book
living_depth <- living_depth %>%
  mutate(
    ald = case_when(
      sp == "G. menardii" ~ 50, ## surface
      sp == "G. uvula" ~ 50, ## surface
      sp == "H. digitata" ~ 200, ## subsurface
      sp == "B. digitata" ~ 200, ## subsurface
      TRUE ~ ald
    )
  )

living_depth <- living_depth %>% 
  add_row(sp = "G. ruber", ald = 40) %>%
  add_row(sp = "G. tenella", ald=40) %>%
  add_row(sp = "G. conglobatus", ald=40)%>%
  add_row(sp = "T. trilobus", ald=60.7) ## same as sacculifer

## merge living_depth and niche_hab
niche_hab <- niche_hab %>% left_join(living_depth, by="sp")

## according to living depth to get a variable living_depth
niche_hab <- niche_hab %>% 
  mutate(temp_ym_hab = case_when(
    ald <= 78 ~ "surf",
    ald <= 164 & ald > 78 ~ "surfsub",
    ald > 164 ~ "sub",
    TRUE ~ "surf"
  ))

## add habitating latitude, global, ll, high, or mid
sp_lat <- data.frame(sp=c("B. digitata", "G. bulloides", "G. falconensis", "G. calida", "G. siphonifera",
                  "G. glutinata", "G. conglobatus", "G. ruber", "G. inflata", "G. rubescens",
                  "G. tenella", "G. hirsuta", "G. scitula", "G. menardii", "N. dutertrei",
                  "N. incompta", "N. pachyderma", "O. universa", "P. obliquiloculata", "T. sacculifer",
                "T. trilobus", "G. crassaformis", "G. truncatulinoides", "T. quinqueloba"),
           hab_lat=c("ll", "gl", "ml_ll", "ll_ml", "ll_ml",
                     "gl", "ll", "ll_ml", "gl", "ll",
                     "ll", "ml", "ml","ll","ll_ml",
                     "ml_hl", "hl_ml", "ml_ll","ll","ll",
                     "ll", "ll_ml","ml","hl_ml"))

niche_hab <- niche_hab %>% left_join(sp_lat, by="sp")

niche_hab <- niche_hab %>% rowwise() %>%
  mutate(opt_ym_temp = subset_mat(age = bin,  depth = temp_ym_hab,lat = hab_lat))

min_count <- 20

niche_hab_subset <- niche_hab %>% filter(n1 > min_count)

niche_hab_subset <- niche_hab_subset %>% arrange(sp, bin)
niche_correlation <- niche_hab_subset %>% group_by(sp) %>% mutate(delta_pe= pe- lag(pe,n=1,order_by=sp),
                                             delta_temp = opt_ym_temp-lag(opt_ym_temp,n=1,order_by=sp))

symbiosis_tbl <- read_csv("data/foram_sp_db.csv") %>% 
  mutate(sp = map_vec(Name, species_abbrev)) %>% mutate(fg=case_when(
    Symbiosis=="No" & Spinose == "No"~"Symbiont-barren Non-Spinose",
    Symbiosis=="Yes" & Spinose == "Yes"~"Symbiont-obligate Spinose",
    Symbiosis=="No" & Spinose == "Yes"~"Symbiont-barren Spinose",
    Symbiosis=="Yes" & Spinose == "No"~"Symbiont-facultative Spinose",
  ))
niche_correlation <- merge(niche_correlation,symbiosis_tbl, by="sp")

## small proportion of variance explained but significant
p <- ggscatter(niche_correlation, x = "delta_temp", y = "delta_pe",
          fill = "bin", shape = 21, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x=1, label.sep = "\n"),
          xlab = "Δ Ocean Temperature (°C)", ylab = "Δ Foraminiferal Optimal Temperature (°C)",
)

p <- p + theme(text = element_text(size = 16),
          legend.position = c(0.9, 0.2))+
  scale_fill_viridis_b(name = "Age (ka)")

ggsave("output/optimal_niche_driver.jpg", dpi = 300, width=6, height = 5)
saveRDS(niche_correlation, "data/RY_realaysis.RDS")

##### plot time series, bin -> age, pe -> optimal temperature
niche_hab %>% 
  ggplot() + geom_line(aes(x=bin,y=pe,color=sp)) +
  facet_wrap(~reorder(sp, pe)) + 
  geom_line(aes(x=bin,y=opt_ym_temp)) +
  theme_bw()+
  theme(legend.position = "none") +
  labs(x="Age (ka)", y="Species optimal temperature  (°C)")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "italic"))

ggsave("output/Topt_timeseries.png", dpi=400, width = 7, height = 5)
