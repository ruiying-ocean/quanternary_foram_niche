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

## instead of calculate global temperature change
## I subset the data by latitudinal bands
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

## Example to use
## e.g., global surface temperature at 4 ka
## or mid/low latitude temperature
subset_mat(4, "surf","gl")
subset_mat(4, "surf","ml_ll")


## update the species name for consistency
## use short name
niche_hab$sp <- map_vec(niche_hab$sp, species_abbrev)

niche_hab <- niche_hab %>%
  mutate(sp = recode(sp, "M. menardii" = "G. menardii",
                     "T. truncatulinoides" = "G. truncatulinoides",
                     "T. crassaformis" = "G. crassaformis",
                     "H. scitula" = "G. scitula",
                     "H. hirsuta" = "G. hirsuta"))

## Link species to their depth habitat
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
## 50/200 are just arbitrary values to match the category (surface/subsurface)
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

## opt_ym_temp is the habitat temperature for the species
niche_hab <- niche_hab %>% rowwise() %>%
  mutate(opt_ym_temp = subset_mat(age = bin,  depth = temp_ym_hab,lat = hab_lat))


## filter out insufficient sample for the KDE estimate (n < 20 for univariate KDE)
min_count <- 20
niche_hab_subset <- niche_hab %>% filter(n1 > min_count)

niche_hab_subset <- niche_hab_subset %>% arrange(sp, bin)
niche_correlation <- niche_hab_subset %>% group_by(sp) %>% mutate(delta_pe= pe- lag(pe,n=1,order_by=sp),
                                             delta_temp = opt_ym_temp-lag(opt_ym_temp,n=1,order_by=sp))

symbiosis_tbl <- read_csv("~/Science/lgm_foram_census/fg/foram_sp_db.csv") %>%
    mutate(sp = map_vec(Name, species_abbrev))  %>% select(sp, Symbiosis, Spinose)

niche_correlation <- merge(niche_correlation,symbiosis_tbl, by="sp")

aov(data=niche_correlation, delta_pe ~ Symbiosis+Spinose) %>% summary() ## Not significantly different by traits

## linear regression model
lm(data=niche_correlation, delta_pe ~ delta_temp) %>% summary()
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
