source('code/lib.R')

## instead of calculate global temperature change
## I subset the data by latitudinal bands
## Example to use
## e.g., global surface temperature at 4 ka
## or mid/low latitude temperature
## >>> subset_mat(4, "surf","gl")
## >>> subset_mat(4, "surf","ml_ll")

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

## foram niche over habitat depth
niche_hab <- read_csv("data/niche-sumry-metrics_SJ-ste_hab_2020-11-15.csv")

## find the optimal year mean temperature
## MAT data: 0m ,surface (40 m in the AOGCM), surface–subsurface (78 m), or subsurface (164 m)
mat    <- read_csv("data/global-MAT_10-deg-grid_8ka.csv")%>% 
  rename(temp_ym_0m_gl = temp_ym_0m,
         temp_ym_surf_gl = temp_ym_surf,
         temp_ym_surfsub_gl = temp_ym_surfsub,
         temp_ym_sub_gl = temp_ym_sub)

## low latitude
mat_ll <- read_csv("data/global-MAT_10-deg-grid_8ka_ll.csv") %>% 
  rename(temp_ym_0m_ll = temp_ym_0m,
         temp_ym_surf_ll = temp_ym_surf,
         temp_ym_surfsub_ll = temp_ym_surfsub,
         temp_ym_sub_ll = temp_ym_sub)

## high latitude
mat_hl <- read_csv("data/global-MAT_10-deg-grid_8ka_hl.csv") %>% 
  rename(temp_ym_0m_hl = temp_ym_0m,
         temp_ym_surf_hl = temp_ym_surf,
         temp_ym_surfsub_hl = temp_ym_surfsub,
         temp_ym_sub_hl = temp_ym_sub)

## mid latitude
mat_ml <- read_csv("data/global-MAT_10-deg-grid_8ka_ml.csv") %>% 
  rename(temp_ym_0m_ml = temp_ym_0m,
         temp_ym_surf_ml = temp_ym_surf,
         temp_ym_surfsub_ml = temp_ym_surfsub,
         temp_ym_sub_ml = temp_ym_sub)

## combine all data
mat_total <- mat %>%
  left_join(mat_ll, by = "bin")%>%
  left_join(mat_hl, by = "bin") %>%
  left_join(mat_ml, by = "bin")

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
## 50/200 are just arbitrary values to match the surface/subsurface categories
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

## calculate the change of temperature and foram's optimal tempeature
## only get the nearest time slice (smallest bin = 8 ka)
delta_temp <- rep(NA, nrow(niche_hab))
delta_pe <- rep(NA, nrow(niche_hab))

for (i in 1:nrow(niche_hab)) {
  sp_i <- niche_hab$sp[i]
  bin_i <- niche_hab$bin[i]
  temp_i <- niche_hab$opt_ym_temp[i]
  pe_i <- niche_hab$pe[i]
  
  ## get the same species
  sub_df <- niche_hab %>%
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


## combine delta data
## containing 135 NAs: 1643 -> 1,508
niche_hab <- niche_hab %>% add_column(delta_temp = delta_temp, delta_pe = delta_pe)

## save as csv
write_csv(niche_hab, "data/niche_hab.csv")
