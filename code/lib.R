library(tidyverse)
library(ggpubr)

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

foram_sp_db <- read_csv("https://raw.githubusercontent.com/ruiying-ocean/lgm_foram_census/main/fg/foram_taxonomy.csv")

trait_info <-foram_sp_db %>%
   mutate(sp = map_vec(`Species name`, species_abbrev)) %>% select(sp, Symbiosis, Spine)

## helper function to return lm coefficients as a list
lm_coeffs <- function(x, y) {
  coeffs = as.list(coefficients(lm(y~x)))
  names(coeffs) = c('i', "s")
  return(coeffs)
}

boot_lm <- function(data, x, y){
  library(data.table)
  set.seed(999999)
  ## generate bootstrap samples of slope ('s') and intercept ('i')
  nboot <- 1000
  mtboot <- lapply(seq_len(nboot), function(i) {
    ## resample
    sampled_data <- data[sample(nrow(data), replace = TRUE), ]
    ## get coefficients of linear model
    lm_coeffs(sampled_data[[x]], sampled_data[[y]])
  })
  mtboot <- rbindlist(mtboot)
  return(mtboot)
}

plot_lm <- function(data, x, y, label_pos, label_size=4, marginal_plot=FALSE, ...){
  if (marginal_plot) {
    # Use R2 instead of R
    p <- ggscatter(data, x = x, y = y, add = "reg.line",...) +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.x = label_pos[1], label.y = label_pos[2], size = label_size)
  } else {
    ## use ggscatterhist to plot the scatter plot
    p <- ggscatterhist(data, x = x, y = y, add = "reg.line",
                       margin.params = list(fill = "lightgray"),...) +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.x = label_pos[1], label.y = label_pos[2], size = label_size)
  }
  

  ## plot the bootstrapped regression results
  mtboot <- boot_lm(data, x, y)
  p <- p + geom_abline(aes(intercept=i, slope=s), data = mtboot, linewidth=0.1, color='grey', alpha=0.1)
  p <- p + geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2)

  ## annotate slope
  ## 95% CI
  ci_lower <- quantile(mtboot$s, 0.025) %>% round(1)
  ci_upper <- quantile(mtboot$s, 0.975) %>% round(1)
  median_value  <- quantile(mtboot$s, 0.5) %>% round(1)
  p <- p + annotate("text", x = label_pos[3], y = label_pos[4],  size = label_size,
                    label = paste0("slope: ",median_value, " (", ci_lower,", ", ci_upper,")"))
  return(p)
}

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
