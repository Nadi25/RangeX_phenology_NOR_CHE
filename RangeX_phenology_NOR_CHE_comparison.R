

# Compare NOR and CHE phenology -----------------------------------------


# import data -------------------------------------------------------------
#' @param file path to a quarto or Rmarkdown file
source_qmd <- function(file){
  tmp <- tempfile()
  knitr::purl(input = file, output = tmp)
  source(tmp)
}

source_qmd("RangeX_phenology_2023_data_cleaning.qmd")

source("RangeX_phenology_NOR_CHE_data_exploration.R")

# turn nor data into presence /absence ------------------------------------

# NOR: phenology_clean
phenology_nor_pres_abs <- phenology_clean |> 
  mutate(presence = ifelse(value > 0, 1, 0))

phenology_nor_pres_abs <- phenology_nor_pres_abs |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

phenology_nor_proportions <- phenology_nor_pres_abs |> 
  group_by(site, species, date, site_treatment, treat_warming, treat_competition, Stage) |> 
  summarise(prop = mean(presence, na.rm = TRUE)*100, # to get %
            n = n(),
            .groups = "drop")

# # then summarize by mean
# phenology_nor_pres_abs_summary <- phenology_nor_pres_abs |> 
#   group_by(species, date, site_treatment, treat_competition, Stage) |> 
#   summarize(mean_pa = mean(presence), .groups = "drop")

# subset of cennig --------------------------------------------------------
phenology_nor_cennig <- phenology_nor_proportions |> 
  filter(species == "cennig")

ggplot(phenology_nor_cennig, aes(x = date)) +
  geom_point(aes(y = prop, color = Stage))+
  geom_line(aes(y = prop, color = Stage)) +
  facet_grid(rows = vars(treat_warming), cols = vars(treat_competition)) +  # Added `scales = "free"`
  facet_wrap(~ site_treatment, ncol = 2)+
  labs(y = "Percentage of individuals in stage", x = "", title = "cennig") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  theme(legend.position = "top")


# loop through all species ------------------------------------------------

# get species list
species_list_nor <- unique(phenology_nor_proportions$species)


# plot 
plots_nor <- map(species_list_nor, ~ {
  dat <- filter(phenology_nor_proportions, species == .x)
  
  ggplot(dat, aes(x = date)) +
    geom_point(aes(y = prop, color = Stage)) +
    geom_line(aes(y = prop, color = Stage)) +
    facet_wrap(~ site_treatment, ncol = 2) +
    labs(y = "Percentage of individuals in stage", x = "", title = .x) +
    scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
    theme(legend.position = "top")
})

# create pdf
pdf("Output/Phenology_nor_all_species_presence_absence.pdf", width = 10, height = 8)
walk(plots_nor, print)
dev.off()










