
# RangeX phenology data exploration NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      14.08.25
## Author:    Nadine Arzt
## Purpose:   Data preparation and exploration


# comment -----------------------------------------------------------------
# CHE Flowers withered corresponds to infructescences in NOR

# CHE.lo.ambi.bare.wf.10.22.1 silvul 2022-06-02 EI No_Buds 11 --> should be 1

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(purrr)


# import phenology data CHE ---------------------------------------
# Read as a single column of text
raw <- read_lines("Data/RangeX_clean_Phenology_2022_CHE.csv")

# Remove the outer wrapping quotes and fix doubled quotes
raw <- gsub('^"|"$', '', raw)   # remove first and last quote in each line
raw <- gsub('""', '"', raw)     # replace doubled quotes with single quotes

# Write a cleaned temporary CSV
write_lines(raw, "Data/RangeX_clean_Phenology_2022_CHE_clean.csv")

# Now read normally
pheno_che <- read_csv("Data/RangeX_clean_Phenology_2022_CHE_clean.csv")

# import phenology data NOR ---------------------------------------
# pheno_nor <- read.csv("Data/RangeX_clean_phenology_2023_NOR.csv")

pheno_nor <- read.csv("Data/RangeX_clean_phenology_NOR_2023.csv", 
                      row.names = 1)

# import metadata CHE -----------------------------------------------------
meta_CHE <- read_csv("Data/RangeX_clean_MetadataFocal_CHE.csv")

# import metadata NOR -----------------------------------------------------
meta_NOR <- read.csv("Data/RangeX_metadata_focal_NOR.csv", row.names = 1)


# merge metadata with phenology -------------------------------------------
# CHE
pheno_22_CHE <- left_join(meta_CHE, pheno_che, by = c("unique_plant_ID", "species"))

# NOR
pheno_23_NOR <- left_join(meta_NOR, pheno_nor, by = c("unique_plant_ID", "species"))
# nor has column comment
# delete for now?

pheno_23_NOR <- pheno_23_NOR |> 
  select(-comment)


# one phenology data set -------------------------------------------------
# combine CHE and NOR
phenology <- rbind(pheno_22_CHE, pheno_23_NOR)


# rename pheno stages to match regions ------------------------------------




# che data exploration ----------------------------------------------------
phenology_che_median_quant <- pheno_22_CHE |> 
  group_by(site, species, date_measurement, treat_warming, treat_competition, phenology_stage) |> 
  summarise(median = median(value),
            lo = quantile(value, probs = 0.1, na.rm = TRUE), 
            hi = quantile(value, probs = 0.9, na.rm = TRUE), 
            .groups = "drop")

# filter only flowers
phenology_che_median_quant_flowers <- phenology_che_median_quant |> 
  filter(phenology_stage == "No_FloOpen")

## combine temperature and competition
phenology_che_median_quant_flowers <- phenology_che_median_quant_flowers |> 
  unite(treatment, site, treat_warming, treat_competition, sep = " ", remove = FALSE)


## plot median of flower number for all species and all treatments
ggplot(data = phenology_che_median_quant_flowers, aes(date_measurement, color = treatment))+
  geom_point(aes(y = median, color = treatment), size = 3)+
  geom_line(aes(y = median, color = treatment, group = treatment))+
  facet_wrap(vars(species), nrow = 2, ncol = 5)+
  labs(y = "Median", x = "", title = "") +
  theme(legend.position = "top")+
  #scale_color_manual(values = flowers_colors)+
  scale_color_manual(values = c("#A50021", "#2482A1", "#F48400", "#33FFFF", "#606060", "#C0C0C0"), name = "Treatment")



# subset of plamed --------------------------------------------------------

phenology_che_median_quant_plamed <- phenology_che_median_quant |> 
  filter(species == "plamed")

phenology_che_median_quant_plamed <- phenology_che_median_quant_plamed |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

phenology_che_median_quant_plamed <- phenology_che_median_quant_plamed |>
  arrange(date_measurement)


phenology_che_median_quant_plamed$site_treatment <- factor(
  phenology_che_median_quant_plamed$site_treatment,
  levels = c("hi ambi bare", "hi ambi vege", "hi warm bare", "hi warm vege", "lo ambi bare", "lo ambi vege"))

ggplot(phenology_che_median_quant_plamed, aes(x = date_measurement)) +
  geom_line(aes(y = median, color = phenology_stage)) +
  geom_ribbon(aes( ymin = lo, ymax = hi, fill = phenology_stage), linewidth = 2, alpha = 0.4) +
  # facet_grid(rows = vars(treat_warming), cols = vars(treat_competition)) +  # Added `scales = "free"`
  facet_wrap(~ site_treatment, ncol = 2)+
  labs(y = "Median +/- quantiles", x = "", title = "Plantago media") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  theme(legend.position = "top")



# subset of brapin --------------------------------------------------------
phenology_che_median_quant_brapin <- phenology_che_median_quant |> 
  filter(species == "brapin")

phenology_che_median_quant_brapin <- phenology_che_median_quant_brapin |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

phenology_che_median_quant_brapin <- phenology_che_median_quant_brapin |>
  arrange(date_measurement)


phenology_che_median_quant_brapin$site_treatment <- factor(
  phenology_che_median_quant_brapin$site_treatment,
  levels = c("hi ambi bare", "hi ambi vege", "hi warm bare", "hi warm vege", "lo ambi bare", "lo ambi vege"))

ggplot(phenology_che_median_quant_brapin, aes(x = date_measurement)) +
  geom_line(aes(y = median, color = phenology_stage)) +
  geom_ribbon(aes( ymin = lo, ymax = hi, fill = phenology_stage), linewidth = 2, alpha = 0.4) +
  # facet_grid(rows = vars(treat_warming), cols = vars(treat_competition)) +  # Added `scales = "free"`
  facet_wrap(~ site_treatment, ncol = 2)+
  labs(y = "Median +/- quantiles", x = "", title = "Brapin") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  theme(legend.position = "top")



# subset of scacol --------------------------------------------------------
phenology_che_median_quant_scacol <- phenology_che_median_quant |> 
  filter(species == "scacol")

phenology_che_median_quant_scacol <- phenology_che_median_quant_scacol |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

phenology_che_median_quant_scacol <- phenology_che_median_quant_scacol |>
  arrange(date_measurement)


phenology_che_median_quant_scacol$site_treatment <- factor(
  phenology_che_median_quant_scacol$site_treatment,
  levels = c("hi ambi bare", "hi ambi vege", "hi warm bare", "hi warm vege", "lo ambi bare", "lo ambi vege"))

ggplot(phenology_che_median_quant_scacol, aes(x = date_measurement)) +
  geom_line(aes(y = median, color = phenology_stage)) +
  #geom_ribbon(aes(y = median, ymin = lo, ymax = hi, fill = phenology_stage), linewidth = 2, alpha = 0.4) +
  # facet_grid(rows = vars(treat_warming), cols = vars(treat_competition)) +  # Added `scales = "free"`
  facet_wrap(~ site_treatment, ncol = 2)+
  labs(y = "Median +/- quantiles", x = "", title = "Brapin") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  theme(legend.position = "top")




# che  ---------------------------------------------------------------
# calculate means to get proportions of plants that are in a certain pheno stage at a certain point in time
phenology_che_proportions <- pheno_22_CHE |> 
  group_by(site, species, date_measurement, treat_warming, treat_competition, phenology_stage) |> 
  summarise(prop = mean(value, na.rm = TRUE)*100, # to get %
            n = n(),
            .groups = "drop")

phenology_che_proportions <- phenology_che_proportions |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

# subset of scacol --------------------------------------------------------
phenology_che_proportions_scacol <- phenology_che_proportions |> 
  filter(species == "scacol")

ggplot(phenology_che_proportions_scacol, aes(x = date_measurement)) +
  geom_point(aes(y = prop, color = phenology_stage))+
  geom_line(aes(y = prop, color = phenology_stage)) +
  facet_grid(rows = vars(treat_warming), cols = vars(treat_competition)) +  # Added `scales = "free"`
  facet_wrap(~ site_treatment, ncol = 2)+
  labs(y = "Percentage of individuals in stage", x = "", title = "Brapin") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
  theme(legend.position = "top")



# loop through all species ------------------------------------------------

# get species list
species_list <- unique(phenology_che_proportions$species)

# 
plots <- map(species_list, ~ {
  dat <- filter(phenology_che_proportions, species == .x)
  
  ggplot(dat, aes(x = date_measurement)) +
    geom_point(aes(y = prop, color = phenology_stage)) +
    geom_line(aes(y = prop, color = phenology_stage)) +
    facet_wrap(~ site_treatment, ncol = 2) +
    labs(y = "Percentage of individuals in stage", x = "", title = .x) +
    scale_color_manual(values = c("darkblue", "darkred", "darkorange", "darkgreen")) +
    theme(legend.position = "top")
})

pdf("phenology_che_species.pdf", width = 10, height = 8)
walk(plots, print)
dev.off()






























