

# Compare NOR and CHE phenology -----------------------------------------


# import data -------------------------------------------------------------
#' #' @param file path to a quarto or Rmarkdown file
#' source_qmd <- function(file){
#'   tmp <- tempfile()
#'   knitr::purl(input = file, output = tmp)
#'   source(tmp)
#' }
#' 
#' source_qmd("RangeX_phenology_2023_data_cleaning.qmd")

# # Extract R code from a Quarto file
# knitr::purl("RangeX_phenology_2023_data_cleaning.qmd", output = "RangeX_phenology_2023_data_cleaning_from_qmd.R")
# 
# # Then you can source it
# source("RangeX_phenology_2023_data_cleaning_from_qmd.R")

# need to run "RangeX_phenology_2023_data_cleaning.qmd" itself

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





# flowering onset nor ---------------------------------------------------------
flowering_onset_nor <- phenology_nor_pres_abs |> 
  filter(Stage == "number_flowers", presence == 1) |>   # keep only flowering observations
  group_by(species, unique_plant_ID, site_treatment) |> 
  summarize(onset_date = min(date),  # first date flowering occurs
            .groups = "drop")

# plot flowering onset per species
ggplot(flowering_onset_nor, aes(x = species, y = onset_date)) +
  geom_jitter(width = 0.2, alpha = 0.5) +   # individual points
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +  # summary
  labs(y = "Flowering onset (date)", x = "Species")

# per treatment
ggplot(flowering_onset_nor, aes(x = species, y = onset_date)) +
  geom_boxplot() +
  facet_wrap(~ site_treatment) +
  labs(y = "Flowering onset (date)", x = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flowering_onset_nor, aes(x = species, y = onset_date, fill = site_treatment)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
  labs(y = "Flowering onset (date)", x = "Species")+
  facet_wrap(~ site_treatment)

# per species to see effect of treatment
ggplot(flowering_onset_nor, aes(x = site_treatment, y = onset_date, fill = site_treatment)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
  facet_wrap(~ species, scales = "free_x") +  # one panel per species
  labs(y = "Flowering onset (date)", x = "Treatment")



# flowering onset che ---------------------------------------------------------
# use pheno_22_CHE because this is already presence absence
# in nor that needed to be changed first
pheno_22_CHE <- pheno_22_CHE |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

flowering_onset_che <- pheno_22_CHE |> 
  filter(phenology_stage == "No_FloOpen", value == 1) |>   # keep only flowering observations
  group_by(species, unique_plant_ID, site_treatment) |> 
  summarize(onset_date = min(date_measurement),  # first date flowering occurs
            .groups = "drop")

# plot flowering onset per species
ggplot(flowering_onset_che, aes(x = species, y = onset_date)) +
  geom_jitter(width = 0.2, alpha = 0.5) +   # individual points
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +  # summary
  labs(y = "Flowering onset (date)", x = "Species")

# per treatment
ggplot(flowering_onset_che, aes(x = species, y = onset_date)) +
  geom_boxplot() +
  facet_wrap(~ site_treatment) +
  labs(y = "Flowering onset (date)", x = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flowering_onset_che, aes(x = species, y = onset_date, fill = site_treatment)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
  labs(y = "Flowering onset (date)", x = "Species")+
  facet_wrap(~ site_treatment)

# per species to see effect of treatment
ggplot(flowering_onset_che, aes(x = site_treatment, y = onset_date, fill = site_treatment)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
  facet_wrap(~ species, scales = "free_x") +  # one panel per species
  labs(y = "Flowering onset (date)", x = "Treatment")









