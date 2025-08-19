

# Compare NOR and CHE phenology -----------------------------------------


# library -----------------------------------------------------------------
library(lme4)


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




# NOR ---------------------------------------------------------------------

# flowering onset nor ---------------------------------------------------------
flowering_onset_nor <- phenology_nor_pres_abs |> 
  filter(Stage == "number_flowers", presence == 1) |>   # keep only flowering observations
  group_by(species, unique_plant_ID, site_treatment, block_ID) |> 
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

# nor flowering onset per treatment ----------------------------------------
flowering_onset_treatment_nor <- flowering_onset_nor |> 
  group_by(site_treatment) |> 
  summarize(mean_onset = mean(onset_date),
            sd_onset = sd(onset_date),
            n = n(),
            .groups = "drop")

# plot flowering onset per treatment all species together
ggplot(flowering_onset_nor, aes(x = site_treatment, y = onset_date, fill = site_treatment)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
  labs(y = "Flowering onset (date)", x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

flo_onset_nor <- ggplot(flowering_onset_nor, aes(x = site_treatment, y = onset_date, fill = site_treatment)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(y = "Flowering onset (date)", x = "Treatment") +
  theme(axis.text.x = element_text(hjust = 1))+
  theme(legend.position = "none")
flo_onset_nor


ggsave(filename = "Output/Flowering_onset_NOR_all_species_together.png", plot = flo_onset_nor, width = 15, height = 10, units = "in")


# convert into julian days nor --------------------------------------------
flowering_onset_nor <- flowering_onset_nor |>
  mutate(onset_julian = as.numeric(strftime(onset_date, format = "%j")))

flowering_onset_treatment_nor <- flowering_onset_nor |> 
  group_by(site_treatment) |> 
  summarize(mean_onset_julian = mean(onset_julian, na.rm = TRUE),
            sd_onset_julian = sd(onset_julian, na.rm = TRUE),
            n = n(),
            .groups = "drop")


flo_onset_nor_julian <- ggplot(flowering_onset_nor, aes(x = site_treatment, y = onset_julian, fill = site_treatment)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(y = "Flowering onset (julian days)", x = "Treatment") +
  theme(axis.text.x = element_text(hjust = 1))+
  theme(legend.position = "none")
flo_onset_nor_julian

ggsave(filename = "Output/Flowering_onset_julian_NOR_all_species_together.png", plot = flo_onset_nor_julian, width = 15, height = 10, units = "in")


# check significance nor -------------------------------------------------
m1_n<- lmerTest::lmer(onset_julian ~ site_treatment + (1 | species) + (1 | block_ID),
                     data = flowering_onset_nor)

summary(m1_n)





# CHE ---------------------------------------------------------------------

# flowering onset che ---------------------------------------------------------
# use pheno_22_CHE because this is already presence absence
# in nor that needed to be changed first
pheno_22_CHE <- pheno_22_CHE |> 
  mutate(site_treatment = paste(site, treat_warming, treat_competition, sep = " "))

flowering_onset_che <- pheno_22_CHE |> 
  filter(phenology_stage == "No_FloOpen", value == 1) |>   # keep only flowering observations
  group_by(species, unique_plant_ID, site_treatment, block_ID) |> 
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




# convert into julian days che --------------------------------------------
flowering_onset_che <- flowering_onset_che |>
  mutate(onset_julian = as.numeric(strftime(onset_date, format = "%j")))

flowering_onset_treatment_che <- flowering_onset_che |> 
  group_by(site_treatment) |> 
  summarize(mean_onset_julian = mean(onset_julian, na.rm = TRUE),
            sd_onset_julian = sd(onset_julian, na.rm = TRUE),
            n = n(),
            .groups = "drop")

flo_onset_che_julian <- ggplot(flowering_onset_che, aes(x = site_treatment, y = onset_julian, fill = site_treatment)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(y = "Flowering onset (julian days)", x = "Treatment") +
  theme(axis.text.x = element_text(hjust = 1))+
  theme(legend.position = "none")
flo_onset_che_julian

ggsave(filename = "Output/Flowering_onset_julian_CHE_all_species_together.png", plot = flo_onset_che_julian, width = 15, height = 10, units = "in")





# che flowering onset per treatment ----------------------------------------
flowering_onset_treatment_che <- flowering_onset_che |> 
  group_by(site_treatment) |> 
  summarize(mean_onset = mean(onset_date),
            sd_onset = sd(onset_date),
            n = n(),
            .groups = "drop") 

# plot flowering onset per treatmen all species together
flo_onset_che <- ggplot(flowering_onset_che, aes(x = site_treatment, y = onset_date, fill = site_treatment)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(y = "Flowering onset (date)", x = "Treatment") +
  theme(axis.text.x = element_text(hjust = 1))+
  theme(legend.position = "none")
flo_onset_che

ggsave(filename = "Output/Flowering_onset_CHE_all_species_together.png", plot = flo_onset_che, width = 15, height = 10, units = "in")



# check significances che -------------------------------------------------

m1 <- lme4::lmer(onset_julian ~ site_treatment + (1 | species), data = flowering_onset_che)
summary(m1)

library(lmerTest)

m2 <- lmerTest::lmer(onset_julian ~ site_treatment + (1 | species), 
           data = flowering_onset_che)

summary(m2)   # now includes p-values

anova(m2)


# include block as random factor too --------------------------------------

m4 <- lmerTest::lmer(onset_julian ~ site_treatment + (1 | species) + (1 | block_ID),
                     data = flowering_onset_che)

summary(m4)

# lo ambi bare as reference -----------------------------------------------
flowering_onset_che$site_treatment <- factor(
  flowering_onset_che$site_treatment
)

flowering_onset_che$site_treatment <- relevel(
  flowering_onset_che$site_treatment,
  ref = "lo ambi bare"
)

m3 <- lmerTest::lmer(onset_julian ~ site_treatment + (1 | species),
                     data = flowering_onset_che)

summary(m3)




# flowering duration che --------------------------------------------------
# calculate first and last day of flowering and then duration
flowering_duration_che <- pheno_22_CHE |> 
  filter(phenology_stage == "No_FloOpen", value == 1) |>  # only flowering 
  group_by(species, unique_plant_ID, site_treatment, block_ID) |> 
  summarize(
    onset_date = min(date_measurement),
    end_date   = max(date_measurement),     # last day flowering observed
    duration   = as.numeric(max(date_measurement) - min(date_measurement)) + 1, # include both ends
    .groups = "drop"
  )

flowering_duration_che_summary <- flowering_duration_che |> 
  group_by(site_treatment, species) |> 
  summarize(
    mean_duration = mean(duration),
    sd_duration   = sd(duration),
    n = n(),
    .groups = "drop"
  )


ggplot(flowering_duration_che, aes(x = site_treatment, y = duration, fill = site_treatment)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(y = "Flowering daration (days)", x = "Treatment") +
  theme(axis.text.x = element_text(hjust = 1))+
  theme(legend.position = "none")

# plot onset against duration
ggplot(flowering_duration_che, aes(x = onset_date, y = duration, color = site_treatment)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Flowering onset (date)", y = "Flowering duration (days)") 











