

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import phenology data che and nor ---------------------------------------
# CHE
# Read as a single column of text
raw <- read_lines("Data/RangeX_clean_Phenology_2022_CHE.csv")

# Remove the outer wrapping quotes and fix doubled quotes
raw <- gsub('^"|"$', '', raw)   # remove first and last quote in each line
raw <- gsub('""', '"', raw)     # replace doubled quotes with single quotes

# Write a cleaned temporary CSV
write_lines(raw, "Data/RangeX_clean_Phenology_2022_CHE_clean.csv")

# Now read normally
pheno_che <- read_csv("Data/RangeX_clean_Phenology_2022_CHE_clean.csv")

# NOR
pheno_nor <- read.csv("Data/RangeX_clean_phenology_2023_NOR.csv")


# import metadata CHE -----------------------------------------------------

meta_CHE <- read_csv("Data/RangeX_clean_MetadataFocal_CHE.csv")

meta_NOR <- read_csv("Data/RangeX_metadata_focal_NOR.csv")












































