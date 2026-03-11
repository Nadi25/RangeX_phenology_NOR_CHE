

Read_me:
RangeX Phenology - Biomass


R.project: RangeX_phenology_NOR_CHE

GitHub: GitHub - Nadi25/RangeX_phenology_NOR_CHE.GitHub

Figures: C:\Users\Nadine\OneDrive - University of Bergen\PhD_RangeX\R codes\RangeX_phenology_NOR_CHE\Output\Biomass

Aim: 
Is the effect of cooling on number of flowers just because of biomass? Do bigger plants produce more flowers?
	Use focal biomass from 2024 to fit one model per species that predicts biomass best. 
	Trait data available for 2023 and 24
	Assumption that this trait – biomass correlation stays constant
	Predict biomass for 2023 and correlate it with number of flowers
	Find out about the effect of cooling on number of flowers when adjusted for biomass

Data used: 
  •	Metadata focal: RangeX_clean_MetadataFocal_NOR.csv
•	Biomass: 
  o	RangeX_raw_NOR_biomass_high_2024_new.csv
o	RangeX_raw_NOR_biomass_low_2024_new.csv
•	Traits:
  o	RangeX_raw_demographic_traits_high_2024.csv
o	RangeX_raw_demographic_traits_low_2024.csv
o	RangeX_raw_traits_high_2023.csv
o	RangeX_raw_traits_low_2023.csv
o	RangeX_YearlyDemographics.csv
•	Phenology:
  o	RangeX_clean_Phenology_2023_NOR.csv (read in the latest phenology data)
o	RangeX_raw_phenology_low_2023.xlsx (in old data cleaning script)
o	RangeX_raw_phenology_high_2023.xlsx (in old data cleaning script)

R-scripts used:
  •	Effect_cooling_on_flower_number_NOR_10.09.25.R





The story:
  
  0. Data preparation
Script for traits 23: RangeX_data_paper_cleaning_demographic_traits_23.R
Script for phenology 23: RangeX_data_paper_cleaning_phenology_2023.R but used updated version of data (RangeX_clean_Phenology_2023_NOR.csv) from RangeX_datapaper project. New unique_plant_ID with replanting number.

Biomass: had problems with exporting and importing biomass data  resulted in some species having biomass clusters (e.g. cyncri). Redownloaded. Fixed clusters.

1. Effect of cooling on number of flowers (total reproductive output)
Old: Effect_cooling_on_flower_number_NOR_10.09.25.R has old lmer model
New: xxx with glmer.nb model (to do!)
(old figure, needs to be updated)
	Plants produce significantly fewer flowers when transplanted beyond their current range when native vegetation is removed
	Flower number stays the same in vegetation
	Cooling (through upslope transplantation) leads to fewer flowers without neighbours
	At both sites, plants produce more flowers when neighbour vegetation is removed

But: Is this just an effect of biomass?
  
  2. Predict biomass 24 with traits 24 with one general model for all species
Script: Biomass_traits_correlation_NOR.R
Test different models with different trait combinations.
Best fit: m_stems_height3 <- lmerTest::lmer(log_biomass ~ log_no_stems * height_reproductive_str + (1|species) + (1|block_ID), data= analysis_data)
Could be improved more (log_height) but since we decided to use species models, I didn’t change it.
Some control plotting: Biomass_traits_expl_plots.R
3. Predict biomass 24 with traits 24 using species specific models (one per species)
Script: Biomass_traits_correlation_per_species.R
Test single traits first, with and without log transformation. Use the best 2 models and test if single, additive or interactive fits best. For some species (cyncri, leuvul, tripra, sildio) single model was best because 2 best fits were a trait with and without log. Therefore, we made the rule that all species have a 2-trait model. For the four species mentioned, the third best trait was used. 
The models: 
  m_sucpra <- lmerTest::lmer(log_biomass ~ log_no_stems + log_number_leaves + (1 | block_ID), data = df_sucpra)
m_cennig <- lmerTest::lmer(log_biomass ~ log_height_reproductive_str * log_no_stems + (1 | block_ID), data = df_cennig)
m_pimsax <- lmerTest::lmer(log_biomass ~ log_number_leaves + log_no_stems + (1 | block_ID), data = df_pimsax)
m_luzmul <- lmerTest::lmer(log_biomass ~ log_number_leaves * log_no_stems + (1 | block_ID), data = df_luzmul)
m_leuvul <- lmerTest::lmer(log_biomass ~ log_no_stems + log_number_leaves + (1 | block_ID), data = df_leuvul)
m_tripra <- lmerTest::lmer(log_biomass ~ log_height_reproductive_str + log_no_stems + (1 | block_ID), data = df_tripra)
m_hypmac <- lmerTest::lmer(log_biomass ~ log_number_leaves + log_no_stems + (1 | block_ID), data = df_hypmac)
m_plalan <- lmerTest::lmer(log_biomass ~ log_height_vegetative_str + log_height_reproductive_str + (1 | block_ID), data = df_plalan)
m_cyncri <- lmerTest::lmer(log_biomass ~ log_no_stems  * log_number_leaves + (1 | block_ID), data = df_cyncri)
m_sildio <- lmerTest::lmer(log_biomass ~ log_number_leaves  * log_height_reproductive_str + (1 | block_ID), data = df_sildio)

Some control plotting: Biomass_traits_expl_plots_species.R

4. Compare general model with species models
Script: Biomass_prediction_for_24.R
Use: analysis_data_24_log
Predict biomass with both methods and compare.
These figures are with the correctly imported biomass data:
 Species models split per species: (old figure with cyncri clusters in Phenology_Biomass_07.03.26.ppt)



For 7/10 species the species model was a better fit then the general model. 
Log(biomass24)_log(pred_biomass24_species_seperated).png
	Continue with species models

5. Predict biomass for 2023
Script: Biomass_prediction_for_23_per_species.R
Old: Biomass_prediction_for_23.R where this was done with the general model
Create dataset per species and use species specific models to predict biomass. 
	bio_pred_23_species is all combined

6. Combine predicted biomass 23 with phenology 23
Script: Biomass_phenology_combine_species_models.R
Old: Biomass_phenology_combine.R (general model)
Do bigger plants produce more flowers? Yes
m_flowers_species <- lmerTest::lmer(pred_log_biomass_species ~ log_max_flower + (1|species) + (1|block_ID), data = bio_flower_species)
Log_pred_biomass_log_max_flowers_lmm_species_models_color.png

7. Test effect of cooling on flower number adjusted for biomass (allocation)
Script: Biomass_Cooling_flower_number_glmer.nb_species_bio_09.03.26.R
Old: Biomass_Cooling_flower_number_glmer.nb_06.03.26.R (general model) and Biomass_Cooling_flower_number_04.03.26.R (lmer)
Old scripts are needed because new script is sourcing them, also to compare models. Should be updated at some point. 
Now also low site is the reference. 

Final plot: p_bio_nb_species = Cooling_competition_flower_number_NOR_adjusted_biomass_species_models_glmer.nb_3.png
	Cooling through upslope transplantation leads to significantly more flowers when plants are in native vegetation
	But not when neighbouring plants have been removed
	Biotic interactions with neighbours decrease the number of flowers significantly at the low site but not at the high site


