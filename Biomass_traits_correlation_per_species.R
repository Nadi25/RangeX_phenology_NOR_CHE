
# BIOMASS 3 ---------------------------------------------------------------

# Fit individual models per species ---------------------------------------

source("Biomass_traits_correlation_NOR.R")


# log transform the traits ------------------------------------------------
analysis_data_24_log <- biomass_traits |>
  select(
    treat_warming, treat_competition, site,
    log_biomass,
    height_vegetative,
    height_vegetative_str,
    height_reproductive,
    height_reproductive_str,
    no_stems,
    number_leaves,
    species,
    block_ID
  ) |>
  mutate(
    log_height_vegetative       = log1p(height_vegetative),
    log_height_vegetative_str   = log1p(height_vegetative_str),
    log_height_reproductive     = log1p(height_reproductive),
    log_height_reproductive_str = log1p(height_reproductive_str),
    log_no_stems                = log1p(no_stems),
    log_number_leaves           = log1p(number_leaves)
  ) |>
  na.omit()


# split data by species ---------------------------------------------------
# use analysis_data because it has only rows that have the traits
data_species <- analysis_data_24_log |> 
  group_split(species)


# function with all model options -----------------------------------------
fit_models <- function(df){
  
  # vegetative height
  m_height_v <- lmerTest::lmer(
    log_biomass ~ height_vegetative + (1|block_ID),
    data = df)
  
  m_height_v_log <- lmerTest::lmer(
    log_biomass ~ log_height_vegetative + (1|block_ID),
    data = df)
  
  # stretched vegetative height
  m_height_vs <- lmerTest::lmer(
    log_biomass ~ height_vegetative_str + (1|block_ID),
    data = df)
  
  m_height_vs_log <- lmerTest::lmer(
    log_biomass ~ log_height_vegetative_str + (1|block_ID),
    data = df)
  
  # reproductive height
  m_height_r <- lmerTest::lmer(
    log_biomass ~ height_reproductive + (1|block_ID),
    data = df)
  
  m_height_r_log <- lmerTest::lmer(
    log_biomass ~ log_height_reproductive + (1|block_ID),
    data = df)
  
  # stretched reproductive height
  m_height_rs <- lmerTest::lmer(
    log_biomass ~ height_reproductive_str + (1|block_ID),
    data = df)
  
  m_height_rs_log <- lmerTest::lmer(
    log_biomass ~ log_height_reproductive_str + (1|block_ID),
    data = df)
  
  # stems
  m_stems <- lmerTest::lmer(
    log_biomass ~ no_stems + (1|block_ID),
    data = df)
  
  m_stems_log <- lmerTest::lmer(
    log_biomass ~ log_no_stems + (1|block_ID),
    data = df)
  
  # leaves
  m_leaves <- lmerTest::lmer(
    log_biomass ~ number_leaves + (1|block_ID),
    data = df)
  
  m_leaves_log <- lmerTest::lmer(
    log_biomass ~ log_number_leaves + (1|block_ID),
    data = df)
  
  # AIC comparison
  AIC(
    m_height_v,
    m_height_v_log,
    m_height_vs,
    m_height_vs_log,
    m_height_r,
    m_height_r_log,
    m_height_rs,
    m_height_rs_log,
    m_stems,
    m_stems_log,
    m_leaves,
    m_leaves_log
  )
}


# run the omdels for all species ------------------------------------------
results <- lapply(data_species, fit_models)




# attach species names ----------------------------------------------------
species_names <- analysis_data_24_log |> 
  distinct(species) |> 
  pull()

names(results) <- species_names


# inspect results for all species -----------------------------------------
results[["sucpra"]]
results[["cennig"]]
results[["pimsax"]]
results[["luzmul"]]
results[["leuvul"]]
results[["tripra"]]
results[["hypmac"]]
results[["plalan"]]
results[["cyncri"]]
results[["sildio"]]



# overview of the best model per species ----------------------------------
best_models <- do.call(rbind, lapply(names(results), function(sp){
  
  x <- results[[sp]]
  best <- x[which.min(x$AIC), ]
  
  data.frame(
    species = sp,
    model   = rownames(best),
    AIC     = best$AIC
  )
}))

best_models

# species           model       AIC
# 1   sucpra     m_stems_log 212.85085
# 2   cennig m_height_rs_log 212.55040
# 3   pimsax    m_leaves_log 261.83000
# 4   luzmul    m_leaves_log 482.49088
# 5   leuvul     m_stems_log 355.26236
# 6   tripra m_height_rs_log 226.53698
# 7   hypmac    m_leaves_log 351.22768
# 8   plalan m_height_vs_log  15.15047
# 9   cyncri     m_stems_log 130.22105
# 10  sildio    m_leaves_log 345.35538


# this list the AIC for all models per species
# 0 is the best model
results_delta <- lapply(results, function(x){
  x$deltaAIC <- x$AIC - min(x$AIC)
  x[order(x$deltaAIC), ]
})
results_delta



# test if certain trait combinations are better than single traits --------
# use the two best single trait models for this

fit_top2_models <- function(species_name, data, results_delta){
  
  # filter dataset for the species
  df <- data |> filter(species == species_name)
  
  # get the top 2 traits (lowest deltaAIC)
  top2 <- results_delta[[species_name]] |> 
    slice_head(n = 2) |> 
    rownames()  # model names, e.g., "m_stems_log", "m_leaves_log"
  
  # map model names to actual variable names in the dataset
  trait_map <- c(
    m_height_v       = "height_vegetative",
    m_height_v_log   = "log_height_vegetative",
    m_height_vs      = "height_vegetative_str",
    m_height_vs_log  = "log_height_vegetative_str",
    m_height_r       = "height_reproductive",
    m_height_r_log   = "log_height_reproductive",
    m_height_rs      = "height_reproductive_str",
    m_height_rs_log  = "log_height_reproductive_str",
    m_stems          = "no_stems",
    m_stems_log      = "log_no_stems",
    m_leaves         = "number_leaves",
    m_leaves_log     = "log_number_leaves"
  )
  
  traits <- trait_map[top2]
  
  # store models in a list
  model_list <- list()
  
  # single best trait
  m_single <- lmer(as.formula(paste("log_biomass ~", traits[1], "+ (1|block_ID)")), data=df)
  model_list[["single"]] <- m_single
  
  # additive model (if 2 traits)
  if(length(traits) == 2){
    m_add <- lmer(as.formula(paste("log_biomass ~", paste(traits, collapse = " + "), "+ (1|block_ID)")), data=df)
    m_int <- lmer(as.formula(paste("log_biomass ~", paste(traits, collapse = " * "), "+ (1|block_ID)")), data=df)
    model_list[["additive"]] <- m_add
    model_list[["interaction"]] <- m_int
  }
  
  # return list of fitted models
  return(model_list)
}


top_models_species <- lapply(species_names, function(sp){
  fit_top2_models(sp, analysis_data_24_log, results_delta)
})

names(top_models_species) <- species_names



# sucpra ------------------------------------------------------------------
# single-trait model for sucpra
summary(top_models_species[["sucpra"]][["single"]])

# additive model for sucpra
summary(top_models_species[["sucpra"]][["additive"]])

# interaction model for sucpra
summary(top_models_species[["sucpra"]][["interaction"]])

# which model is best
AIC(
  top_models_species[["sucpra"]][["single"]],
  top_models_species[["sucpra"]][["additive"]],
  top_models_species[["sucpra"]][["interaction"]]
)

# top_models_species[["sucpra"]][["single"]]       4 130.2210
# top_models_species[["sucpra"]][["additive"]]     5 120.8155  ## best 
# top_models_species[["sucpra"]][["interaction"]]  6 124.3117

# final model for sucpra --------------------------------------------------
# dataset for sucpra
df_sucpra <- analysis_data_24_log |> filter(species == "sucpra")

# final model
m_sucpra <- lmerTest::lmer(log_biomass ~ log_no_stems + log_number_leaves + (1 | block_ID),
                 data = df_sucpra)
summary(m_sucpra)

# cennig ------------------------------------------------------------------
# single-trait model for cennig
summary(top_models_species[["cennig"]][["single"]])

# additive model for cennig
summary(top_models_species[["cennig"]][["additive"]])

# interaction model for cennig
summary(top_models_species[["cennig"]][["interaction"]])

# which model is best
AIC(
  top_models_species[["cennig"]][["single"]],
  top_models_species[["cennig"]][["additive"]],
  top_models_species[["cennig"]][["interaction"]]
)

# top_models_species[["cennig"]][["single"]]       4 463.8535
# top_models_species[["cennig"]][["additive"]]     5 151.0341
# top_models_species[["cennig"]][["interaction"]]  6 142.3000  ## best

# final model for cennig --------------------------------------------------
# dataset for cennig
df_cennig <- analysis_data_24_log |> filter(species == "cennig")

# final model
m_cennig <- lmerTest::lmer(log_biomass ~ log_height_reproductive_str * log_no_stems + 
                             (1 | block_ID), data = df_cennig)
summary(m_cennig)


# pimsax ------------------------------------------------------------------
# single-trait model for pimsax
summary(top_models_species[["pimsax"]][["single"]])

# additive model for pimsax
summary(top_models_species[["pimsax"]][["additive"]])

# interaction model for pimsax
summary(top_models_species[["pimsax"]][["interaction"]])

# which model is best
AIC(
  top_models_species[["pimsax"]][["single"]],
  top_models_species[["pimsax"]][["additive"]],
  top_models_species[["pimsax"]][["interaction"]]
)

# top_models_species[["pimsax"]][["single"]]       4 253.1334
# top_models_species[["pimsax"]][["additive"]]     5 238.4411  ## best
# top_models_species[["pimsax"]][["interaction"]]  6 240.7304

# final model for pimsax --------------------------------------------------
# dataset for pimsax
df_pimsax <- analysis_data_24_log |> filter(species == "pimsax")

# final model
m_pimsax <- lmerTest::lmer(log_biomass ~ log_number_leaves + log_no_stems + 
                             (1 | block_ID), data = df_pimsax)
summary(m_pimsax)


# luzmul ------------------------------------------------------------------
df_luzmul <- analysis_data_24_log |> filter(species == "luzmul")

# single-trait model
summary(top_models_species[["luzmul"]][["single"]])

# additive model
summary(top_models_species[["luzmul"]][["additive"]])

# interaction model
summary(top_models_species[["luzmul"]][["interaction"]])

# compare models
AIC(
  top_models_species[["luzmul"]][["single"]],
  top_models_species[["luzmul"]][["additive"]],
  top_models_species[["luzmul"]][["interaction"]]
)

# top_models_species[["luzmul"]][["single"]]       4 433.8694
# top_models_species[["luzmul"]][["additive"]]     5 348.5358
# top_models_species[["luzmul"]][["interaction"]]  6 346.2326  ## best

# final model for luzmul --------------------------------------------------
m_luzmul <- lmerTest::lmer(
  log_biomass ~ log_number_leaves * log_no_stems + (1 | block_ID),
  data = df_luzmul
)
summary(m_luzmul)

# leuvul ------------------------------------------------------------------
df_leuvul <- analysis_data_24_log |> filter(species == "leuvul")

summary(top_models_species[["leuvul"]][["single"]])
summary(top_models_species[["leuvul"]][["additive"]])
summary(top_models_species[["leuvul"]][["interaction"]])

AIC(
  top_models_species[["leuvul"]][["single"]],
  top_models_species[["leuvul"]][["additive"]],
  top_models_species[["leuvul"]][["interaction"]]
)

# top_models_species[["leuvul"]][["single"]]       4 493.5125  ## best
# top_models_species[["leuvul"]][["additive"]]     5 501.2501
# top_models_species[["leuvul"]][["interaction"]]  6 506.1067

# final model for leuvul --------------------------------------------------
m_leuvul <- lmerTest::lmer(
  log_biomass ~ log_no_stems + (1 | block_ID),
  data = df_leuvul
)
summary(m_leuvul)

# tripra ------------------------------------------------------------------
df_tripra <- analysis_data_24_log |> filter(species == "tripra")

summary(top_models_species[["tripra"]][["single"]])
summary(top_models_species[["tripra"]][["additive"]])
summary(top_models_species[["tripra"]][["interaction"]])

AIC(
  top_models_species[["tripra"]][["single"]],
  top_models_species[["tripra"]][["additive"]],
  top_models_species[["tripra"]][["interaction"]]
)

# top_models_species[["tripra"]][["single"]]       4 381.9630  ## best
# top_models_species[["tripra"]][["additive"]]     5 387.1187
# top_models_species[["tripra"]][["interaction"]]  6 391.1435

# final model for tripra --------------------------------------------------
m_tripra <- lmerTest::lmer(
  log_biomass ~ log_height_reproductive_str  + (1 | block_ID),
  data = df_tripra
)
summary(m_tripra)


# hypmac ------------------------------------------------------------------
df_hypmac <- analysis_data_24_log |> filter(species == "hypmac")

summary(top_models_species[["hypmac"]][["single"]])
summary(top_models_species[["hypmac"]][["additive"]])
summary(top_models_species[["hypmac"]][["interaction"]])

AIC(
  top_models_species[["hypmac"]][["single"]],
  top_models_species[["hypmac"]][["additive"]],
  top_models_species[["hypmac"]][["interaction"]]
)

# top_models_species[["hypmac"]][["single"]]       4 261.8300
# top_models_species[["hypmac"]][["additive"]]     5 261.6323  ## best
# top_models_species[["hypmac"]][["interaction"]]  6 263.7201

# final model for hypmac --------------------------------------------------
m_hypmac <- lmerTest::lmer(
  log_biomass ~ log_number_leaves + log_no_stems + (1 | block_ID),
  data = df_hypmac
)
summary(m_hypmac)

# plalan ------------------------------------------------------------------
df_plalan <- analysis_data_24_log |> filter(species == "plalan")

summary(top_models_species[["plalan"]][["single"]])
summary(top_models_species[["plalan"]][["additive"]])
summary(top_models_species[["plalan"]][["interaction"]])

AIC(
  top_models_species[["plalan"]][["single"]],
  top_models_species[["plalan"]][["additive"]],
  top_models_species[["plalan"]][["interaction"]]
)

# top_models_species[["plalan"]][["single"]]       4 403.5728
# top_models_species[["plalan"]][["additive"]]     5 402.1336  ## best
# top_models_species[["plalan"]][["interaction"]]  6 403.0608

# final model for plalan --------------------------------------------------
m_plalan <- lmerTest::lmer(
  log_biomass ~ log_height_vegetative_str + log_height_reproductive_str + (1 | block_ID),
  data = df_plalan
)
summary(m_plalan)

# cyncri ------------------------------------------------------------------
df_cyncri <- analysis_data_24_log |> filter(species == "cyncri")

summary(top_models_species[["cyncri"]][["single"]])
summary(top_models_species[["cyncri"]][["additive"]])
summary(top_models_species[["cyncri"]][["interaction"]])

AIC(
  top_models_species[["cyncri"]][["single"]],
  top_models_species[["cyncri"]][["additive"]],
  top_models_species[["cyncri"]][["interaction"]]
)

# top_models_species[["cyncri"]][["single"]]       4 213.8145  ## best
# top_models_species[["cyncri"]][["additive"]]     5 216.3082
# top_models_species[["cyncri"]][["interaction"]]  6 214.3393

# final model for cyncri --------------------------------------------------
m_cyncri <- lmerTest::lmer(
  log_biomass ~ log_no_stems + (1 | block_ID),
  data = df_cyncri
)
summary(m_cyncri)

# sildio ------------------------------------------------------------------
df_sildio <- analysis_data_24_log |> filter(species == "sildio")

summary(top_models_species[["sildio"]][["single"]])
summary(top_models_species[["sildio"]][["additive"]])
summary(top_models_species[["sildio"]][["interaction"]])

AIC(
  top_models_species[["sildio"]][["single"]],
  top_models_species[["sildio"]][["additive"]],
  top_models_species[["sildio"]][["interaction"]]
)

# top_models_species[["sildio"]][["single"]]       4 17.92125
# top_models_species[["sildio"]][["additive"]]     4 17.92125
# top_models_species[["sildio"]][["interaction"]]  4 17.92125

# final model for sildio --------------------------------------------------
m_sildio <- lmerTest::lmer(
  log_biomass ~ log_number_leaves + (1 | block_ID),
  data = df_sildio
)
summary(m_sildio)













