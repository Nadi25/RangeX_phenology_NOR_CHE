

# BIOMASS 4 ---------------------------------------------------------------

# RangeX biomass - traits plots per species ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Biomass and trait regressions plots species model based



source("Biomass_traits_correlation_per_species.R.R")

library(ggeffects)
theme_set(theme_bw(base_size = 12))



# List of species and corresponding datasets/models
species_list <- c("sucpra","cennig","pimsax","luzmul","leuvul","tripra","hypmac","plalan","cyncri","sildio")
df_list <- lapply(species_list, function(sp) analysis_data_24_log |> filter(species == sp))
names(df_list) <- species_list

model_list <- list(
  sucpra = m_sucpra,
  cennig = m_cennig,
  pimsax = m_pimsax,
  luzmul = m_luzmul,
  leuvul = m_leuvul,
  tripra = m_tripra,
  hypmac = m_hypmac,
  plalan = m_plalan,
  cyncri = m_cyncri,
  sildio = m_sildio
)

# Generate plots
plots <- list()

for(sp in species_list){
  
  df_sp <- df_list[[sp]]
  model_sp <- model_list[[sp]]
  
  # get fixed-effect predictors
  pred_vars <- all.vars(formula(model_sp))[-1]
  pred_vars <- pred_vars[!grepl("\\|", pred_vars)]
  
  # x variable
  x_var <- pred_vars[1]
  
  # terms for ggpredict
  if(length(pred_vars) > 1){
    terms_for_pred <- c(x_var, paste0(pred_vars[-1], " [quantile]"))
  } else {
    terms_for_pred <- x_var
  }
  
  # predict only fixed effects
  pred_df <- ggpredict(model_sp, terms = terms_for_pred, type = "fixed")
  
  #or use only first predictor
  # terms_for_pred <- x_var
  # pred_df <- ggpredict(model_sp, terms = terms_for_pred, type = "fixed")
  
  # plot
  if(length(pred_vars) == 1){
    p <- ggplot(df_sp, aes_string(x = x_var, y = "log_biomass")) +
      geom_point(alpha = 0.3, color = "grey50") +
      geom_line(data = pred_df, aes(x = x, y = predicted), size = 1.2, color = "steelblue") +
      geom_ribbon(data = pred_df, aes(x = x, ymin = conf.low, ymax = conf.high),
                  fill = "steelblue", alpha = 0.15) +
      labs(title = sp, x = x_var, y = "log(biomass)") +
      theme_minimal()
  } else {
    p <- ggplot(df_sp, aes_string(x = x_var, y = "log_biomass")) +
      geom_point(alpha = 0.3, color = "grey50") +
      geom_line(data = pred_df, aes(x = x, y = predicted, color = group), size = 1.2) +
      geom_ribbon(data = pred_df, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                  inherit.aes = FALSE, alpha = 0.15) +  # <-- critical
      labs(title = sp, x = x_var, y = "log(biomass)", 
           color = paste(pred_vars[-1], collapse = ", "), 
           fill  = paste(pred_vars[-1], collapse = ", "))
  }
  
  plots[[sp]] <- p
}

# example
plots[["sucpra"]]
plots[["pimsax"]]


# optionally, save all plots in a PDF
pdf("Output/Biomass/Species_biomass_traits_relations.pdf", width = 7, height = 5)
for(sp in species_list){
  print(plots[[sp]])
}
dev.off()
















