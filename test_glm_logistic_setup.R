# Notes from Bigelow et al. 2024

# ?glm
# 
# glm(, family = "binomial")
# 
# ?anova.glm(..., test = "LRT")
# 
# AIC
# AUC
# Hosmer-Lemeshow

#####

library(tidyverse)
library(data.table)
library(ResourceSelection) # hoslem.test
library(rms) # lrm

tree_vars <- fread('/Users/dlm356/Library/CloudStorage/Box-Box/Katz lab/NYC/tree_mortality_variables/all_variables.csv')

# Convert LandUse from integer to character for unique labels
appendCharLU <- function(lu){
  if (is.na(lu)){
    lu <- paste0("LU_", lu)
  } else {
    if(nchar(as.character(lu)) < 2){
      lu <- paste0("0", lu)
    }
    lu <- paste0("LU_", lu)
  }
  return(lu)
}

#sapply(sp_sub$LandUse, appendCharLU)

# sp_sub <- tree_vars %>% 
#   filter(species == 'Acer platanoides' & canopy_change %in% c(1, 3)) %>%
#   mutate(canopy_endstate = (canopy_change - 1)/2, # convert 1 and 3 to 0 and 1, where 0 is alive and 1 is dead; should this be flipped for survival??
#         LandUse_char = sapply(LandUse, appendCharLU))

sp_list <- c("Platanus x acerifolia",
             "Gleditsia triacanthos",
             "Pyrus calleryana",
             "Quercus palustris",
             "Acer platanoides", 
             "Tilia cordata")

for(i in 1:length(sp_list)){
  
  sp <- sp_list[i]
  print(sp)
  
  sp_sub <- tree_vars %>% 
    filter(species == sp & canopy_change %in% c(1, 3)) %>%
    mutate(canopy_endstate = (canopy_change - 3)*(-1/2), # convert 1 and 3 to 1 and 0, where 1 is alive and 0 is dead. This is because the models in the literature are focused on likelihood of survival
           LandUse_char = sapply(LandUse, appendCharLU))
  
  sp_sub <- sp_sub %>% 
    filter(LandUse_char != "LU_NA")
  
  #sp_sub$LandUse_char <- relevel(as.factor(sp_sub$LandUse_char), "LU_NA") # make NA the default level, is this the best approach?
  sp_sub$LandUse_char <- relevel(as.factor(sp_sub$LandUse_char), "LU_09") # "LU09_Open_Space_and_Outdoor_Recreation" is base case
  
  #####
  # Can prefilter for pluto_dist and xis_dist if needed
  
  # Format input df as needed
  sp_sub_format <- sp_sub %>% 
    mutate(BldgClass_fac = factor(BldgClass), 
           BldgClass_Group_fac = factor(BldgClass_Group),
           in_sandy_zone_bool = as.logical(in_sandy_zone),
           is_B_cons_bool = as.logical(is_B_cons),
           is_S_cons_bool = as.logical(is_S_cons),
           is_DM_bool = as.logical(is_DM)) %>%
    select(canopy_endstate, # dependent variable - does this need to be factor??
           tree_dbh, steward_level, # tree diameter at breast height, number of signs of stewardship
           BldgClass_fac, BldgClass_Group_fac, LandUse_char, # building type (detailed), building type (high level), land use
           in_sandy_zone_bool, # sandy inundation zone
           is_B_cons_bool, is_S_cons_bool, is_DM_bool, # building construction, street construction, building demolition
           summer_max, summer_min, summer_mean, days_max_27, days_max_32, days_max_35, # temperature
           TC_10_diff, GS_10_diff, SO_10_diff, WA_10_diff, BD_10_diff, RD_10_diff, OI_10_diff, RR_10_diff, # land cover diff, doing 10 m
           TC_10_2017, GS_10_2017, SO_10_2017, WA_10_2017, BD_10_2017, RD_10_2017, OI_10_2017, RR_10_2017, # land cover 2017, doing 10 m
           TC_10_2021, GS_10_2021, SO_10_2021, WA_10_2021, BD_10_2021, RD_10_2021, OI_10_2021, RR_10_2021) # land cover 2021, doing 10 m
  
  # Subset and organize similar to the variables we would want based on 
  sp_sub_format <- sp_sub_format %>% mutate(imp_10_2021 = BD_10_2017 + RD_10_2017 + OI_10_2017 + RR_10_2017) # building + road + other impervious + railroad
  
  # null_model <- glm( canopy_endstate ~ 1,
  #                    family = binomial(link = "logit"),
  #                    data = sp_sub_format)
  # 
  # mort_model_acpl <- glm( canopy_endstate ~ imp_10_2021 + tree_dbh + is_B_cons_bool,
  #                    family = binomial(link = "logit"),
  #                    data = sp_sub_format)
  # 
  # glm( canopy_endstate ~ imp_10_2021 + tree_dbh + is_DM_bool, #is_B_cons_bool,
  #      family = binomial(link = "logit"),
  #      data = sp_sub_format) %>% summary()
  # # is_DM_bool has slightly smaller p value (0.11) than is_B_cons_bool (0.17)
  # 
  # glm( canopy_endstate ~ imp_10_2021 + tree_dbh + is_DM_bool + steward_level,
  #      family = binomial(link = "logit"),
  #      data = sp_sub_format) %>% summary()
  # # steward level is highly significant too
  # 
  # glm( canopy_endstate ~ imp_10_2021 + tree_dbh + is_DM_bool + steward_level + LandUse_char,
  #      family = binomial(link = "logit"),
  #      data = sp_sub_format) %>% summary()
  # # sig land use classes, 04 mixed residential & commercial, 05 commercial and office buildings
  # # almost sig (<0.10): 01 one and two family buildings, 06 industrial and manufacturing, 08 public facilities & institutions 
  # 
  # sp_sub_format$canopy_endstate %>% table()
  
  mort_model <- glm( canopy_endstate ~ imp_10_2021 + tree_dbh + is_DM_bool + steward_level + in_sandy_zone_bool + LandUse_char,
                     family = binomial(link = "logit"),
                     #family = binomial(link = "cloglog"),
                     data = sp_sub_format)
  
  mort_model_summary <- summary(mort_model)
  
  coef <- data.frame(mort_model_summary$coefficients)
  coef$OR <- exp(coef$Estimate)
  ci <- confint(mort_model)
  ci_or <- exp(ci)
  
  coef <- cbind.data.frame(coef, ci_or)
  colnames(coef) <- c("beta", "se", "Z", "p_value", "OR", "lower_95", "upper_95")
  coef$vars <- rownames(coef)
  # coef$vars_2 <- c("Intercept", "Impervious_10m_Pct", "DBH", "Demolition", "Stewardship_Level", "In_Sandy_Zone",
  #                  "LU01_One_and_Two_Family_Buildings", "LU02_MultiFamily_WalkUp_Buildings", "LU03_MultiFamily_Elevator_Buildings", "LU04_Mixed_Residential_Commercial_Buildings",
  #                  "LU05_Commercial_and_Office_Buildings", "LU06_Industrial_and_Manufacturing", "LU07_Transportation_and_Utility", "LU08_Public_Facilities_and_Institutions",
  #                  "LU09_Open_Space_and_Outdoor_Recreation", "LU10_Parking_Facilities", "LU11_Vacant_Land")
  coef$vars_2 <- c("Intercept", "Impervious_10m_Pct", "DBH", "Demolition", "Stewardship_Level", "In_Sandy_Zone",
                   "LU01_One_and_Two_Family_Buildings", "LU02_MultiFamily_WalkUp_Buildings", "LU03_MultiFamily_Elevator_Buildings", "LU04_Mixed_Residential_Commercial_Buildings",
                   "LU05_Commercial_and_Office_Buildings", "LU06_Industrial_and_Manufacturing", "LU07_Transportation_and_Utility", "LU08_Public_Facilities_and_Institutions",
                    "LU10_Parking_Facilities", "LU11_Vacant_Land") # "LU09_Open_Space_and_Outdoor_Recreation" is base case
  coef$sig <- FALSE
  coef$sig[which(coef$p_value < 0.05)] <- TRUE
  coef$direction <- "Insignificant"
  coef$direction[which(coef$sig & coef$OR > 1)] <- "Positive"
  coef$direction[which(coef$sig & coef$OR < 1)] <- "Negative"
  coef$species <- sp
  if (i == 1) {
    coef_all <- coef
  } else {
    coef_all <- rbind.data.frame(coef_all, coef)
  }
}

coef_all_plotting <- coef_all %>% filter(vars_2 != "Intercept")

coef_all_plotting %>%
  ggplot(aes(color = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(aes(x = OR, y = vars_2), size = 2) +
  geom_linerange(aes(xmin = lower_95, xmax = upper_95, y = vars_2)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("gray80", "red", "blue")) +
  facet_wrap(~species) +
  labs(x = "Odds Ratio", y = "", color = "") +
  theme_bw() +
  theme(strip.text = element_text(face = "italic"))

ggsave("/Users/dlm356/dlm356_files/nyc_tree_mortality/figures/logistic_regression_odds_ratio_by_species_example_logit.jpg",
      width = 12, height = 7, units = "in")

# ggsave("/Users/dlm356/dlm356_files/nyc_tree_mortality/figures/logistic_regression_odds_ratio_by_species_example_cloglog.jpg",
#        width = 12, height = 7, units = "in")

# Need to get model fit parameters for these as well
# Record: AIC, AUC, Hosmer-Lemeshow

#####
# Breakdown of the dataset by species and genus, with mortality rates
sp_change_counts <- tree_vars %>% 
  group_by(species, canopy_change) %>% 
  dplyr::summarize(sp_cc_counts = length(canopy_change)) %>% 
  pivot_wider(id_cols = species, names_from = canopy_change, values_from = sp_cc_counts)
sp_change_counts <- sp_change_counts %>% 
  mutate(total_alive_dead = `1` + `3`,
         pct_alive = round(`1`/(`1` + `3`)*100,4))

sp_change_counts <- sp_change_counts[order(sp_change_counts$total_alive_dead, decreasing = TRUE),] %>% drop_na()

library(ggrepel)

ggplot() +
  geom_point(data = sp_change_counts, aes(x = total_alive_dead, y = pct_alive), color = "gray80") +
  geom_point(data = sp_change_counts[1:12,], aes(x = total_alive_dead, y = pct_alive, color = species)) +
  geom_text_repel(data = sp_change_counts[1:12,], aes(x = total_alive_dead, y = pct_alive, color = species, label = species)) +
  labs(x = "Total Alive + Dead", y = "Survival %") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("/Users/dlm356/dlm356_files/nyc_tree_mortality/figures/top_species_survival_counts.jpg",
       width = 6, height = 5, units = "in")
