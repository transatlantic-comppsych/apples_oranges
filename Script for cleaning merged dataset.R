# create and clean new merged dataset
library(readr)
library(tidyverse)

df_full_psych <- read_csv("Full Psychotherapy Dataset.csv", col_types = cols(year = col_character()))
df_full_med <- read.csv("Full Medication Dataset.csv")

# merge datasets
merged_dataset <- bind_rows(df_full_med, df_full_psych)

# fix column ordering
merged_dataset <- select(merged_dataset, -c(primary, hierarchy, time))
merged_dataset <- merged_dataset %>% relocate(descr_active, .after = control_type)
merged_dataset <- merged_dataset %>% relocate(descr_control, .after = descr_active)
merged_dataset <- merged_dataset %>% relocate(mean_age, .after = age_group)
merged_dataset <- merged_dataset %>% relocate(percent_women, .after = control_mean_age)
merged_dataset <- merged_dataset %>% relocate(responders_active: cuij_responders_control, .after = observed_resp_rate_control)

#create vector indicating instrument primacy according to our hierarchy
unique(merged_dataset$instrument) # check unique values to account for differences in formatting of instrument names
merged_dataset <- merged_dataset %>%
  mutate(
    instrument_value = case_when(
      instrument %in% c("CDRS", "CDRS-R", "cdrs-r") ~ 1,
      instrument %in% c("HAM-D", "ham-d") ~ 2,
      instrument %in% c("MADRS") ~ 3,
      instrument %in% c("BDI", "bdi", "bdi-ii") ~ 4,
      instrument %in% c("cdi") ~ 5,
      instrument %in% c("K-SADS-L", "K-SADS", "K-SADS (adapted)") ~ 6,
      instrument %in% c("mfq", "smfq") ~ 7,
      instrument %in% c("RADS", "rads") ~ 8,
      instrument %in% c("bid") ~ 9,
      instrument %in% c("CDS") ~ 10,
      instrument %in% c("ces-d") ~ 11,
      instrument %in% c("CAS") ~ 12,
      instrument %in% c("cbcl-d") ~ 13,
      TRUE ~ 99 # Default case, if none of the above conditions match
    )
  )
merged_dataset <- merged_dataset %>% relocate(instrument_value, .after = instrument)

# create an instrument name variable
merged_dataset <- merged_dataset %>%
  mutate(
    instrument_name = case_when(
      instrument %in% c("CDRS", "CDRS-R", "cdrs-r") ~ "cdrs",
      instrument %in% c("HAM-D", "ham-d") ~ "hamd",
      instrument %in% c("MADRS") ~ "madrs",
      instrument %in% c("BDI", "bdi", "bdi-ii") ~ "bdi",
      instrument %in% c("cdi") ~ "cdi",
      instrument %in% c("K-SADS-L", "K-SADS", "K-SADS (adapted)") ~ "ksads",
      instrument %in% c("mfq", "smfq") ~ "mfq",
      instrument %in% c("RADS", "rads") ~ "rads",
      instrument %in% c("bid") ~ "bid",
      instrument %in% c("CDS") ~ "cds",
      instrument %in% c("ces-d") ~ "ces_d",
      instrument %in% c("CAS") ~ "cas",
      instrument %in% c("cbcl-d") ~ "cbcl_d",
      # TRUE ~ 99 # Default case, if none of the above conditions match
    )
  )
merged_dataset <- merged_dataset %>% relocate(instrument_name, .after = instrument)

# create response rate variables
# first create our primary response rate variable, which is estimated number of responders / n at randomisation
resp_rate_active <- (merged_dataset$responders_active)/(merged_dataset$baseline_n_active)
resp_rate_control <- (merged_dataset$responders_control)/(merged_dataset$baseline_n_control)

#then calculate response rate using n at post-test (i.e. completers only) 
resp_rate_active_completers <- (merged_dataset$responders_active)/(merged_dataset$post_n_active)
resp_rate_control_completers <- (merged_dataset$responders_control)/(merged_dataset$post_n_control)

merged_dataset <- cbind(merged_dataset, resp_rate_active, resp_rate_control, resp_rate_active_completers, resp_rate_control_completers)
merged_dataset <- merged_dataset %>% relocate(resp_rate_active: resp_rate_control_completers, .after = responders_control)

# merged_dataset <- merged_dataset %>%
#   mutate(
#     resp_rate_active = round(resp_rate_active, 4),
#     resp_rate_control = round(resp_rate_control, 4),
#     resp_rate_active_completers = round(resp_rate_active_completers, 4),
#     resp_rate_control_completers = round(resp_rate_control_completers, 4)
#   )

#calculate response rate for studies that reported no of responders
calc_observed_resp_rate_active <- (merged_dataset$observed_responders_active)/(merged_dataset$observed_responders_active_n)
calc_observed_resp_rate_control <- (merged_dataset$observed_responders_control)/(merged_dataset$observed_responders_control_n)
merged_dataset$observed_resp_rate_active <- coalesce(calc_observed_resp_rate_active, merged_dataset$observed_resp_rate_active)
merged_dataset$observed_resp_rate_control <- coalesce(calc_observed_resp_rate_control, merged_dataset$observed_resp_rate_control)

#plot and check correlation between our calculations and observed response rate, for active condition
plot(merged_dataset$resp_rate_active, merged_dataset$observed_resp_rate_active, 
     xlab = "Response rates calculated by us", ylab = "Observed response rates",
     main = "Comparison of estimated vs observed response rates for active arm of medication trials",
     pch = 19, col = "blue")
cor(merged_dataset$resp_rate_active, merged_dataset$observed_resp_rate_active, method = "pearson", use = "pairwise.complete.obs")

#plot and check correlation between our calculations and observed response rate, for control condition
plot(merged_dataset$resp_rate_control, merged_dataset$observed_resp_rate_control, 
     xlab = "Response rates calculated by us", ylab = "Observed response rates",
     main = "Comparison of estimated vs observed response rates for control arm of medication trials",
     pch = 19, col = "blue")
cor(merged_dataset$resp_rate_control, merged_dataset$observed_resp_rate_control, method = "pearson", use = "pairwise.complete.obs")

#check correlation between our calculations and cuijpers' calculations 
cor(merged_dataset$responders_active, merged_dataset$cuij_responders_active, method = "pearson", use = "pairwise.complete.obs")
cor(merged_dataset$responders_control, merged_dataset$cuij_responders_control, method = "pearson", use = "pairwise.complete.obs")

#check plots
# for active arm
plot(merged_dataset$responders_active, merged_dataset$cuij_responders_active, 
     xlab = "No of responders estimated by us", ylab = "No of responders estimated by Cuijpers",
     main = "Comparison of our estimations vs Cuijpers' for no of responders in active arm",
     pch = 19, col = "blue")

# for control arm
plot(merged_dataset$responders_control, merged_dataset$cuij_responders_control, 
     xlab = "No of responders estimated by us", ylab = "No of responders estimated by Cuijpers",
     main = "Comparison of our estimations vs Cuijpers' for no of responders in control arm",
     pch = 19, col = "blue")

#calculate cohens d
merged_dataset <- merged_dataset %>% 
  mutate(cohens_d_active = (post_mean_active - baseline_mean_active)/
  ((post_sd_active + baseline_sd_active)/2), cohens_d_control = (post_mean_control - baseline_mean_control)/
  ((post_sd_control + baseline_sd_control)/2))
  
  
  
  
    
  
#   
#   
# cohens_d_active = (merged_dataset$post_mean_active - merged_dataset$baseline_mean_active)/
#   ((merged_dataset$post_sd_active + merged_dataset$baseline_sd_active)/2)
# cohens_d_control <- (merged_dataset$post_mean_control - merged_dataset$baseline_mean_control)/
#   ((merged_dataset$post_sd_control + merged_dataset$baseline_sd_control)/2)
# 
# merged_dataset <- cbind(merged_dataset, cohens_d_active, cohens_d_control)
# 
# write.csv(merged_dataset, "Apples vs Oranges Dataset.csv") 
# 
# 
# cor.test(Apples_vs_Oranges_Dataset$resp_rate_active, Apples_vs_Oranges_Dataset$observed_resp_rate_active)
# library(tidyverse) 

df_appl_v_orange <- merged_dataset



inst_names <- unique(df_appl_v_orange$instrument_name)
# this is meds
meds_d_means <- list()
psy_d_means <- list()
for(i in 1: length(inst_names)){
meds_d_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 0, (instrument_name == inst_names[i])) %>% 
   summarise(n = n(), avg_d_act = mean(cohens_d_active, na.rm = T), sd_d_act = sd(cohens_d_active, na.rm = T), 
             avg_d_ctrl = mean(cohens_d_control, na.rm = T), sd_d_ctrl = sd(cohens_d_control, na.rm = T))
  


psy_d_means[[i]] <-  df_appl_v_orange  %>% 
   filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
   summarise(n = n(), avg_d_act = mean(cohens_d_active, na.rm = T), sd_d_act = sd(cohens_d_active, na.rm = T), 
             avg_d_ctrl = mean(cohens_d_control, na.rm = T), sd_d_ctrl = sd(cohens_d_control, na.rm = T))
 
}

names(meds_d_means) <- inst_names
names(psy_d_means) <- inst_names
meds_d_means <- do.call(rbind,meds_d_means )
psy_d_means <- do.call(rbind,psy_d_means)
psy_d_means <- psy_d_means %>%  mutate(type = "psy", 
                                       instr = rownames(psy_d_means)
                                       )
meds_d_means <- meds_d_means %>%  mutate(type = "med", instr = rownames(psy_d_means) )
meds_psy_combo_means <- rbind(meds_d_means,psy_d_means )
rownames(meds_psy_combo_means) <- NULL
meds_psy_combo_means %>% 
  arrange(instr)





#check 30% response rate


merged_dataset  %>% 
  filter(psy_or_med == 0) 


names(psy_d_means) <- unique(Apples_vs_Oranges_Dataset$instrument_name)[1:10]


meds_d_means

meds_vs_psy <- data.frame(rbind(meds_d_means, psy_d_means))
meds_vs_psy 




test_df <- Apples_vs_Oranges_Dataset %>% 
  filter(psy_or_med == 0, (instrument_value == 1|instrument_value == 2 |instrument_value == 3)) %>% 
  filter(!is.na(resp_rate_active) & !is.na(observed_resp_rate_active)  ) %>% 
   dplyr:: select(resp_rate_active, observed_resp_rate_active, study, year, response_criterion, instrument) #%>% 
#   filter(!is.na(resp_rate_active) & !is.na(observed_resp_rate_active)  )
# 
# Apples_vs_Oranges_Dataset %>% 
#   mutate(cuij_rate_active = cuij_responders_active/baseline_n_active) %>% 
#   filter(psy_or_med == 1) %>% 
#   dplyr:: select(resp_rate_active, cuij_rate_active, study, year, response_criterion, instrument) %>% 
#   filter(!is.na(resp_rate_active) & !is.na(cuij_rate_active)  )
