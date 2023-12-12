# create and clean new merged dataset
library(readr)
library(tidyverse)
library(knitr)
install.packages("kableExtra")
library(kableExtra)

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
      instrument %in% c("CGI", "CGI-I") ~ "cgi",
      instrument %in% c("GAS") ~ "gas",
      instrument %in% c("Acholi Psychosocial Assessment Instrument (APAI): a local instrument") ~ "apai",
      instrument %in% c("phq-9") ~ "phq_9",
      instrument %in% c("PFCâ€“S") ~ "pfc",
      instrument %in% c("PAPA") ~ "papa",
      instrument %in% c("dass") ~ "dass",
      instrument %in% c("disc-c MDD symptom count") ~ "disc_c_mdd",
      # TRUE ~ 99 # Default case, if none of the above conditions match
    )
  )
merged_dataset <- merged_dataset %>% relocate(instrument_name, .after = instrument)

unique(merged_dataset$instrument)

# create response rate variables
# first create our primary response rate variable, which is estimated number of responders / n at randomisation
resp_rate_active <- (merged_dataset$responders_active)/(merged_dataset$baseline_n_active)
resp_rate_control <- (merged_dataset$responders_control)/(merged_dataset$baseline_n_control)

#then calculate response rate using n at post-test (i.e. completers only) 
resp_rate_active_completers <- (merged_dataset$responders_active)/(merged_dataset$post_n_active)
resp_rate_control_completers <- (merged_dataset$responders_control)/(merged_dataset$post_n_control)

merged_dataset <- cbind(merged_dataset, resp_rate_active, resp_rate_control, resp_rate_active_completers, resp_rate_control_completers)
merged_dataset <- merged_dataset %>% relocate(resp_rate_active: resp_rate_control_completers, .after = responders_control)

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
  
#this is psy
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

# create unique study ids to account for multiple active arms per study name 
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(study_ID = ifelse(psy_or_med == 0, paste(study, year, active_type, sep = "_"), 
                  paste(study, descr_active, sep = "_")))
# arranging by instrument value
df_appl_v_orange <- df_appl_v_orange %>% arrange(study_ID, instrument_value)
# retain only first row
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 

# start looking at demographics
df_demographics <- df_first_row %>% select(study, year, psy_or_med, active_type, control_type, descr_active, descr_control,
                                           instrument_name, baseline_mean_active, baseline_sd_active, baseline_n_active,
                                           baseline_mean_control, baseline_sd_control, baseline_n_control, 
                                           mean_age, active_mean_age, control_mean_age, 
                                           percent_women, active_percent_women, control_percent_women )
# create overall mean age variable for all studies 
df_demographics <- df_demographics %>%  mutate(overall_mean_age = (active_mean_age * baseline_n_active + control_mean_age * baseline_n_control)
                                               /(baseline_n_active + baseline_n_control))
df_demographics <- df_demographics %>%  mutate(mean_age = coalesce(mean_age, overall_mean_age))

#create overall percent women variable for all studies
df_demographics <- df_demographics %>%  mutate(overall_percent_women = (active_percent_women * baseline_n_active + control_percent_women * baseline_n_control)
                                               /(baseline_n_active + baseline_n_control))
df_demographics <- df_demographics %>%  mutate(percent_women = coalesce(percent_women, overall_percent_women))

df_demographics <- df_demographics %>%  select(-c(active_mean_age, control_mean_age, overall_mean_age, 
                                                  active_percent_women, control_percent_women, overall_percent_women))

# combine arm description variables across psy and med
df_demographics$descr_active <- ifelse(is.na(df_demographics$descr_active), df_demographics$active_type, df_demographics$descr_active)
df_demographics$descr_control <- ifelse(is.na(df_demographics$descr_control), df_demographics$control_type, df_demographics$descr_control)

df_demographics <- df_demographics %>%  select(-c(active_mean_age, control_mean_age, overall_mean_age, 
                                                  active_percent_women, control_percent_women, overall_percent_women,
                                                  active_type, control_type))
df_demographics <- df_demographics %>%  arrange(psy_or_med)

#create a table
table_demographics <- kable(df_demographics, caption = "Demographics")
print(table_demographics)

write.csv(df_appl_v_orange, "Apples vs Oranges Dataset.csv") 

#test_df <- Apples_vs_Oranges_Dataset %>% 
#  filter(psy_or_med == 0, (instrument_value == 1|instrument_value == 2 |instrument_value == 3)) %>% 
#  filter(!is.na(resp_rate_active) & !is.na(observed_resp_rate_active)  ) %>% 
#   dplyr:: select(resp_rate_active, observed_resp_rate_active, study, year, response_criterion, instrument) #%>% 
#   filter(!is.na(resp_rate_active) & !is.na(observed_resp_rate_active)  )
# 
# Apples_vs_Oranges_Dataset %>% 
#   mutate(cuij_rate_active = cuij_responders_active/baseline_n_active) %>% 
#   filter(psy_or_med == 1) %>% 
#   dplyr:: select(resp_rate_active, cuij_rate_active, study, year, response_criterion, instrument) %>% 
#   filter(!is.na(resp_rate_active) & !is.na(cuij_rate_active)  )
