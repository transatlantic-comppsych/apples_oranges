# create and clean new merged dataset

# merge datasets
merged_dataset <- bind_rows(df_full_med, df_full_psych)

# fix column ordering
merged_dataset <- select(merged_dataset, -c(primary, hierarchy, time, index))
merged_dataset <- merged_dataset %>% relocate(descr_active, .after = control_type)
merged_dataset <- merged_dataset %>% relocate(descr_control, .after = descr_active)
merged_dataset <- merged_dataset %>% relocate(mean_age, .after = age_group)
merged_dataset <- merged_dataset %>% relocate(percent_women, .after = control_mean_age)
merged_dataset <- merged_dataset %>% relocate(responders_active: cuij_responders_control, .after = observed_resp_rate_control)

install.packages("tidyverse")
library(tidyverse)

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

# create response rate variables
# first create our primary response rate variable, which is estimated number of responders / n at randomisation
resp_rate_active <- (merged_dataset$responders_active)/(merged_dataset$baseline_n_active)
resp_rate_control <- (merged_dataset$responders_control)/(merged_dataset$baseline_n_control)

#then calculate response rate using n at post-test (i.e. completers only) 
resp_rate_active_completers <- (merged_dataset$responders_active)/(merged_dataset$post_n_active)
resp_rate_control_completers <- (merged_dataset$responders_control)/(merged_dataset$post_n_control)

merged_dataset <- cbind(merged_dataset, resp_rate_active, resp_rate_control, resp_rate_active_completers, resp_rate_control_completers)
merged_dataset <- merged_dataset %>% relocate(resp_rate_active: resp_rate_control_completers, .after = responders_control)

merged_dataset <- merged_dataset %>%
  mutate(
    resp_rate_active = round(resp_rate_active, 4),
    resp_rate_control = round(resp_rate_control, 4),
    resp_rate_active_completers = round(resp_rate_active_completers, 4),
    resp_rate_control_completers = round(resp_rate_control_completers, 4)
  )

write.csv(merged_dataset, "Apples vs Oranges Dataset.csv") 

for_visual_inspection <- cbind(merged_dataset$study, merged_dataset$year, merged_dataset$instrument_value, merged_dataset$resp_rate_active, merged_dataset$resp_rate_control)
