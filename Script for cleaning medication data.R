# script for cleaning medication data
# open working_dataset_medication
library(readxl)
library(tidyverse)

Working_Dataset_Medication <- read_excel("Working_Dataset_Medication.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "text", "text", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "text", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "text", "numeric", 
                                                       "text", "text", "text", "text", 
                                                       "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric",
                                                       "text", "text"))
Working_Dataset_Medication$year <- sub("\\.0$", "", Working_Dataset_Medication$year)

#add vector indicating whether psychotherapy or medication, where 1 is psych and 0 is med
psy_or_med <- rep(0, nrow(Working_Dataset_Medication))
Working_Dataset_Medication <- cbind(Working_Dataset_Medication, psy_or_med)
Working_Dataset_Medication <- Working_Dataset_Medication %>% relocate(psy_or_med, .after = year)

# rename variables to be consistent with psychotherapy dataframe
Working_Dataset_Medication <- Working_Dataset_Medication %>% rename(active_type = arm1, control_type = arm2, 
                              post_mean_active = dru_post_mean,	post_sd_active = dru_post_sd, post_n_active =	dru_post_n, 
                              post_mean_control = pla_post_mean,	post_sd_control = pla_post_sd, post_n_control =	pla_post_n,	
                              baseline_mean_active = dru_baseline_m, baseline_sd_active	= dru_baseline_sd, baseline_n_active =	dru_baseline_n_arm1,
                              baseline_mean_control = pla_baseline_m, baseline_sd_control	= pla_baseline_sd, baseline_n_control =	pla_baseline_n, 
                              active_mean_change = dru_mean_change,	active_sd_change = dru_sd_change, active_n_change =	dru_n_change,	
                              control_mean_change = pla_mean_change, control_sd_change = pla_sd_change, control_n_change	= pla_n_change,
                              age_m_active = dru_mean_age, age_sd_active = dru_sd_age, age_m_control = pla_mean_age, 
                              age_sd_control = pla_sd_age, 
                              active_percent_women	= dru_percent_women, control_percent_women =	pla_percent_women)

#rename response variables to clarify that these numbers represent OBSERVED/REPORTED response rates
# rather than those estimated by us

Working_Dataset_Medication <- Working_Dataset_Medication %>% rename(observed_responders_active = responders_active, observed_responders_active_n = responders_active_n, 
                                                                    observed_responders_control = responders_control, observed_responders_control_n	= responders_control_n)

# add index column
Working_Dataset_Medication$Column1 <- 1:nrow(Working_Dataset_Medication)
Working_Dataset_Medication <- Working_Dataset_Medication %>% relocate(Column1, .before = study)

# create dataframe with variables needed to calculate no of responders
clean_med <- Working_Dataset_Medication %>% 
  select(c(Column1, post_n_active, post_mean_active, post_sd_active, baseline_mean_active, post_n_control, post_mean_control, post_sd_control, baseline_mean_control)) %>% 
  na.omit()

set.seed(1998) # to reproduce anything with random numbers

# create and store an empty vector 
responders_active <- 0
responders_control <- 0
responders_active_30 <- 0
responders_control_30 <- 0 
responders_active_35 <- 0 
responders_control_35 <- 0

#now calculate response rates using Cuijpers' estimation method
for(i in 1: nrow(clean_med)){ 
  
  responders_active[i] <- sum((rnorm(clean_med$post_n_active[i], clean_med$post_mean_active[i], 
                                     clean_med$post_sd_active[i]))<(clean_med$baseline_mean_active[i]/2))
  
}

for(i in 1: nrow(clean_med)){ 
  
  responders_control[i] <- sum((rnorm(clean_med$post_n_control[i], clean_med$post_mean_control[i], 
                                     clean_med$post_sd_control[i]))<(clean_med$baseline_mean_control[i]/2))
  
}

# re do with 30% reduction criterion
for(i in 1: nrow(clean_med)){ 
  
  responders_active_30[i] <- sum((rnorm(clean_med$post_n_active[i], clean_med$post_mean_active[i], 
                                     clean_med$post_sd_active[i]))<(clean_med$baseline_mean_active[i]*0.7))
  
}

for(i in 1: nrow(clean_med)){ 
  
  responders_control_30[i] <- sum((rnorm(clean_med$post_n_control[i], clean_med$post_mean_control[i], 
                                      clean_med$post_sd_control[i]))<(clean_med$baseline_mean_control[i]*0.7))
  
}

# re do with 35% reduction criterion
for(i in 1: nrow(clean_med)){ 
  
  responders_active_35[i] <- sum((rnorm(clean_med$post_n_active[i], clean_med$post_mean_active[i], 
                                     clean_med$post_sd_active[i]))<(clean_med$baseline_mean_active[i]*0.65))
  
}

for(i in 1: nrow(clean_med)){ 
  
  responders_control_35[i] <- sum((rnorm(clean_med$post_n_control[i], clean_med$post_mean_control[i], 
                                      clean_med$post_sd_control[i]))<(clean_med$baseline_mean_control[i]*0.65))
  
}

# create a dataframe with our estimated no of responders for each comparison
clean_med <- cbind(clean_med, responders_active, responders_control, responders_active_30, responders_control_30,
                   responders_active_35, responders_control_35)
df_responders_med <- clean_med %>% select(c(Column1, responders_active, responders_control, responders_active_30, 
                                            responders_control_30, responders_active_35, responders_control_35))
df_full_med <- full_join(Working_Dataset_Medication, df_responders_med) # add vectors to full dataset

# Please note that some studies report SE rather than SD. Initially these were incorrectly inputed into our 
# original medication dataset however these errors have now been corrected in the original ("Working_Dataset_Medication")
# This applied to Berard 2006, Emslie 2006, Emslie 2007, Emslie 2009, Findling 2009, Keller 2001, Paxil 2009

# Please also note some spelling errors for medication names that have also been corrected in original dataset.

write.csv(df_full_med, "Full Medication Dataset.csv")

