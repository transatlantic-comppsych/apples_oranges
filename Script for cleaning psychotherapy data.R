library(tidyverse)
library(dplyr)
library(readxl)

set.seed(1998) # to reproduce anything with random numbers

Full_Dataset_Cuijpers_MA <- read_excel("Full_Dataset_Cuijpers_MA.xlsx")
View(Full_Dataset_Cuijpers_MA)

# Sub commas for dots and convert to numeric variables

Full_Dataset_Cuijpers_MA$n_arm1 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$n_arm1) 
Full_Dataset_Cuijpers_MA$mean_arm1 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$mean_arm1) 
Full_Dataset_Cuijpers_MA$sd_arm1 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$sd_arm1) 
Full_Dataset_Cuijpers_MA$baseline_m_arm1 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$baseline_m_arm1) 
Full_Dataset_Cuijpers_MA$n_arm1 <- as.numeric(Full_Dataset_Cuijpers_MA$n_arm1)
Full_Dataset_Cuijpers_MA$mean_arm1 <- as.numeric(Full_Dataset_Cuijpers_MA$mean_arm1)
Full_Dataset_Cuijpers_MA$sd_arm1 <- as.numeric(Full_Dataset_Cuijpers_MA$sd_arm1)
Full_Dataset_Cuijpers_MA$baseline_m_arm1 <- as.numeric(Full_Dataset_Cuijpers_MA$baseline_m_arm1)
Full_Dataset_Cuijpers_MA$n_arm2 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$n_arm2) 
Full_Dataset_Cuijpers_MA$mean_arm2 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$mean_arm2) 
Full_Dataset_Cuijpers_MA$sd_arm2 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$sd_arm2) 
Full_Dataset_Cuijpers_MA$baseline_m_arm2 <- gsub(",", ".", Full_Dataset_Cuijpers_MA$baseline_m_arm2) 
Full_Dataset_Cuijpers_MA$n_arm2 <- as.numeric(Full_Dataset_Cuijpers_MA$n_arm2)
Full_Dataset_Cuijpers_MA$mean_arm2 <- as.numeric(Full_Dataset_Cuijpers_MA$mean_arm2)
Full_Dataset_Cuijpers_MA$sd_arm2 <- as.numeric(Full_Dataset_Cuijpers_MA$sd_arm2)
Full_Dataset_Cuijpers_MA$baseline_m_arm2 <- as.numeric(Full_Dataset_Cuijpers_MA$baseline_m_arm2)

# create dataframe with vectors needed to calculate no of responders

df_clean <- Full_Dataset_Cuijpers_MA %>% 
  select(c(Column1, n_arm1, mean_arm1, sd_arm1, baseline_m_arm1, n_arm2, mean_arm2, sd_arm2, baseline_m_arm2)) %>% 
  na.omit()

# create and store an empty vector 
responders_arm1 <- 0
responders_arm2 <- 0

# a loop to create a distribution of response per study and find out how many below the criterion
# which is 50% reduction of baseline mean

for(i in 1: nrow(df_clean)){ 
  
  responders_arm1[i] <- sum((rnorm(df_clean$n_arm1[i], df_clean$mean_arm1[i], 
                                   df_clean$sd_arm1[i]))<(df_clean$baseline_m_arm1[i]/2))
  
}

for(i in 1: nrow(df_clean)){ 
  
  responders_arm2[i] <- sum((rnorm(df_clean$n_arm2[i], df_clean$mean_arm2[i], 
                                   df_clean$sd_arm2[i]))<(df_clean$baseline_m_arm2[i]/2))
  
}

# create a dataframe with our estimated no of responders for each comparison
df_responders <- as.data.frame(cbind(df_clean$Column1, responders_arm1, responders_arm2))
df_responders <- df_responders %>% rename(Column1 = V1)
df_full <- full_join(Full_Dataset_Cuijpers_MA, df_responders)

# add condition/arm description and instrument name to responders dataframe to ensure response estimates
# are matched up to correct comparison as there are several per study
df_responders <- df_full %>% select(Column1, study, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument, 
                                               responders_arm1, responders_arm2)

#create dataframe with response rates calculated by Cuijpers
Cuijpers_response_rate_estimates <- read_excel("Cuijpers_response_rate_estimates.xlsx")

df_cuijpers_calcs <- Cuijpers_response_rate_estimates %>% select(study, outcome, ee, ec)
df_cuijpers_calcs <- df_cuijpers_calcs %>% rename(instrument = outcome)

# merge datasets matching up study name and instrument used
df_compare_response_rates <- merge(df_responders, df_cuijpers_calcs, by = c("study","instrument"), all.x = TRUE)

# Export csv to manually check and add missing values (due to differences in ways study name and instrument is formatted)
write.csv(df_compare_response_rates, "Response_rate_comparison.csv")

# read in updated csv with additions
df_compare_response_rates <- read.csv("Response_rate_comparison_updated.csv")

# check correlation between our calculations of response rate and Cuijper's calculations
cor(df_compare_response_rates$responders_arm1, df_compare_response_rates$ee, method = "pearson", use = "pairwise.complete.obs")
cor(df_compare_response_rates$responders_arm2, df_compare_response_rates$ec, method = "pearson", use = "pairwise.complete.obs")

for_merge <- df_compare_response_rates %>% select(Column1, ee, ec)
df_full <- full_join(df_full, for_merge)
                      
# create dataframe with main variables needed for meta-analysis
df_full_psych <- df_full %>% 
  select(c(Column1, study, year, condition_arm1, condition_arm2, descr_arm1, descr_arm2, instrument, rating,
           mean_arm1: n_arm2, baseline_m_arm1:baseline_n_arm2, mean_age, percent_women, responders_arm1, responders_arm2, 
           ee, ec, country, comorbid_mental, `comorbid_mental?`, diagnosis))

# rename variables
df_full_psych <- df_full_psych %>% rename(active_type = condition_arm1, control_type = condition_arm2, descr_active = descr_arm1, descr_control = descr_arm2, 
                                          post_mean_active = mean_arm1,	post_sd_active = sd_arm1, post_n_active =	n_arm1, 
                                          post_mean_control = mean_arm2,	post_sd_control = sd_arm2, post_n_control =	n_arm2,	
                                          baseline_mean_active = baseline_m_arm1, baseline_sd_active	= baseline_sd_arm1, baseline_n_active =	baseline_n_arm1,
                                          baseline_mean_control = baseline_m_arm2, baseline_sd_control	= baseline_sd_arm2, baseline_n_control =	baseline_n_arm2, 
                                          responders_active = responders_arm1, responders_control =	responders_arm2,	
                                          cuij_responders_active = ee, cuij_responders_control = ec)

#add vector indicating whether psychotherapy or medication 

psy_or_med <- rep(1, nrow(df_full_psych))
df_full_psych <- cbind(df_full_psych, psy_or_med)
df_full_psych <- df_full_psych %>% relocate(psy_or_med, .after = year)

df_full_psych$year <- as.character(df_full_psych$year)

df_full_psych$baseline_sd_active <- gsub(",", ".", df_full_psych$baseline_sd_active)
df_full_psych$baseline_n_active <- gsub(",", ".", df_full_psych$baseline_n_active)
df_full_psych$baseline_sd_control <- gsub(",", ".", df_full_psych$baseline_sd_control)
df_full_psych$baseline_n_control <- gsub(",", ".", df_full_psych$baseline_n_control)
df_full_psych$mean_age <- gsub(",", ".", df_full_psych$mean_age)

df_full_psych$baseline_sd_active <- as.numeric(df_full_psych$baseline_sd_active)
df_full_psych$baseline_n_active <- as.numeric(df_full_psych$baseline_n_active)
df_full_psych$baseline_sd_control <- as.numeric(df_full_psych$baseline_sd_control)
df_full_psych$baseline_n_control <- as.numeric(df_full_psych$baseline_n_control)
df_full_psych$mean_age <- as.numeric(df_full_psych$mean_age)

# We have noticed a few errors in Cuijpers dataset
# One is regarding the incorrect extraction of data for De Jong Heesen 2020. I have fixed this in full dataset Cuijpers MA. 
# Error was for baseline mean and SD in control arm on the CDI parent

# I am going to filter out recent studies published after the release of Cuijpers MA. This is because we are running a search for med
# studies up to the last date of Cuijpers search (01/01/2021), and we want to be consistent.

df_full_psych <- df_full_psych %>% 
  filter(!(year %in% c(2021, 2022, 2023)))

# I'm going to remove the control condition for March 2004 - psy - as the control is actually placebo but right now it is being incorrectly
# considered as a psychological control.

df_full_psych <- df_full_psych %>%
  mutate_at(vars(contains("control")), ~ifelse(study == "March, 2004", NA, .))

write.csv(df_full_psych, "Full Psychotherapy Dataset.csv") 
