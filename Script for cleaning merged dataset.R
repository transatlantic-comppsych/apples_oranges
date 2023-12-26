library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(misty)
library(openxlsx)
library(readxl)

# Merge and Clean Dataset -------------------------------------------------

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

# create an instrument name variable to ensure consistency
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

# Charlotte note to self: Now that I am more familiar with R, I can see that it would have been better to do the below with mutate(). 
# At some point, fix code below to make more tidy.

# create response rate variables
# first create our primary response rate variable, which is estimated number of responders / n at randomisation
resp_rate_active <- (merged_dataset$responders_active)/(merged_dataset$baseline_n_active)
resp_rate_control <- (merged_dataset$responders_control)/(merged_dataset$baseline_n_control)

# then calculate response rate using n at post-test (i.e. completers only) 
resp_rate_active_completers <- (merged_dataset$responders_active)/(merged_dataset$post_n_active)
resp_rate_control_completers <- (merged_dataset$responders_control)/(merged_dataset$post_n_control)

merged_dataset <- cbind(merged_dataset, resp_rate_active, resp_rate_control, resp_rate_active_completers, resp_rate_control_completers)
merged_dataset <- merged_dataset %>% relocate(resp_rate_active: resp_rate_control_completers, .after = responders_control)

# calculate response rate for studies that reported no of responders rather than a response rate
calc_observed_resp_rate_active <- (merged_dataset$observed_responders_active)/(merged_dataset$observed_responders_active_n)
calc_observed_resp_rate_control <- (merged_dataset$observed_responders_control)/(merged_dataset$observed_responders_control_n)

# then concatenate with variable indicating reported response rates
merged_dataset$observed_resp_rate_active <- coalesce(calc_observed_resp_rate_active, merged_dataset$observed_resp_rate_active)
merged_dataset$observed_resp_rate_control <- coalesce(calc_observed_resp_rate_control, merged_dataset$observed_resp_rate_control)

# check correlation between our calculations and observed response rate
cor(merged_dataset$resp_rate_active, merged_dataset$observed_resp_rate_active, method = "pearson", use = "pairwise.complete.obs")
cor(merged_dataset$resp_rate_control, merged_dataset$observed_resp_rate_control, method = "pearson", use = "pairwise.complete.obs")

# check correlation between our calculations and cuijpers' calculations 
cor(merged_dataset$responders_active, merged_dataset$cuij_responders_active, method = "pearson", use = "pairwise.complete.obs")
cor(merged_dataset$responders_control, merged_dataset$cuij_responders_control, method = "pearson", use = "pairwise.complete.obs")

#rename dataframe
df_appl_v_orange <- merged_dataset

# We have mean_age and percent_women reported overall for psy trials but per arm for med trials

# create overall mean age variable for all studies 
df_appl_v_orange <- df_appl_v_orange %>%  mutate(overall_mean_age = (active_mean_age * baseline_n_active + control_mean_age * baseline_n_control)
                                                 /(baseline_n_active + baseline_n_control))
df_appl_v_orange <- df_appl_v_orange %>%  mutate(mean_age = coalesce(mean_age, overall_mean_age))

#create overall percent women variable for all studies
df_appl_v_orange <- df_appl_v_orange %>%  mutate(overall_percent_women = (active_percent_women * baseline_n_active + control_percent_women * baseline_n_control)
                                                 /(baseline_n_active + baseline_n_control))
df_appl_v_orange <- df_appl_v_orange %>%  mutate(percent_women = coalesce(percent_women, overall_percent_women))

# calculate cohens d
df_appl_v_orange <- df_appl_v_orange %>% 
  mutate(cohens_d_active = (post_mean_active - baseline_mean_active)/
  ((post_sd_active + baseline_sd_active)/2), cohens_d_control = (post_mean_control - baseline_mean_control)/
  ((post_sd_control + baseline_sd_control)/2))

# Create summary statistics for each instrument  --------

#average statistics for each instrument, collapsed across studies for both med and psy categories 
# this includes duplicates of studies 

meds_d_means <- list()
psy_d_means <- list()
meds_baseline_means <- list()
psy_baseline_means <- list()
meds_post_means <- list()
psy_post_means <- list()

inst_names <- unique(df_appl_v_orange$instrument_name)[1:6]
for(i in 1: length(inst_names)){
#cohens d for meds
  meds_d_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 0, (instrument_name == inst_names[i])) %>% 
   summarise(n = n(), avg_d_act = mean(cohens_d_active, na.rm = T), sd_d_act = sd(cohens_d_active, na.rm = T), 
             avg_d_ctrl = mean(cohens_d_control, na.rm = T), sd_d_ctrl = sd(cohens_d_control, na.rm = T))
  
# cohens d for psy
psy_d_means[[i]] <-  df_appl_v_orange  %>% 
   filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
   summarise(n = n(), avg_d_act = mean(cohens_d_active, na.rm = T), sd_d_act = sd(cohens_d_active, na.rm = T), 
             avg_d_ctrl = mean(cohens_d_control, na.rm = T), sd_d_ctrl = sd(cohens_d_control, na.rm = T))

# for medication trials, average baseline mean scores for each instrument
meds_baseline_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 0, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), total_n_control = sum(baseline_n_control, na.rm = T), 
            total_n_active = sum(baseline_n_active, na.rm = T),
            avg_age = mean(mean_age, na.rm = T),
            avg_baseline_mean_act = mean(baseline_mean_active, na.rm = T), 
            sd_baseline_mean_act = sd(baseline_mean_active, na.rm = T), 
            avg_baseline_mean_ctrl = mean(baseline_mean_control, na.rm = T), 
            sd_baseline_mean_ctrl = sd(baseline_mean_control, na.rm = T))

# for psychotherapy trials, average baseline mean scores for each instrument
psy_baseline_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), total_n_control = sum(baseline_n_control, na.rm = T), 
            total_n_active = sum(baseline_n_active, na.rm = T),
             avg_age = mean(mean_age, na.rm = T),
            avg_baseline_mean_act = mean(baseline_mean_active, na.rm = T), 
            sd_baseline_mean_act = sd(baseline_mean_active, na.rm = T), 
            avg_baseline_mean_ctrl = mean(baseline_mean_control, na.rm = T), 
            sd_baseline_mean_ctrl = sd(baseline_mean_control, na.rm = T))

# for medication trials, average post-test mean scores for each instrument
meds_post_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 0, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), avg_post_mean_act = mean(post_mean_active, na.rm = T), sd_post_mean_act = sd(post_mean_active, na.rm = T), 
            avg_post_mean_ctrl = mean(post_mean_control, na.rm = T), sd_post_mean_ctrl = sd(post_mean_control, na.rm = T))

# for psychotherapy trials, average post-test mean scores for each instrument
psy_post_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), avg_post_mean_act = mean(post_mean_active, na.rm = T), sd_post_mean_act = sd(post_mean_active, na.rm = T), 
            avg_post_mean_ctrl = mean(post_mean_control, na.rm = T), sd_post_mean_ctrl = sd(post_mean_control, na.rm = T))

}

names(meds_d_means) <- inst_names
names(psy_d_means) <- inst_names
names(meds_baseline_means) <- inst_names
names(psy_baseline_means) <- inst_names
names(meds_post_means) <- inst_names
names(psy_post_means) <- inst_names
meds_d_means <- do.call(rbind,meds_d_means )
psy_d_means <- do.call(rbind,psy_d_means)
meds_baseline_means <- do.call(rbind, meds_baseline_means)
psy_baseline_means <- do.call(rbind, psy_baseline_means)
meds_post_means <- do.call(rbind, meds_post_means)
psy_post_means <- do.call(rbind, psy_post_means)

psy_d_means <- psy_d_means %>%  mutate(type = "psy", instr = rownames(psy_d_means))
meds_d_means <- meds_d_means %>%  mutate(type = "med", instr = rownames(psy_d_means) )
psy_baseline_means <- psy_baseline_means %>%  mutate(type = "psy", instr = rownames(psy_baseline_means))
meds_baseline_means <- meds_baseline_means %>%  mutate(type = "med", instr = rownames(meds_baseline_means))
psy_post_means <- psy_post_means %>%  mutate(type = "psy", instr = rownames(psy_post_means))
meds_post_means <- meds_post_means %>%  mutate(type = "med", instr = rownames(meds_post_means))

meds_psy_combo_d_means <- rbind(meds_d_means,psy_d_means )
meds_psy_combo_baseline_means <- rbind(meds_baseline_means, psy_baseline_means )
meds_psy_combo_post_means <- rbind(meds_post_means, psy_post_means )

df_stats_per_instrument <- full_join(meds_psy_combo_baseline_means, meds_psy_combo_post_means, by = c("type", "instr", "n"))
df_stats_per_instrument <- full_join(df_stats_per_instrument, meds_psy_combo_d_means, by = c("type", "instr", "n"))

df_stats_per_instrument <- df_stats_per_instrument %>% relocate(instr, .before = n)
df_stats_per_instrument <- df_stats_per_instrument %>% relocate(type, .after = instr)
df_stats_per_instrument <- df_stats_per_instrument %>% arrange(instr) 

# Look at demographics per study, for primary instrument ------------------

# create unique study ids to account for cases when there are multiple active or control arms per study name 
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(study_ID = ifelse(psy_or_med == 0, paste(study, year, active_type, control_type, sep = "_"), 
                           paste(study, descr_active, descr_control, sep = "_")))

# arranging by study ID and instrument value
df_appl_v_orange <- df_appl_v_orange %>% arrange(study_ID, instrument_value)

# retain only first row to look at demographics as this will keep one row per study_ID, taking the primary instrument
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 

#check how many med comparisons we cannot calc cohens d for (for primary instrument)
filter_test <- df_first_row %>% filter(psy_or_med == 0)
sum(is.na(filter_test$cohens_d_active))
sum(is.na(filter_test$cohens_d_control))

# start looking at demographics, baseline and post means, and cohens d per arm
df_demographics <- df_first_row %>% select(study, year, psy_or_med, active_type, control_type, descr_active, descr_control,
                                           instrument_name, baseline_mean_active, baseline_sd_active, baseline_n_active,
                                           baseline_mean_control, baseline_sd_control, baseline_n_control, 
                                           post_mean_active, post_sd_active, post_n_active, post_mean_control, post_sd_control, post_n_control,
                                           cohens_d_active, cohens_d_control,  mean_age, percent_women)

# combine arm description variables across psy and med
df_demographics$descr_active <- ifelse(is.na(df_demographics$descr_active), df_demographics$active_type, df_demographics$descr_active)
df_demographics$descr_control <- ifelse(is.na(df_demographics$descr_control), df_demographics$control_type, df_demographics$descr_control)

df_demographics <- df_demographics %>%  select(-c(active_type, control_type))
df_demographics <- df_demographics %>%  arrange(psy_or_med)

# check how many studies we have baseline means for primary instrument
df_summary_missing_data <- df_demographics %>% 
  group_by(psy_or_med) %>% 
  summarise(n_reporting_base_means_act = sum(!is.na(baseline_mean_active)), n_reporting_base_means_contr = sum(!is.na(baseline_mean_control)),
  n_reporting_post_means_act = sum(!is.na(post_mean_active)), n_reporting_post_means_contr = sum(!is.na(post_mean_control)))

# average cohens d, mean age and percent women, summarising across psy and med
df_means_demo <- df_demographics %>%  
  group_by(psy_or_med) %>% 
  summarise(mean_cohens_d_active = mean(cohens_d_active, na.rm = TRUE),
  mean_cohens_d_control = mean(cohens_d_control, na.rm = TRUE),
  mean_age = mean(mean_age, na.rm = TRUE), 
  mean_percent_women = mean(percent_women, na.rm = TRUE),
  missing_d = sum(is.na(cohens_d_active)))

# Argyris looking at stats per instr

# keep only non_zero values
test_2 <- df_stats_per_instrument %>% 
  filter((type == "med" & n != 0) | (type == "psy" & n != 0) )

# keep only duplicated rows
library(misty)
test_2 <- df.duplicated(test_2, instr)

get_cdrs_means <- test_2 %>% 
  filter(instr == "cdrs") 

test_2 %>% 
  filter(instr == "hamd") 


test_2 %>% 
  filter(instr == "bdi") 

t.test.fromSummaryStats <- function(mu,n,s) {
  -diff(mu) / sqrt( sum( s^2/n ) )
}

mu <- c(get_cdrs_means$avg_d_ctrl[1],get_cdrs_means$avg_d_ctrl[2])
n <- c(get_cdrs_means$total_n_control[1],get_cdrs_means$total_n_control[2])
s <- c(get_cdrs_means$sd_d_ctrl[1],get_cdrs_means$sd_d_ctrl[2])
t.test.fromSummaryStats(mu,n,s)

df_demographics %>% 
  group_by(psy_or_med ) %>% 
  summarise (avg_age = mean(mean_age, na.rm = T), sd_age = sd(mean_age, na.rm = T))

#openxlsx:: write.xlsx(df_stats_per_instrument, file = "test_2.xlsx", colNames = T, borders = "columns", asTable = F)

# check the Cohen's ds for medication and psychotherapy
tapply(df_appl_v_orange$cohens_d_control,
       df_appl_v_orange$psy_or_med, summary,na.rm = TRUE)
# you see that I used tapply rather than dplyr's group_by. Here I am also using aggregate, another
# R base function. Neat, right :) 
aggregate(df_appl_v_orange$cohens_d_control, by  = list(df_appl_v_orange$psy_or_med), summary)

# from the above it seemed like we had some studies where the controls have a positive d
# here is a quick histogram to see what I mean
tapply(df_appl_v_orange$cohens_d_control,df_appl_v_orange$psy_or_med, hist)

# I have therefore re-run things here excluding those with positive ds. 
tapply(df_appl_v_orange[df_appl_v_orange$cohens_d_control<=0, ]$cohens_d_control,
       df_appl_v_orange[df_appl_v_orange$cohens_d_control<=0, ]$psy_or_med, summary,na.rm = TRUE)

# which ones are the offending studies. As can be seen, nearly all of these studies are in the
# psychotherapy part with some duplicates or triplicates.

studies_with_positive_cohens_ds <- tapply(df_appl_v_orange[df_appl_v_orange$cohens_d_control>=0
                 , ]$study, df_appl_v_orange[df_appl_v_orange$cohens_d_control>=0
                                             , ]$psy_or_med, na.omit)

names(studies_with_positive_cohens_ds) <- c("meds", "psy") # to make prettier

studies_with_positive_cohens_ds

# We have inspected those with pos cohens d. It appears for most there is a genuine (small) deterioration in control arm.
# For Geller, positive cohens d is a factor of using the GAS. This should not be a problem once we take primary instrument for each study.
# Other large post cohens d is for Reynolds 1986, this seems to be correct. 
# Detected one error for the De Jong Heesen study - there is a mistake in Cuijpers dataset. Fixed in original dataset.
# yet to do - inform Cuipers

#check which studies have missing means or sds at baseline or post 

# Specify the columns to check
columns_to_check <- c(
  "baseline_mean_active", "baseline_sd_active",
  "baseline_mean_control", "baseline_sd_control",
  "post_mean_active", "post_sd_active",
  "post_mean_control", "post_sd_control"
)

# Idemissing values for each study
tapply(print(columns_with_missing_values[,1:2]
), columns_with_missing_values$psy_or_med)
#identify which columns have missing values for each study
columns_with_missing_values <- df_appl_v_orange %>%
  filter(rowSums(is.na(.[columns_to_check])) > 0) %>%
  select(psy_or_med, study, year, where(function(x) any(is.na(x))))

# Print columns with 
# create excel sheet to edit
# we have created a notes column named how_handle_es_calc where we inspect each study and its missing data
# from there we work out which method to use to impute / derive an appropriate value to substitute missing values

openxlsx:: write.xlsx(columns_with_missing_values, file = "columns_with_missing_values.xlsx", colNames = T, borders = "columns", asTable = F)

# The updated version of this sheet with notes is called "columns_with_missing_values_updated.xlsx"

# Please note that some studies report SE rather than SD. Initially these were incorrectly inputed into our 
# original medication dataset however these errors have now been corrected in the original ("Working_Dataset_Medication")
# This applied to Berard 2006, Emslie 2006, Emslie 2007, Emslie 2009, Findling 2009, Keller 2001, Paxil 2009

# Now begin imputing missing values. We will start the the meds studies. The psy studies that are missing values
# were not in Cuijper's MA table of included studies (I think because their primary outcome measures were not continuous).
# We will need to decide our approach here - if we retain these studies, we will have to redo extraction. Will come back to this. 

# To check how many rows with missing SDs we have
# Let's do this for only the primary instrument for each study (using df_first_row), starting with med studies.
df_first_row_missing_sds <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_sds = sum(rowSums(is.na(select(., c(baseline_sd_active , post_sd_active, baseline_sd_control, post_sd_control)))) > 0)
  )
df_first_row_missing_sds

# Now check how many rows with missing means
df_first_row_missing_means <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_means = sum(rowSums(is.na(select(., c(baseline_mean_active , post_mean_active, baseline_mean_control, post_mean_control)))) > 0)
  )
df_first_row_missing_means

# Let's impute missing SDs first. If either the baseline or post-intervention SD is unavailable, it will be substituted by the other.
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    baseline_sd_active = ifelse(psy_or_med == 0 & is.na(baseline_sd_active), post_sd_active, baseline_sd_active),
    post_sd_active = ifelse(psy_or_med == 0 & is.na(post_sd_active), baseline_sd_active, post_sd_active),
    baseline_sd_control = ifelse(psy_or_med == 0 & is.na(baseline_sd_control), post_sd_control, baseline_sd_control),
    post_sd_control = ifelse(psy_or_med == 0 & is.na(post_sd_control), baseline_sd_control, post_sd_control)
  )

# Now lets impute missing means. If either the baseline or post mean is missing, use the change score to calculate this. 
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    baseline_mean_active = ifelse(psy_or_med == 0 & is.na(baseline_mean_active), post_mean_active - active_mean_change, baseline_mean_active),
    baseline_mean_control = ifelse(psy_or_med == 0 & is.na(baseline_mean_control), post_mean_control - control_mean_change, baseline_mean_control),
    post_mean_active = ifelse(psy_or_med == 0 & is.na(post_mean_active), baseline_mean_active + active_mean_change, post_mean_active),
    post_mean_control = ifelse(psy_or_med == 0 & is.na(post_mean_control), baseline_mean_control + control_mean_change, post_mean_control)
  )

# Check how many studies with missing SDs after performing imputation
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 

df_first_row_missing_sds <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_sds = sum(rowSums(is.na(select(., c(baseline_sd_active , post_sd_active, baseline_sd_control, post_sd_control)))) > 0)
  )
df_first_row_missing_sds

# We have gone from 21 to 8 studies with missing SDs. Great!

# And check to see how many studies with missing means after performing imputation

df_first_row_missing_means <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_means = sum(rowSums(is.na(select(., c(baseline_mean_active , post_mean_active, baseline_mean_control, post_mean_control)))) > 0)
  )
df_first_row_missing_means

# We have gone from 15 to 9 studies with missing means. Great!

# Lets recalculate cohens d now that we have more data
df_appl_v_orange <- df_appl_v_orange %>% 
  mutate(cohens_d_active = (post_mean_active - baseline_mean_active)/
           ((post_sd_active + baseline_sd_active)/2), cohens_d_control = (post_mean_control - baseline_mean_control)/
           ((post_sd_control + baseline_sd_control)/2))
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE)

filter_test <- df_first_row %>% filter(psy_or_med == 0)
sum(!is.na(filter_test$cohens_d_active))
sum(!is.na(filter_test$cohens_d_control))

# Before we could only calculate cohens d for 12 med studies, now we can calculate this for 22

# how many unique med comparisons do we have
unique_psy <- df_appl_v_orange %>% 
  filter(psy_or_med == 1) %>% 
  summarise(unique_psy = n_distinct(study_ID))
unique_psy

# and for psy
unique_med <- df_appl_v_orange %>% 
  filter(psy_or_med == 0) %>% 
  summarise(unique_med = n_distinct(study_ID))
unique_med

# Check again to identify which med studies still have missing values in our columns of interest after performing the imputation above
new_columns_with_missing_values <- df_first_row %>%
  filter(rowSums(is.na(.[columns_to_check])) > 0) %>%
  filter(psy_or_med == 0) %>% 
  select(study, year, active_type, instrument_name, where(function(x) any(is.na(x))))
tapply(print(new_columns_with_missing_values[,1:2]), new_columns_with_missing_values$psy_or_med)
openxlsx:: write.xlsx(new_columns_with_missing_values, file = "new_columns_with_missing_values.xlsx", colNames = T, borders = "columns", asTable = F)

# For studies missing all sds, we will sub in an average SD taken from similar studies in the dataset. We will calculate average SDs for each arm, pre and post.
# We will just do this for the CDRS. The studies missing SDs for both pre and post are Bristol-Myers Squibb 2002a and 2002b, Emslie 2002b, Organon 2002a and 2002b, 
# Paxil (GlaxoSmithKline) 2009, Wagner 2006

# First calculate the average values
calc_mean_sds <- df_appl_v_orange %>% 
  filter(psy_or_med == 0, instrument_name == "cdrs") %>% 
  summarise(mean_baseline_sd_active = mean(baseline_sd_active, na.rm = TRUE),
            mean_baseline_sd_control = mean(baseline_sd_control, na.rm = TRUE),
            mean_post_sd_active = mean(post_sd_active, na.rm = TRUE),
            mean_post_sd_control = mean(post_sd_control, na.rm = TRUE))

# I want these as numeric values
mean_baseline_sd_active <- calc_mean_sds$mean_baseline_sd_active
mean_baseline_sd_control <- calc_mean_sds$mean_baseline_sd_control
mean_post_sd_active <- calc_mean_sds$mean_post_sd_active
mean_post_sd_control <- calc_mean_sds$mean_post_sd_control

# Then use these for imputation. Only use these mean values for med studies using the CDRS.
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    baseline_sd_active = ifelse(psy_or_med == 0 & is.na(baseline_sd_active) & instrument_name == "cdrs", mean_baseline_sd_active, baseline_sd_active),
    post_sd_active = ifelse(psy_or_med == 0 & is.na(post_sd_active) & instrument_name == "cdrs", mean_post_sd_active, post_sd_active),
    baseline_sd_control = ifelse(psy_or_med == 0 & is.na(baseline_sd_control) & instrument_name == "cdrs", mean_baseline_sd_control, baseline_sd_control),
    post_sd_control = ifelse(psy_or_med == 0 & is.na(post_sd_control) & instrument_name == "cdrs", mean_post_sd_control, post_sd_control)
  )

# Read in Cipriani dataset ------------------------------------------------

# We are now going to clean and merge in Cipriani's data. For context, we have tried contacting Cipriani and Peng Xie multiple times
# to request the full dataset for their MA but did not receive a reply. There is a more limited dataset provided online
# as a PDF. The dataset I will now read in called "Full_Dataset_Cipriani_MA" is taken from the table in this PDF 
# and converted to excel. I have added column names - this is the only thing I have changed. 

df_cipriani <- read_excel("Full_Dataset_Cipriani_MA.xlsx", 
                                       col_types = c("text", "text", "text", 
                                                     "text", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric"))

# Rename columns that we already have in master dataset to make sure we can identify that they have come from Cipriani
colnames(df_cipriani)
columns_to_rename <- c("change", "change_sd", "change_n", "responders", "responders_n", "responders_n_missing")
df_cipriani <- df_cipriani %>%
  rename_with(~paste0("cip_", .), columns_to_rename)
df_cipriani

# A couple of things causing problems. For Almeida-Montes 2005, Eli Lilly 1986, Bristol-Myers Squibb 2002,
# GlaxoSmithKline 2009, von Knorring 2006 - these exist in Cipriani's dataset but not ours. 
# Almeida-Montes and Eli Lilly we haven't been able to find the original papers, hence not in our dataset.

# GlaxoSmithKline 2009, von Knorring 2006 - fix formatting differences.

df_cipriani <- df_cipriani %>%
  mutate(study = ifelse(study == "GlaxoSmithKline", "Paxil (GlaxoSmithKline)", study),
         study = ifelse(study == "von Knorring", "Von Knorring", study))

# Cipriani has different rows for each condition whereas we have one row per actv vs ctrl comparison. We will need to 
# subset his dataset to prepare to merge.

# Start with placebo

df_cipriani_ctrl <- df_cipriani %>%
  filter(type == "Placebo")
df_cipriani_ctrl <- df_cipriani_ctrl %>%
  rename_with(~paste0(.x, "_ctrl"), c("cip_change": "suicide_n")) %>% 
  rename(control_type = type)

 # And then active

df_cipriani_actv <- df_cipriani %>%
  filter(type != "Placebo")
df_cipriani_actv <- df_cipriani_actv %>%
  rename_with(~paste0(.x, "_actv"), c("cip_change": "suicide_n")) %>% 
  rename(active_type = type)

# Now join them both.
df_cipriani_form <- full_join(df_cipriani_actv, df_cipriani_ctrl, by = c("study", "year"))

# There are 3 studies that do not include a placebo. We will exclude these. 
df_cipriani_form <- df_cipriani_form %>% 
  filter(!is.na(control_type))

# Cipriani does not indicate which instrument his change scores apply to. However we do use the same 
# instrument hierarchy. I'm going to use df_first_row for this reason, which is our dataset which includes only the primary
# instrument for each comparison
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 
df_first_row <- full_join(df_first_row, df_cipriani_form, by = c("study", "year", "active_type", "control_type"))

# Now we want to join this back into master dataset
df_first_row_prepare <- df_first_row %>% 
  select(c(study_ID, instrument_name, study, year, active_type, control_type, cip_change_actv: suicide_n_actv, cip_change_ctrl: suicide_n_ctrl))
df_appl_v_orange <- full_join(df_appl_v_orange, df_first_row_prepare, by = c("study_ID", "instrument_name"))

# There are a few studies present in second dataset but not present in first (as we were not able to locate these studies). 
# Hence we only have Cipriani's data for these studies. 

df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    study = coalesce(study.x, study.y),
    year = coalesce(year.x, year.y),
    active_type = coalesce(active_type.x, active_type.y),
    control_type = coalesce(control_type.x, control_type.y)) %>%
  select(-study.x, -study.y, -year.x, -year.y, -active_type.x, -active_type.y, -control_type.x, -control_type.y) %>% 
  relocate(study, year, active_type, control_type, .after = "Column1")

# Checked dataframe, looks mostly good with a couple of problems. Some haven't joined correctly because there are errors in how the drug name is spelled. 
# I have corrected spelling errors in our original med dataset. 

# Wagner 2003 hasn't read in correctly because Cipriani has considered it two studies (Wager 2003a and b). I've gone back to the paper and
# though there were two studies these were pooled a priori and only combined data is reported. We have complete data for this study, so for now 
# adding Cipriani's data doesn't add anything. I'll just remove the duplicate rows for now.

df_appl_v_orange <- df_appl_v_orange %>% 
  filter(!(study == "Wagner" & (year == "2003a" | year == "2003b")))

# Now use Cipriani's change scores to fill in missing means

df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    baseline_mean_active = ifelse(psy_or_med == 0 & is.na(baseline_mean_active), post_mean_active - cip_change_actv, baseline_mean_active),
    baseline_mean_control = ifelse(psy_or_med == 0 & is.na(baseline_mean_control), post_mean_control - cip_change_ctrl, baseline_mean_control),
    post_mean_active = ifelse(psy_or_med == 0 & is.na(post_mean_active), baseline_mean_active + cip_change_actv, post_mean_active),
    post_mean_control = ifelse(psy_or_med == 0 & is.na(post_mean_control), baseline_mean_control + cip_change_ctrl, post_mean_control)
  )

# Again, lets recalculate cohens d now that we have more data
df_appl_v_orange <- df_appl_v_orange %>% 
  mutate(cohens_d_active = (post_mean_active - baseline_mean_active)/
           ((post_sd_active + baseline_sd_active)/2), cohens_d_control = (post_mean_control - baseline_mean_control)/
           ((post_sd_control + baseline_sd_control)/2))
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 
filter_test <- df_first_row %>% filter(psy_or_med == 0)
sum(!is.na(filter_test$cohens_d_active))
sum(!is.na(filter_test$cohens_d_control))
unique(filter_test$study_ID)

study_ids_with_missing_ds <- df_first_row %>%
  filter(is.na(cohens_d_active)) %>%
  filter(psy_or_med == 0) %>% 
  select(study_ID)

# We have cohens ds for 28/33 med studies. For Bristol Myers Squibb 2002a, Emslie 2002b, Hughes 1990 and Paxil GlaxoSmithKline 2009
# we will not be able to calculate ds due to very poor reporting in the original studies. No possible way to impute missing data.
# The other study is Emslie 2007(combined) which is actually misleading as it is a combination of 2007a and 2007b, however for two outcome measures
# data has been provided for the studies combined. We have complete data on the CDRS for the individual studies. Have checked with Argyris, we will remove this study. 

df_appl_v_orange <- df_appl_v_orange %>% 
  filter(study_ID != "Emslie_2007 (combined)_Venlafaxine_Placebo")

# Okay, I think that is done. We now have Cohens ds for 28/32 studies and that's the best we can do. 

# Let's look at missing ds for psy studies  -------------------------------

# Look at cohens d missing for psy studies
filter_test <- df_first_row %>% filter(psy_or_med == 1)
sum(!is.na(filter_test$cohens_d_active))
sum(!is.na(filter_test$cohens_d_control))

# We have cohens d for 58/66 studies. Let's identify the studies w missing data. 
study_ids_with_missing_ds <- df_first_row %>%
  filter(is.na(cohens_d_active)) %>%
  filter(psy_or_med == 1) %>% 
  select(study_ID)

# None of these 8 studies are included in Cuijpers' MA because their primary outcome measure is not continuous.
# For a few of them I may be able to extract some relevant data and impute other data using methods above. I can do this for Shomaker 2016 and Yu 2002. 
# I have extracted additional data and added to "FUll_Dataset_Cuijpers_MA".
# The 6 other studies do not report on a continuous outcome measure or do not provide sufficient data for us to impute missing means or SDs. See notes column
# of "columns_with_missing_values_updated" for specific descriptions of each study. 

# Shomaker 2016 has a missing sd at post, so I will perform imputation as above

df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    baseline_sd_active = ifelse(psy_or_med == 1 & is.na(baseline_sd_active), post_sd_active, baseline_sd_active),
    post_sd_active = ifelse(psy_or_med == 1 & is.na(post_sd_active), baseline_sd_active, post_sd_active),
    baseline_sd_control = ifelse(psy_or_med == 1 & is.na(baseline_sd_control), post_sd_control, baseline_sd_control),
    post_sd_control = ifelse(psy_or_med == 1 & is.na(post_sd_control), baseline_sd_control, post_sd_control)
  )

# And check whether we have more studies with cohens ds now
df_appl_v_orange <- df_appl_v_orange %>% 
  mutate(cohens_d_active = (post_mean_active - baseline_mean_active)/
           ((post_sd_active + baseline_sd_active)/2), cohens_d_control = (post_mean_control - baseline_mean_control)/
           ((post_sd_control + baseline_sd_control)/2))
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 
filter_test <- df_first_row %>% filter(psy_or_med == 1)
sum(!is.na(filter_test$cohens_d_active))
sum(!is.na(filter_test$cohens_d_control))
unique(filter_test$study_ID)

study_ids_with_missing_ds <- df_first_row %>%
  filter(is.na(cohens_d_active)) %>%
  filter(psy_or_med == 1) %>% 
  select(study_ID)

# Great, now we have Cohens ds for Shomaker 2016 and Yu 2002. Hence we now have cohens d for 60 / 66 psy studies. 

# Argyris and Charlotte discussed using change scores rather than pre and post means / sds as these are more easily accessible
# for psy studies change scores can be easily computed using baseline and post means. SDs of change scores can be computed according to Cochrane recource
# titled "Chapter 6: Choosing effect measures and computing estimates of effect" https://training.cochrane.org/handbook/current/chapter-06#section-6-5

# First calculate change scores for psy studies. I will calculate this as post score - baseline for consistency with Cipriani
df_appl_v_orange <- df_appl_v_orange %>% 
  mutate(change_active = ifelse(psy_or_med ==1, post_mean_active - baseline_mean_active, NA)) %>% 
  mutate(change_control = ifelse(psy_or_med ==1, post_mean_control - baseline_mean_control, NA))

# Now calculate the correlation coefficients. For this we need a study with complete data (i.e. SDs for pre, post, and change).
# We don't have this for psychotherapy (Cuijpers only reports pre and post), so let's check if we have this for any med studies
studies_w_complete_sds <- df_appl_v_orange %>%
  filter(psy_or_med == 0, complete.cases(baseline_sd_active, post_sd_active, active_sd_change))
  
# We only have this for one study. Hence we decided to stick with cohens d for now and perform the imputations above.


####### Argyris additions aide memoir
test <- df_appl_v_orange  %>% 
  group_by(study_ID) %>% 
  filter(instrument_value == min(instrument_value)) %>% 
  group_by(psy_or_med) %>% 
  summarise(av_cohens_d_ctrl = mean(cohens_d_control, na.rm = T), sd_cohens_d_ctrl = sd(cohens_d_control, na.rm = T),
            av_cohens_d_actuve = mean(cohens_d_active, na.rm = T), sd_cohens_d_active = sd(cohens_d_active, na.rm = T))

test

# create new id
test <- df_appl_v_orange  %>% 
  mutate(new_study_id = case_when(psy_or_med == 0 ~ paste(study,year, sep = ", "),
                        .default = study )) 




test[test$psy_or_med==0, ]$new_study_id == test[test$psy_or_med==0, ]$study
test[test$psy_or_med==1, ]$new_study_id == test[test$psy_or_med==1, ]$study      


# use this code only for examinig controls, for actives use distinct(active_type...)
test_dist_contrl<-  test %>%          
  group_by(new_study_id) %>% 
  filter(instrument_value == min(instrument_value)) %>% 
  distinct(control_type,.keep_all = TRUE)


test_dist_contrl[duplicated(test_dist_contrl$study_ID),]

test_dist_contrl[test_dist_contrl$new_study_id=="Atkinson, 2014",]

test_2 <- test[duplicated(test$study_ID),] #sorting out 
dim(test_2)

dups <- test_2$study_ID
test_3 <- test[test$study_ID %in% dups,]

head(test_3, 26)


test%>% 
  ggplot(aes(x = cohens_d_control))+
  geom_histogram(bins= 10)+
  facet_wrap(~psy_or_med, nrow = 2)



test%>% 
  ggplot(aes(y = cohens_d_control, x = as.factor(psy_or_med)))+
  geom_point(aes(y = cohens_d_control, x = as.factor(psy_or_med), size = baseline_n_control)) +
  geom_boxplot(alpha = 0.5)


test%>% 
  ggplot(aes(y = cohens_d_control, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
  geom_point(aes(y = cohens_d_control, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
  geom_violin(alpha = 0.5)+
  stat_summary(
    mapping = aes(y = cohens_d_control, x = as.factor(psy_or_med)),
    fun.min = function(z) { quantile(z,0.25) },
    fun.max = function(z) { quantile(z,0.75) },
    fun = median)+
  ggtitle("Effect Sizes of the Control Arms of Anti-depressant\n and Psychotherapy Trials in Adolescent Depression", subtitle = "with medians and IQRs")+
  ylab("Cohen's d of the primary outcome")+
  scale_x_discrete(labels=c("0" = "Anti-depressant trial\nControls", "1" = "Psychotherapy trial\nControls"))+
  theme(axis.text.x= element_text(size= 12)) +
  theme(axis.text.y= element_text(size= 12)) +
  theme(axis.title.y= element_text(size= 12))+
  theme(axis.title.x= element_blank())+
   theme(legend.position = "none")+
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") 
  theme(plot.title = element_text(color="black", size=14, face="bold")) +
  theme_minimal()

  



test %>% 
  filter(cohens_d_control<(-2) & psy_or_med == 1)


test

duplicated(test$study_ID)

test[test$study=="Bolton, 2007",]
