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
merged_dataset <- dplyr:: select(merged_dataset, -c(primary, hierarchy, time))
merged_dataset <- merged_dataset %>% relocate(descr_active, .after = control_type)
merged_dataset <- merged_dataset %>% relocate(descr_control, .after = descr_active)
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
      instrument %in% c("mfq") ~ "mfq",
      instrument %in% c("smfq") ~ "smfq",
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

# The code below should actually be implemented once we turn the dataframe to long, which we do in the quarto. So I will not run this code here. 

# # We have percent_women reported overall for psy trials but per arm for med trials
# #create overall percent women variable for all studies
# df_appl_v_orange <- df_appl_v_orange %>%  mutate(overall_percent_women = (active_percent_women * baseline_n_active + control_percent_women * baseline_n_control)
#                                                  /(baseline_n_active + baseline_n_control))
# df_appl_v_orange <- df_appl_v_orange %>%  mutate(percent_women = coalesce(percent_women, overall_percent_women))
# 
# # we have separate age variables for each arm and for overall. Where age is reported per arm, I will calculate an overall. 
# # I'm using an old variable name so I don't have the change the rest of the code
#  
# df_appl_v_orange <- df_appl_v_orange %>%  mutate(overall_mean_age = (age_m_active * baseline_n_active + age_m_control * baseline_n_control)
#                                                  /(baseline_n_active + baseline_n_control))
# df_appl_v_orange <- df_appl_v_orange %>%  mutate(mean_age = coalesce(age_m_overall, overall_mean_age))
# 
# # I'd like to create a pooled SD for age
# df_appl_v_orange <- df_appl_v_orange %>%  
#   mutate(pooled_sd_age = sqrt(  (   (baseline_n_active - 1)*((age_sd_active)^2)   + (baseline_n_control - 1)*((age_sd_control)^2)) /
#                                                                         (baseline_n_active + baseline_n_control - 2)))
# df_appl_v_orange <- df_appl_v_orange %>%  mutate(sd_age = coalesce(age_sd_overall , pooled_sd_age))

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
 #           avg_age = mean(mean_age, na.rm = T),
            avg_baseline_mean_act = mean(baseline_mean_active, na.rm = T), 
            sd_baseline_mean_act = sd(baseline_mean_active, na.rm = T), 
            avg_baseline_mean_ctrl = mean(baseline_mean_control, na.rm = T), 
            sd_baseline_mean_ctrl = sd(baseline_mean_control, na.rm = T))

# for psychotherapy trials, average baseline mean scores for each instrument
psy_baseline_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), total_n_control = sum(baseline_n_control, na.rm = T), 
            total_n_active = sum(baseline_n_active, na.rm = T),
      #       avg_age = mean(mean_age, na.rm = T),
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
write.csv(df_first_row, "df_first_row.csv")

#check how many med comparisons we cannot calc cohens d for (for primary instrument)
filter_test <- df_first_row %>% filter(psy_or_med == 0)
sum(is.na(filter_test$cohens_d_active))
sum(is.na(filter_test$cohens_d_control))

# start looking at demographics, baseline and post means, and cohens d per arm
df_demographics <- df_first_row %>% dplyr:: select(study, year, psy_or_med, active_type, control_type, descr_active, descr_control,
                                           instrument_name, baseline_mean_active, baseline_sd_active, baseline_n_active,
                                           baseline_mean_control, baseline_sd_control, baseline_n_control, 
                                           post_mean_active, post_sd_active, post_n_active, post_mean_control, post_sd_control, post_n_control,
                                           cohens_d_active, cohens_d_control, percent_women)

# combine arm description variables across psy and med
df_demographics$descr_active <- ifelse(is.na(df_demographics$descr_active), df_demographics$active_type, df_demographics$descr_active)
df_demographics$descr_control <- ifelse(is.na(df_demographics$descr_control), df_demographics$control_type, df_demographics$descr_control)

df_demographics <- df_demographics %>%  dplyr::select(-c(active_type, control_type))
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
#  mean_age = mean(mean_age, na.rm = TRUE), 
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

# df_demographics %>% 
#   group_by(psy_or_med ) %>% 
#   summarise (avg_age = mean(mean_age, na.rm = T), sd_age = sd(mean_age, na.rm = T))

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


# Identify which columns have missing values for each study
columns_with_missing_values <- df_appl_v_orange %>%
  filter(rowSums(is.na(.[columns_to_check])) > 0) %>%
  dplyr::select(psy_or_med, study, year, where(function(x) any(is.na(x))))

# Print columns with missing values for each study THROWS UP ERROR TOLD CHARLOTTE
# tapply(print(columns_with_missing_values[,1:2]
# ), columns_with_missing_values$psy_or_med)


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
    rows_with_na_sds = sum(rowSums(is.na(dplyr::select(., c(baseline_sd_active , post_sd_active, baseline_sd_control, post_sd_control)))) > 0)
  )
df_first_row_missing_sds

# Now check how many rows with missing means
df_first_row_missing_means <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_means = sum(rowSums(is.na(dplyr::select(., c(baseline_mean_active , post_mean_active, baseline_mean_control, post_mean_control)))) > 0)
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
    rows_with_na_sds = sum(rowSums(is.na(dplyr::select(., c(baseline_sd_active , post_sd_active, baseline_sd_control, post_sd_control)))) > 0)
  )
df_first_row_missing_sds

# We have gone from 21 to 8 studies with missing SDs. Great!

# And check to see how many studies with missing means after performing imputation

df_first_row_missing_means <- df_first_row %>% 
  filter(psy_or_med == 0) %>% 
  summarise(
    rows_with_na_means = sum(rowSums(is.na(dplyr::select(., c(baseline_mean_active , post_mean_active, baseline_mean_control, post_mean_control)))) > 0)
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
  dplyr:: select(study, year, active_type, instrument_name, where(function(x) any(is.na(x))))
###tapply(print(new_columns_with_missing_values[,1:2]), new_columns_with_missing_values$psy_or_med) THROWS UP ERROR TOO
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
  dplyr::select(c(study_ID, instrument_name, study, year, active_type, control_type, cip_change_actv: suicide_n_actv, cip_change_ctrl: suicide_n_ctrl))
df_appl_v_orange <- full_join(df_appl_v_orange, df_first_row_prepare, by = c("study_ID", "instrument_name"))

# There are a few studies present in second dataset but not present in first (as we were not able to locate these studies). 
# Hence we only have Cipriani's data for these studies. 

df_appl_v_orange <- df_appl_v_orange %>%
  mutate(
    study = coalesce(study.x, study.y),
    year = coalesce(year.x, year.y),
    active_type = coalesce(active_type.x, active_type.y),
    control_type = coalesce(control_type.x, control_type.y)) %>%
  dplyr::select(-study.x, -study.y, -year.x, -year.y, -active_type.x, -active_type.y, -control_type.x, -control_type.y) %>% 
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
  dplyr::select(study_ID)

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
  dplyr::select(study_ID)

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
  dplyr::select(study_ID)

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

# save dataframe 
openxlsx:: write.xlsx(df_appl_v_orange, "df_appl_v_orange.xlsx") # you can use this now for the quarto document

############IGNORE from HERE to line 976

####### Argyris additions aide memoir
# test <- df_appl_v_orange  %>% 
#   group_by(study_ID) %>% 
#   filter(instrument_value == min(instrument_value)) %>% 
#   group_by(psy_or_med) %>% 
#   summarise(av_cohens_d_ctrl = mean(cohens_d_control, na.rm = T), sd_cohens_d_ctrl = sd(cohens_d_control, na.rm = T),
#             av_cohens_d_actuve = mean(cohens_d_active, na.rm = T), sd_cohens_d_active = sd(cohens_d_active, na.rm = T))
# 
# test
# 
# # create new id
# test <- df_appl_v_orange  %>% 
#   mutate(new_study_id = case_when(psy_or_med == 0 ~ paste(study,year, sep = ", "),
#                         .default = study )) 
# 
# 
# 
# 
# test[test$psy_or_med==0, ]$new_study_id == test[test$psy_or_med==0, ]$study
# test[test$psy_or_med==1, ]$new_study_id == test[test$psy_or_med==1, ]$study      
# 
# 
# # USE THIS CODE only for examinig CONTROLS, for actives use distinct(active_type...)
# 
# # discovered an error in the above for the percentage women o fthe Fristad study. I have checked in the
# # cuijpers dataset and the correct percentage is 43.1, but could not verify with the paper as it is not in 
# # our folder and after a quick search I could not find it online either. Messaged Charlotte on Discord to
# # check again.
# df_appl_v_orange[df_appl_v_orange$study_ID=="Fristad, 2019_cbt + placebo_placebo",]$percent_women <-43.1
# 
# 
# df_appl_v_orange <-df_appl_v_orange  %>% 
#   mutate(new_study_id = case_when(psy_or_med == 0 ~ paste(study,year, sep = ", "),
#                                   .default = study )) 
# 
# 
# # We also need to calculate SE for proportion women
# # for proportions, this is calculated as sqrt(p(1-p)/n), which I implement stepwise below
# 
# prodcut_perc_women <-  (df_appl_v_orange$percent_women/100)*
#   (1-(df_appl_v_orange$percent_women/100) ) 
# 
# total_n <- df_appl_v_orange$baseline_n_active + 
#   df_appl_v_orange$baseline_n_control
# 
# df_appl_v_orange$percent_women_std_error <- sqrt(prodcut_perc_women/total_women )
# 
# 
# # We also need to calculate SE for baseline severity
# 
# df_appl_v_orange$baseline_st_error_active <- df_appl_v_orange$baseline_sd_active/sqrt(df_appl_v_orange$baseline_n_active)
# 
# df_appl_v_orange$baseline_st_error_control <-df_appl_v_orange$baseline_sd_control/sqrt(df_appl_v_orange$baseline_n_control)
# 
# 
# 
# 
# test_dist_contrl<-  df_appl_v_orange %>%          
#   group_by(new_study_id) %>% 
#   filter(instrument_value == min(instrument_value)) %>% 
#   distinct(control_type,.keep_all = TRUE)
# 
# test_dist_contrl[duplicated(test_dist_contrl$study_ID),]
# 
# test_dist_contrl[test_dist_contrl$new_study_id=="Atkinson, 2014",]
# 
# test_2 <- test[duplicated(test$study_ID),] #sorting out 
# dim(test_2)
# 
# dups <- test_2$study_ID
# test_3 <- test[test$study_ID %in% dups,]
# 
# head(test_3, 26)
# 
# 
# 
# test_dist_contrl%>% 
#   ggplot(aes(y = cohens_d_control, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
#   geom_point(aes(y = cohens_d_control, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
#   geom_violin(alpha = 0.5)+
#   stat_summary(
#     mapping = aes(y = cohens_d_control, x = as.factor(psy_or_med)),
#     fun.min = function(z) { quantile(z,0.25) },
#     fun.max = function(z) { quantile(z,0.75) },
#     fun = median)+
#   ggtitle("Effect Sizes of the Control Arms of Anti-depressant\n and Psychotherapy Trials in Adolescent Depression", subtitle = "with medians and IQRs")+
#   ylab("Cohen's d of the primary outcome")+
#   scale_x_discrete(labels=c("0" = "Anti-depressant trial\nControls", "1" = "Psychotherapy trial\nControls"))+
#   theme(axis.text.x= element_text(size= 12)) +
#   theme(axis.text.y= element_text(size= 12)) +
#   theme(axis.title.y= element_text(size= 12))+
#   theme(axis.title.x= element_blank())+
#    theme(legend.position = "none")+
#   geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") 
#   theme(plot.title = element_text(color="black", size=14, face="bold")) +
#   theme_minimal()
# 
#   
#   test_dist_contrl%>% 
#     filter(instrument_name == "cdrs") %>% 
#     ggplot(aes(y = baseline_mean_control, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
#     geom_point(aes(y = baseline_mean_control, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
#     geom_violin(alpha = 0.5)+
#     stat_summary(
#       mapping = aes(y = baseline_mean_control, x = as.factor(psy_or_med)),
#       fun.min = function(z) { quantile(z,0.25) },
#       fun.max = function(z) { quantile(z,0.75) },
#       fun = median)
#   
#   
#   test_dist_contrl%>% 
#     filter(instrument_name == "hamd") %>% 
#     ggplot(aes(y = baseline_mean_control, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
#     geom_point(aes(y = baseline_mean_control, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
#     geom_violin(alpha = 0.5)+
#     stat_summary(
#       mapping = aes(y = baseline_mean_control, x = as.factor(psy_or_med)),
#       fun.min = function(z) { quantile(z,0.25) },
#       fun.max = function(z) { quantile(z,0.75) },
#       fun = median)
#   
# 
#   
#   test_dist_contrl%>% 
#     ggplot(aes(y = percent_women, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
#     geom_point(aes(y = percent_women, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
#     geom_violin(alpha = 0.5)+
#     stat_summary(
#       mapping = aes(y = percent_women, x = as.factor(psy_or_med)),
#       fun.min = function(z) { quantile(z,0.25) },
#       fun.max = function(z) { quantile(z,0.75) },
#       fun = median)  
#   t.test(test_dist_contrl$percent_women, test_dist_contrl$psy_or_med)  
#   
#   test_dist_contrl%>% 
#     ggplot(aes(y = mean_age, x = as.factor(psy_or_med), colour = as.factor(psy_or_med) ))+
#     geom_point(aes(y = mean_age, x = as.factor(psy_or_med), size = baseline_n_control),position = position_jitter(seed = .3, width = 0.08), alpha = 0.6) +
#     geom_violin(alpha = 0.5)+
#     stat_summary(
#       mapping = aes(y = mean_age, x = as.factor(psy_or_med)),
#       fun.min = function(z) { quantile(z,0.25) },
#       fun.max = function(z) { quantile(z,0.75) },
#       fun = median)  
# t.test(test_dist_contrl$mean_age, test_dist_contrl$psy_or_med)
#   
# 
#   test_dist_contrl%>% 
#     filter(instrument_name == "cdrs") %>% 
#     group_by(psy_or_med) %>% 
#     summarise(n= n(), avg_baseline = mean(baseline_mean_control, na.rm = T))
#   
#   
#   test_dist_contrl%>% 
#     filter(instrument_name == "hamd") %>% 
#     group_by(psy_or_med) %>% 
#     summarise(n= n(), avg_baseline = mean(baseline_mean_control, na.rm = T)) 
#   
#   test_dist_contrl%>% 
#     filter(instrument_name == "bdi") %>% 
#     group_by(psy_or_med) %>% 
#     summarise(n= n(), avg_baseline = mean(baseline_mean_control, na.rm = T)) 
#   
#   test_dist_contrl%>% 
#     filter(instrument_name == "ces_d") %>% 
#     group_by(psy_or_med) %>% 
#     summarise(n= n(), avg_baseline = mean(baseline_mean_control, na.rm = T)) 
#   
# unique(test_dist_contrl$instrument_name)
# 
#   
# 
#   summarise(n= n(), avg_gender = mean(baseline_mean_control, na.rm = T)) 
# 
# ###
#   library(tidyverse) # needed for 'glimpse'
#   library(dmetar)
#   library(meta)
#   
# 
# 
# met_perc_women <- metagen(TE = percent_women,
#                  seTE = percent_women_std_error,
#                  studlab = study,
#                  data = df_appl_v_orange,
#                  sm = "SMD",
#                  fixed = FALSE,
#                  random = TRUE,
#                  method.tau = "REML",
#                  hakn = TRUE,
#                  title = "percentage women across studies")
# update.meta(met_perc_women, 
#             subgroup =psy_or_med, 
#             tau.common = FALSE)
# 
# 
# 
# met_perc_women <- metagen(TE = percent_women,
#                           seTE = percent_women_std_error,
#                           studlab = study,
#                           data = df_appl_v_orange,
#                           sm = "SMD",
#                           fixed = FALSE,
#                           random = TRUE,
#                           method.tau = "REML",
#                           hakn = TRUE,
#                           title = "percentage women across studies")
# update.meta(met_perc_women, 
#             subgroup =psy_or_med, 
#             tau.common = FALSE)
# 
# 
# df_for_cdrs <- test_dist_contrl %>% 
#   filter(instrument_name == "cdrs")
# 
# met_baseline_severity_cdrs <- metagen(TE = baseline_mean_control,
#                           seTE = baseline_st_error_control,
#                           studlab = new_study_id,
#                           data = df_for_cdrs,
#                           sm = "MD",
#                           fixed = FALSE,
#                           random = TRUE,
#                           method.tau = "REML",
#                           hakn = TRUE,
#                           title = "percentage women across studies")
# update.meta(met_baseline_severity_cdrs, 
#             subgroup =psy_or_med, 
#             tau.common = FALSE)
# 
# 
# 
# df_for_hamd <- test_dist_contrl %>% 
#   filter(instrument_name == "hamd")
# 
# met_baseline_severity_hamd <- metagen(TE = baseline_mean_control,
#                                       seTE = baseline_st_error_control,
#                                       studlab = new_study_id,
#                                       data = df_for_hamd,
#                                       sm = "MD",
#                                       fixed = FALSE,
#                                       random = TRUE,
#                                       method.tau = "REML",
#                                       hakn = TRUE,
#                                       title = "percentage women across studies")
# update.meta(met_baseline_severity_hamd, 
#             subgroup =psy_or_med, 
#             tau.common = FALSE)
# 
# 
# 
# 
# 
#  
# pdf(file = "forestplot.pdf", width = 15, height = 20)
# forest.meta(met_cohens_control_cbt_fluox_esc, 
#             sortvar = TE,
#             prediction = TRUE, 
#             print.tau2 = FALSE,
#             order = order(df_cbt_fluox_esc$psy_or_med),
#              subgroup = TRUE,
#              subgroup.name = df_cbt_fluox_esc$psy_or_med,
#             test.subgroup.random,
#             test.effect.subgroup.random,
#             sep.subgroup = df_cbt_fluox_esc$psy_or_med,
#             leftlabs = c("Study", "g", "SE"))
# dev.off()
# 
# 
# pdf(file = "forestplot.pdf", width = 15, height = 20)
# par(mar = c(6, 6, 6, 6))
# forest.meta(met_cohens_control_cbt_fluox_esc, layout = "JAMA",main = "Bigger margin: 6, 6, 6, 6")
# dev.off()
# 
# 
# 
# met_cohens_control_cbt_fluox_esc <- metagen(TE = baseline_mean_control,
#                                             seTE = baseline_st_error_control,
#                                             studlab = new_study_id,
#                                             data = df_cbt_fluox_esc,
#                                             sm = "SMD",
#                                             fixed = FALSE,
#                                             random = TRUE,
#                                             method.tau = "REML",
#                                             hakn = TRUE,
#                                             title = "percentage women across studies")
# 
# 
# #### It is useful to have two metanalyses and combine them to one
# ### the idea is that each metanalysis has its own heterogeneity, tau, and that it is reasonable 
# ### to combine. 
# 
# 
# 

  



#######Important code########### latest as of 30th December 2023



# Start here------------------------------------------------------

#Need to use SMDs, ie our Cohen's d and then use Standard error of SMD, to achieve this
# I need reliabilities.

# note re: CDRS reliability from here https://www.liebertpub.com/doi/epdf/10.1089/104454601317261546 
# Using a   2-week interval, and different psychiatrists from the first to the second assessment, 
# Poz-nanski et    al. (1984) demonstrated high reliability (r=   0.86) 
# for the CDRS-R total score in 53 clinic-referred6- to 12-year-olds.


### A few more tidying things from Argyris before doing metanalyses
# # discovered an error in the percentage women o fthe Fristad study. I have checked in the
# # cuijpers dataset and the correct percentage is 43.1, but could not verify with the paper as it is not in 
# # our folder and after a quick search I could not find it online either. Messaged Charlotte on Discord to
# # check again.
df_appl_v_orange[df_appl_v_orange$study_ID=="Fristad, 2019_cbt + placebo_placebo",]$percent_women <-43.1



# Create SEs proportions for percentage women --------------------------------------------

# # We also need to calculate SE for proportion women for the baseline calculations
# # for proportions, this is calculated as sqrt(p(1-p)/n), which I implement stepwise below

product_perc_women <-  (df_appl_v_orange$percent_women/100)*
  (1-(df_appl_v_orange$percent_women/100) ) 

total_n <- df_appl_v_orange$baseline_n_active + 
  df_appl_v_orange$baseline_n_control

df_appl_v_orange$percent_women_std_error <- sqrt(product_perc_women/total_n )


# Calculate SE for baseline severity --------------------------------------


df_appl_v_orange$baseline_st_error_active <- 
  df_appl_v_orange$baseline_sd_active/sqrt(df_appl_v_orange$baseline_n_active)

df_appl_v_orange$baseline_st_error_control <-
  df_appl_v_orange$baseline_sd_control/sqrt(df_appl_v_orange$baseline_n_control)



# Turn into a long database with unique rows ------------------------------


### Important: create a dataset that will have unique control studies (see problem that we identified with Charlotte, 
#namely common control conditions)
# create new id with Charlotte to help with better identification and work with duplicates (see below) 
df_appl_v_orange  <- df_appl_v_orange  %>%
  mutate(new_study_id = case_when(psy_or_med == 0 ~ paste(study,year, sep = ", "),
                                  .default = study ))



# Step 1: keep only the rows with the top instrument in our hierarchy
df_with_distinct_instruments <-  df_appl_v_orange %>%          
  group_by(new_study_id) %>% 
  filter(instrument_value == min(instrument_value)) # coded for the smallest number to be best. 

# Step 2: turn dataframes to long
# A: turn long the rows with active
turn_long_active_type <- df_with_distinct_instruments %>% 
  dplyr:: select(new_study_id, active_type, psy_or_med, baseline_n_active, cohens_d_active) %>% 
  pivot_longer(cols = c(cohens_d_active), 
               names_to = "arm_effect_size", 
               values_to = "cohens_d") 

# also rename active_type to treatment for the merge below.
turn_long_active_type <- rename(turn_long_active_type,treatment = active_type,
                                baseline_n = baseline_n_active) 

# to illustrate the issue, here we have one study with two controls for which the active at the moment, also exists twice. 
turn_long_active_type[turn_long_active_type$new_study_id == "Stallard, 2012",]
# but here another one where the same study reasonably contributes two actives, fluoxetine and duloxetine.
turn_long_active_type[turn_long_active_type$new_study_id == "Atkinson, 2014",]


# we now need to go through each study id and remove duplicates
turn_long_active_type <-
  turn_long_active_type %>% 
  group_by(new_study_id) %>% 
  distinct(treatment, .keep_all = TRUE)

# Now checking if this worked with the studies that I used to illustrate the problem above. 
turn_long_active_type[turn_long_active_type$new_study_id == "Stallard, 2012",]
turn_long_active_type[turn_long_active_type$new_study_id == "Atkinson, 2014",]
# also check one which is single to make sure it is kept
turn_long_active_type[turn_long_active_type$new_study_id == "Ackerson, 1998",]



# B: turn long control rows
turn_long_control_type <- df_with_distinct_instruments %>% 
  dplyr:: select(new_study_id, control_type, psy_or_med, baseline_n_control, cohens_d_control) %>% 
  pivot_longer(cols = c(cohens_d_control), 
               names_to = "arm_effect_size", 
               values_to = "cohens_d") 

# also rename active_type to treatment for the merge below.
turn_long_control_type <- rename(turn_long_control_type,treatment = control_type,
                                 baseline_n = baseline_n_control) 

# to illustrate the issue, here we have one study with two controls that are reasonable. 
turn_long_control_type[turn_long_control_type$new_study_id == "Stallard, 2012",]
# but here another one where the same study reasonably contributes two placebos
turn_long_control_type[turn_long_control_type$new_study_id == "Atkinson, 2014",]


# we now need to go through each study id and remove duplicates
turn_long_control_type <-
  turn_long_control_type %>% 
  group_by(new_study_id) %>% 
  distinct(treatment, .keep_all = TRUE)

# Now checking if this worked with the studies that I used to illustrate the problem above. 
turn_long_control_type[turn_long_control_type$new_study_id == "Stallard, 2012",]
turn_long_control_type[turn_long_control_type$new_study_id == "Atkinson, 2014",]
# also check one which is single to make sure it is kept
turn_long_control_type[turn_long_control_type$new_study_id == "Ackerson, 1998",]


### Now merge the active and control datasets
df_long_for_metan <-rbind(turn_long_active_type, turn_long_control_type)
dim(df_long_for_metan ) #check dimension
#make sure no study lost
length(unique(df_long_for_metan$new_study_id ) ) == length(unique(df_appl_v_orange$new_study_id ) )

# Now check again the studies with muliple arms
# Now checking if this worked with the studies that I used to illustrate the problem above. 
df_long_for_metan[df_long_for_metan$new_study_id == "Stallard, 2012",]
df_long_for_metan[df_long_for_metan$new_study_id == "Atkinson, 2014",]
# also check one which is single to make sure it is kept
df_long_for_metan[df_long_for_metan$new_study_id == "Ackerson, 1998",]


# save this as the master dataframe  --------------------------------------
# use this datafram to perform all operations below in quarto
# ask Charlotte to check steps above (lines 986 to 1121)
write.csv(df_long_for_metan, "df_long_for_metan", row.names = F)

# describe studies
n_unique_studies <- length(unique(df_long_for_metan$new_study_id))
df_count_studies <- df_long_for_metan %>% 
  group_by(psy_or_med, arm_effect_size) %>% 
  count(is.na(cohens_d))


df_count_studies <-rename(df_count_studies, is_missing = `is.na(cohens_d)`)

df_count_studies <-
  df_count_studies %>% 
  mutate(condition = case_when(psy_or_med == 0 & arm_effect_size == "cohens_d_active" ~ "medication_active",
                             psy_or_med == 0 & arm_effect_size == "cohens_d_control" ~ "medication_control",
                             psy_or_med == 1 & arm_effect_size == "cohens_d_active" ~ "psychotherapy_active",
                             psy_or_med == 1 & arm_effect_size == "cohens_d_control" ~ "psychotherapy_control")
  )

df_count_studies[df_count_studies$condition == "medication_active"& df_count_studies$is_missing == FALSE,]$n

df_count_studies_not_missing <- df_count_studies[df_count_studies$is_missing == FALSE,]


# Generate simulations of standard errors ---------------------------------



########### simulate so that each study has a value from a distribution of 
######## of correlations. 

# I will generate random numbers per study id from a distribution with these parameters. 
# it is reasonable to generate one random correlation value per study as there is no reason why the correlation should
# systematically vary within studies

# I have created a function for this
library(truncnorm)


simulate_dataframes_for_st_errors <- function(df, num_repetitions, seed, n, a, b, mean, sd) { # n refers to the number
                                                                                              # of unique ids to which a correlation coef
                                                                                              # is allocated.
  set.seed(seed)
  
  # Empty list to store simulated dfs
  list_df_simulated <- list()
  
  for (i in 1:num_repetitions) {
    # Simulate the vector
    simulated_correlations_vector <- truncnorm::rtruncnorm(n, a, b, mean, sd)  #using the truncnorm to create correlations
    
    # Create a copy of the original dataframe
    df_simulated_copy <- df
    
    # Add/update the sims column
    df_simulated_copy$correlation_sim_values <- simulated_correlations_vector[match(df_simulated_copy$new_study_id, unique(df_simulated_copy$new_study_id))]
    
    # Remove duplicated columns
    df_simulated_copy <- df_simulated_copy[, !duplicated(colnames(df_simulated_copy))]
    
    # Calculate the ses
    df_simulated_copy <- df_simulated_copy %>% 
      group_by(new_study_id, baseline_n) %>% 
      mutate(simulated_se = sqrt(((2*(1-correlation_sim_values))/baseline_n) + 
                                   (cohens_d^2/(2*baseline_n))))
    
    # Add the simulated dataframe to the list
    list_df_simulated[[i]] <- df_simulated_copy
  }
  
  return(list_df_simulated)
}

list_df_simulated <-  simulate_dataframes_for_st_errors (df = df_long_for_metan, 
                                              num_repetitions = 1000, 
                                              seed  = 1974,
                                              n = length(unique(df_long_for_metan$new_study_id)), 
                                              a = 0.45, 
                                              b = .9, 
                                              mean = 0.65, 
                                              sd = 0.2)

#check this worked

list_df_simulated[[10]][,c("new_study_id", "correlation_sim_values", "simulated_se")] # looks right
summary(list_df_simulated[[1000]]$correlation_sim_values, na.rm = T) # as expected # the mean is around the parameter I gave



# Run metaregression ------------------------------------------------------
library(metafor)
# add a new multilevel variable for the regression

for(i in 1: length(list_df_simulated)){
  list_df_simulated[[i]] <- list_df_simulated[[i]] %>% 
    mutate(four_level_var = case_when(psy_or_med == 0 & arm_effect_size == "cohens_d_active" ~ "medication_active",
                                      psy_or_med == 0 & arm_effect_size == "cohens_d_control" ~ "medication_control",
                                      psy_or_med == 1 & arm_effect_size == "cohens_d_active" ~ "psychotherapy_active",
                                      psy_or_med == 1 & arm_effect_size == "cohens_d_control" ~ "psychotherapy_control")
    )
  
  list_df_simulated[[i]]$four_level_var <- factor(list_df_simulated[[i]]$four_level_var) # turn to factor
  
  # relevel so that medication control becomes the reference category for the regression
  list_df_simulated[[i]]$four_level_var <- relevel(list_df_simulated[[1]]$four_level_var, ref = "medication_control")
}


# check it worked
# list_df_simulated[[2]] %>% 
#   dplyr:: select(psy_or_med, arm_effect_size, four_level_var)



# specify the model 
model_1 <- as.formula(~ four_level_var)

# run the metaregression
list_model_1_meta_reg <- list()

for(i in 1: length(list_df_simulated)){
  
  list_model_1_meta_reg[[i]] <- rma(yi = cohens_d, 
                                    sei = simulated_se, 
                                    data = list_df_simulated[[i]] , 
                                    method = "ML", 
                                    mods = model_1 , 
                                    test = "knha")   
  
}

# list_model_1_meta_reg[[2]]


# Function to extract parameters (coefs, ses, CI) for each level of the dummy variable
# it is an awkward one, because I couldnt' come up with a better way to add the intercept to each coefficient
# given the named output of the built in coef function. 
# here I exctract the coeffcients
extract_coefficients_func <- function(df_with_coefs){
  list_coefs <- list()
  coefs <- 0
  coefficient_output <- 0
  
  for(i in 1: length(coefficients(df_with_coefs))){
    
    temp_vec <- c(0, rep(coefficients(df_with_coefs)[[1]], 
                         length(coefficients(df_with_coefs)) - 1 ))
    coefs[i] <- coef(df_with_coefs)[[i]]
    
    coefficient_output <- temp_vec + coefs
    
    st_error_output <- df_with_coefs$se
    
    df_coefficients <- data.frame(cbind(coefficients = coefficient_output, se = st_error_output ))
    
    df_coefficients$lower_bound <- df_coefficients$coefficients - 1.96*df_coefficients$se
    df_coefficients$upper_bound <- df_coefficients$coefficients + 1.96*df_coefficients$se
  }
  return (df_coefficients )
}


# dummy_var_means <- extract_coefficients_func(list_model_1_meta_reg[[1]])

list_dummy_var_means <-lapply(list_model_1_meta_reg, extract_coefficients_func)

# this is the way to get the means and SEs from the simulation across all datasets. 

df_mean_coefs_from_sim <- data.frame(
  
  condition = levels(list_df_simulated[[1]]$four_level_var),
  
  mean_coefficient = c(mean(sapply(list_dummy_var_means, function(df) df[1,1])),
                      mean(sapply(list_dummy_var_means, function(df) df[2,1])),
                      mean(sapply(list_dummy_var_means, function(df) df[3,1])),
                      mean(sapply(list_dummy_var_means, function(df) df[4,1]))),
  
  mean_se = c(mean(sapply(list_dummy_var_means, function(df) df[1,2])),
              mean(sapply(list_dummy_var_means, function(df) df[2,2])),
              mean(sapply(list_dummy_var_means, function(df) df[3,2])),
              mean(sapply(list_dummy_var_means, function(df) df[4,2])))
  
)

# and add the CIs

df_mean_coefs_from_sim$upper_ci <- df_mean_coefs_from_sim$mean_coefficient + 1.96 * df_mean_coefs_from_sim$mean_se

df_mean_coefs_from_sim$lower_ci <- df_mean_coefs_from_sim$mean_coefficient - 1.96 * df_mean_coefs_from_sim$mean_se
# df_mean_coefs_from_sim # this is now the dataframe containing the means you need for plotting.

#also, add the n of studies non missing from variable created above
#important first create the same condition variable: 
df_count_studies_not_missing$condition <- str_replace_all(df_count_studies_not_missing$condition, "_", " ")

# I needed to join them because of the relevelling, the rows would not align otherwise.
#df_mean_coefs_from_sim <- right_join(df_mean_coefs_from_sim, df_count_studies_not_missing)


# need to merge this with the count studies variable to obtain the ks
library(forcats)
ordering_criterion <- unique(df_mean_coefs_from_sim$condition)

# 
df_count_studies_not_missing <- df_count_studies_not_missing %>% arrange(ordering_criterion) %>%
  mutate(condition = fct_relevel(condition, levels = unique(condition)))
# I will use this for plotting below.



# plot results from simulated means ---------------------------------------

# Create summary stats text
df_mean_coefs_from_sim$text_label <- 0
for(i in 1: nrow(df_mean_coefs_from_sim))
df_mean_coefs_from_sim$text_label[i] <-  paste(
   "k = ", df_count_studies_not_missing $n[i],"\n", 
   round(df_mean_coefs_from_sim$mean_coefficient[i], 2),
   "[" ,
 round(df_mean_coefs_from_sim$lower_ci[i],2), 
 round(df_mean_coefs_from_sim$upper_ci[i], 2),
"]") 
                                                              

# recode condition for ease of plotting
df_mean_coefs_from_sim$condition <- str_replace_all(df_mean_coefs_from_sim$condition, "_", " ")

# encode colours
redish_palette <- c("medication control" = "deeppink", "medication active" = "deeppink3")
blueish_palette <- c("psychotherapy active" = "steelblue1", "psychotherapy control" = "steelblue3")




ggplot(df_mean_coefs_from_sim, aes(x = mean_coefficient, y = 1:nrow(df_mean_coefs_from_sim),
                                   colour = condition, label = text_label)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.5)) +
  geom_text(vjust = +1.5, size = 4) +  # Adjust vjust and size as needed
  scale_size_continuous(guide = "none") +
  guides(colour = FALSE) + 
  scale_color_manual(values = c(redish_palette, blueish_palette)) +
  theme_minimal() +
  labs(x = "TE-random", y = NULL, title = "Adolescent Depression Trial Efficacy by Treatment Type and Treatment Arm",
       subtitle = "metanalytically derived estimates of within group changes") +
  xlab("Standardized Mean Difference (SMD) with 95% CIs") +
  ylab("") +
  ylim(0, nrow(df_mean_coefs_from_sim) + 1) +
  xlim(-2.5, 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.5, colour = "grey") +
  geom_segment(x = -1.25, xend = -1.75, y = 4.7, yend = 4.7, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "grey") +
  geom_text(x = -1.55, y = 4.9, label = "More Effective", color = "grey", vjust = 0.5, hjust = 1) +
  geom_segment(x = -1.35, xend = -0.85, y = 4.9, yend = 4.9, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "grey") +
  geom_text(x = -1.05, y = 5.05, label = "Less Effective", color = "grey", vjust = 0.5, hjust = 0)+
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x  = element_text(size = 14),
        axis.text.y = (element_blank()),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))+
  geom_curve(aes(x = 0.18, y = 4.73, xend = 0.02, yend = 4.5), color = "grey", curvature = -0.2, arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_text(x = 0.2, y = 4.75, label = "Line of No Effect", color = "grey", vjust = 0.5, hjust = 0)









######BELOW IS MAINLY OLD CODE, stop here for the moment.

mod_1 <- as.formula(~ arm_effect_size)
mod_2 <- as.formula(~ psy_or_med + arm_effect_size)
mod_3 <-  as.formula(~ psy_or_med*arm_effect_size)


list_mod_1_meta_reg <- list()
list_mod_2_meta_reg <- list()
list_mod_3_meta_reg <- list()

for(i in 1: length(list_df_simulated)){
  
  list_mod_1_meta_reg[[i]] <- rma(yi = cohens_d, 
                                  sei = simulated_se, 
                                  data = list_df_simulated[[i]] , 
                                  method = "ML", 
                                  mods = mod_1 , 
                                  test = "knha")    
  
  
  list_mod_2_meta_reg[[i]] <- rma(yi = cohens_d, 
                                  sei = simulated_se, 
                                  data = list_df_simulated[[i]] , 
                                  method = "ML", 
                                  mods = mod_2, 
                                  test = "knha")
  
  
  list_mod_3_meta_reg[[i]] <- rma(yi = cohens_d, 
                                  sei = simulated_se, 
                                  data = list_df_simulated[[i]] , 
                                  method = "ML", 
                                  mods = mod_3, 
                                  test = "knha")
  
  
}




# Create a function to extract AICc and LRT values
extract_aicc_lrt <- function(anova_result) {
  aicc_r <- anova_result$fit.stats.r["AIC"][[1]]
  aicc_f <- anova_result$fit.stats.f["AIC"][[1]]
  lrt <- anova_result$LRT  # Assuming LRT is in the second row and fifth column
  p_value <- anova_result$pval
  return(cbind(aicc_f, aicc_r , lrt, p_value))
}


# Perform comparisons between list_mod_1_meta_reg and list_mod_2_meta_reg and 2 and 3
comparison_result_1_vs_2 <- list()
aicc_lrt_values_1_vs_2 <- list()
comparison_result_2_vs_3 <- list()
aicc_lrt_values_2_vs_3 <- list()

for (i in 1:length(list_df_simulated)) {
  comparison_result_1_vs_2[[i]] <- anova(list_mod_1_meta_reg[[i]], list_mod_2_meta_reg[[i]])
  
  aicc_lrt_values_1_vs_2[[i]] <- extract_aicc_lrt(comparison_result_1_vs_2[[i]])
  
  
  comparison_result_2_vs_3[[i]] <- anova(list_mod_2_meta_reg[[i]], list_mod_3_meta_reg[[i]])
  
  aicc_lrt_values_2_vs_3[[i]] <- extract_aicc_lrt(comparison_result_2_vs_3[[i]])
  
}

# dataframe for comparing models 1 and 2
df_mods_1_vs_2 <- data.frame(do.call("rbind", aicc_lrt_values_1_vs_2))

# dataframe for comparing models 2 and 3
df_mods_2_vs_3 <- data.frame(do.call("rbind", aicc_lrt_values_2_vs_3)) 
                            


# this code allows me to extract the coefficients for each model. I am doing it here for the first one
list_for_coefficients_winning_model <- list ()

for (i in 1:length(list_df_simulated)) {
  list_for_coefficients_winning_model[[i]] <-data.frame(broom:: tidy(list_mod_1_meta_reg[[i]]))
  
  names(list_for_coefficients_winning_model)[i] <-  paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|")[i]
  #paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|")
  
}


list_for_coefficients_winning_model[[1]]



coef_psy_or_med_main_effect <- 0
coef_psy_or_med_interaction <- 0

for(i in 1: length(list_for_coefficients_winning_model)){
  
  coef_psy_or_med_main_effect[i] <- list_for_coefficients_winning_model[[i]]$p.value[2]
  coef_psy_or_med_interaction[i] <- list_for_coefficients_winning_model[[i]]$p.value[4]
  
}

perc_signif_coef_psy_or_med_main_effect <- 
  (sum(coef_psy_or_med_main_effect<0.05) / length(coef_psy_or_med_main_effect))*100

perc_signif_coef_psy_or_med_interaction_random_sims <- 
  (sum(coef_psy_or_med_interaction<0.05) / length(coef_psy_or_med_interaction))*100



# Add to the dataset the weights from the RE metanalysis and CIs for plotting ----------------------------

# first obtain the CIs for each study
for(i in 1: length(list_df_simulated)){
list_df_simulated[[i]]$upper_conf_int <- with(list_df_simulated[[i]], cohens_d - (1.96*simulated_se))

list_df_simulated[[i]]$lower_conf_int <- with(list_df_simulated[[i]], cohens_d + (1.96*simulated_se))

list_df_simulated[[i]]$weights <-  weights(list_mod_3_meta_reg[[i]])  # this is the function to obtain the weights.

}



# Run individual metanalyses ----------------------------------------------

### you will also need to get the group estimates for the means and CIs of SMDs from individidual 
### metanalyses for each control and active. 

head(list_df_simulated[[i]])

test <- expand.grid(c("a", "b"), c("==0", "==1"))
test
eval(test[1,])



# Create the possible combinations of control/active and meds/therapy
combinations <- expand.grid(psy_or_med = c(0, 1), 
                            arm_effect_size = c("cohens_d_control", "cohens_d_active"))


# create new variables for labelling
combinations <- combinations %>% 
  mutate(treatment_type = case_when(psy_or_med == "0" ~ "anti-depressant", 
                                                psy_or_med == "1" ~ "psychotherapy"), 
         treatment_arm = case_when(arm_effect_size ==  "cohens_d_control" ~ "control", 
                                   arm_effect_size == "cohens_d_active" ~ "active"))


result <- list()
name_vec <- 0
# Loop over combos
for (i in 1: nrow(combinations)) {


  # Call the metagen function for each combo
  result[[i]] <- metagen(
    TE = cohens_d,
    seTE = simulated_se,
    studlab = new_study_id,
    data = list_df_simulated[[1]][list_df_simulated[[1]]$psy_or_med == combinations$psy_or_med[i] & 
                                    
                        list_df_simulated[[1]]$arm_effect_size == combinations$arm_effect_size[i],],
    sm = "SMD",
    fixed = FALSE,
    random = TRUE,
    method.tau = "REML",
    hakn = TRUE,
    title = paste(combinations[i, "treatment_type"], # provide names
                 combinations[i, "treatment_arm"])
  )
 
  name_vec[i] = paste(combinations[i, "treatment_type"], # provide names
                   combinations[i, "treatment_arm"])

}


## Here is a function I wrote to extrac the key results for graphing and tabulating.

extract_stats <- function(result) {
  # Check if the result is not NULL
  if (is.null(result)) {
    warning("Result is NULL. Returning NULL.")
    return(NULL)
  }
  
  # get the summary table with results
  statistics <- summary(result)
  
  # Extract specific stats
  extracted_stats <- c(
    k_study = statistics$k.study,
    TE_random = statistics$TE.random,
    lower_random = statistics$lower.random,
    upper_random = statistics$upper.random,
    pval = statistics$pval.random,
    tau2 = statistics$tau2,
    lower_tau2 = statistics$lower.tau2,
    upper_tau2 = statistics$upper.tau2
  )
  
  return(extracted_stats)
}

# create a dataframe with the results
statistics_indiv_metan <- lapply(result, extract_stats) 
statistics_indiv_metan <- data.frame(do.call("rbind", statistics_indiv_metan))
statistics_indiv_metan$condition <- name_vec

# also split the last column into two for ease of plotting
statistics_indiv_metan$treatment <- stringr::str_extract(statistics_indiv_metan$condition, stringr::regex(pattern = "[a-z,-]+(?=\\s)", ignore_case = F)) 
statistics_indiv_metan$arm <- stringr::str_remove(statistics_indiv_metan$condition, stringr::regex(pattern = "[a-z,-]+(?=\\s)", ignore_case = F)) 


  



# ATTEMPTS AT GRAPHING. ---------------------------------------------------


# Ιndividual forest plots of individual metanalyses ----------------------------------

# Generate PDF with a plot per page looping over the different metanalyses

for(i in 1: nrow(statistics_indiv_metan)){
  pdf_file <- paste0(statistics_indiv_metan$condition[i],".pdf")
  pdf(pdf_file, width = 15, height = 15)
  forest.meta(result[[i]], layout = "JAMA", sortvar = TE)
  
  grid:: grid.text(paste("Efficacy in the ", statistics_indiv_metan$condition[i], "arm"), .375, .97, gp=grid::gpar(cex=1.5))
  dev.off()
}
# now we have one pdf per plot in the project directory.


# plot the summary statistics ---------------------------------------------

# Create summary stats text

statistics_indiv_metan$text_label <-  c(
  paste(statistics_indiv_metan$condition[1],"\n","k = ", statistics_indiv_metan$k_study[1],"\n", "tau2 = ", round(statistics_indiv_metan$tau2[1],2) ), 
   
  paste(statistics_indiv_metan$condition[2],"\n","k = ", statistics_indiv_metan$k_study[2],"\n", "tau2 = ", round(statistics_indiv_metan$tau2[2],2) ), 
  
  paste(statistics_indiv_metan$condition[3],"\n","k = ", statistics_indiv_metan$k_study[3],"\n", "tau2 = ", round(statistics_indiv_metan$tau2[3],2) ), 
   
  paste(statistics_indiv_metan$condition[4],"\n","k = ", statistics_indiv_metan$k_study[4],"\n", "tau2 = ", round(statistics_indiv_metan$tau2[4],2) ) 
)



# Create ggplot with points and error bars
# Create ggplot with points, error bars, arrows, and text
redish_palette <- c("anti-depressant control" = "deeppink", "anti-depressant active" = "deeppink3")
blueish_palette <- c("psychotherapy control" = "steelblue1", "psychotherapy active" = "steelblue3")


ggplot(statistics_indiv_metan, aes(x = TE_random, y = 1:nrow(statistics_indiv_metan), colour = condition, label = text_label)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lower_random, xmax = upper_random), width = 0.2, position = position_dodge(0.5)) +
  geom_text(vjust = +1.5, size = 4) +  # Adjust vjust and size as needed
  scale_size_continuous(guide = "none") +
  guides(colour = FALSE) + 
  scale_color_manual(values = c(redish_palette, blueish_palette)) +
  theme_minimal() +
  labs(x = "TE-random", y = NULL, title = "Adolescent Depression Trial Efficacy by Treatment Type and Treatment Arm",
       subtitle = "metanalytically derived estimates of within group changes") +
  xlab("Standardized Mean Difference (SMD) with 95% CIs") +
  ylab("") +
  ylim(0, nrow(statistics_indiv_metan) + 1) +
  xlim(-2.5, 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.5, colour = "grey") +
  geom_segment(x = -1.25, xend = -1.75, y = 4.7, yend = 4.7, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "grey") +
  geom_text(x = -1.55, y = 4.9, label = "More Effective", color = "grey", vjust = 0.5, hjust = 1) +
  geom_segment(x = -1.35, xend = -0.85, y = 4.9, yend = 4.9, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "grey") +
  geom_text(x = -1.05, y = 5.05, label = "Less Effective", color = "grey", vjust = 0.5, hjust = 0)+
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x  = element_text(size = 14),
        axis.text.y = (element_blank()),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))+
  geom_curve(aes(x = 0.18, y = 4.73, xend = 0.02, yend = 4.5), color = "grey", curvature = -0.2, arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_text(x = 0.2, y = 4.75, label = "Line of No Effect", color = "grey", vjust = 0.5, hjust = 0)


# Custom made Forest plot I made in ggplot in ggplot. Consider using ----------------------------------

# 
# library(forcats)
# 
# 
# # data processing 
# list_df_simulated[[1]]$new_study_id_new <- list_df_simulated[[1]]$new_study_id
# 
# # prepare the label
# list_df_simulated[[1]]$labels_for_facet <- factor(list_df_simulated[[1]]$psy_or_med, levels = c(0, 1),
#                                                   labels = c("Control Arms of Anti-depressant trials" , "Control Arms of Psychotherapy trials"))
# 
# # Dataset just for controls only here
# list_df_simulated_for_controls_plot <- list_df_simulated[[1]] %>%
#   filter(arm_effect_size == "cohens_d_control") %>%
#   group_by(psy_or_med)
# 
# # this attaches an extra number to the studies with two control arms
# list_df_simulated_for_controls_plot <- within(list_df_simulated_for_controls_plot, new_study_id <- ave(new_study_id, new_study_id_new, FUN = make.unique))
# 
# # this doesn't work at the moment, need to fix it because the y-axis changes around.
# # max_study_counts <- list_df_simulated_for_controls_plot %>%
# #   filter(arm_effect_size == "cohens_d_control") %>%
# #   group_by(psy_or_med) %>%
# #   summarise(max_study_count = max(as.numeric(new_study_id)))
# 
# 
# p_new <- list_df_simulated_for_controls_plot %>% 
#   filter(arm_effect_size == "cohens_d_control") %>% 
#   filter(!is.na(cohens_d)) %>% 
#   mutate(new_study_id = fct_reorder(new_study_id, cohens_d, .desc = TRUE)) %>%  #this to reorder
#   ggplot(aes(y = reorder(new_study_id, cohens_d))) + #this too
#   theme_classic()
# 
# # Your plotting code with facet_wrap
# p_new <- p_new +
#   geom_point(aes(x = cohens_d, colour = factor(psy_or_med)), shape = 15, size = 4) +
#   geom_linerange(aes(xmin = lower_conf_int, xmax = upper_conf_int)) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(x = "Standardised Mean Difference (negative is better)", y = "") +
#   guides(color = "none") +
#   ggtitle("Metanalysis of Depression Therapy: Within Group Change in the Control Conditions") +
#   scale_colour_manual(values = c("red", "blue")) +
#   facet_wrap(~ labels_for_facet, scales = "free_y", ncol = 1) +
#   # coord_cartesian(ylim = c(1, max(max_study_counts$max_study_count)))+
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),  # Set facet label size
#     plot.title = element_text(size = 18)  # Set plot title size
#   )
# 
# df_for_plotting_Sriv <- list_df_simulated_for_controls_plot %>%
#   filter(arm_effect_size == "cohens_d_control" & new_study_id == "Srivastava, 2020") 
# 
# p_new <-p_new  + xlim(-3.8,1)
# 
# p_new <- p_new +  geom_segment(aes(x = cohens_d, xend = lower_conf_int, y = "Srivastava, 2020", 
#                                    yend = "Srivastava, 2020"), 
#                                data = df_for_plotting_Sriv) 
# p_new <- p_new +    geom_segment(aes(x = cohens_d, xend = -3.6, y = "Srivastava, 2020", 
#                                      yend = "Srivastava, 2020"), arrow = arrow(length = unit(0.01, "npc")),
#                                  data = df_for_plotting_Sriv)  
# 
# pdf(file = "forestplot_from_metareg_control.pdf", width = 15, height = 20)
# p_new 
# dev.off()
# 
# 
# #### Same graph for the active condition
# 
# # data processing 
# list_df_simulated[[1]]$new_study_id_new <- list_df_simulated[[1]]$new_study_id
# 
# list_df_simulated[[1]]$labels_for_facet <- factor(list_df_simulated[[1]]$psy_or_med, levels = c(0, 1),
#                                                   labels = c("Active Arms of Anti-depressant trials" , "Active Arms of Psychotherapy trials"))
# 
# # Calculate maximum number of studies for each psy_or_med group
# list_df_simulated_for_active_plot <- list_df_simulated[[1]] %>%
#   filter(arm_effect_size == "cohens_d_active") %>%
#   group_by(psy_or_med)
# 
# list_df_simulated_for_active_plot <- within(list_df_simulated_for_active_plot, new_study_id <- ave(new_study_id, new_study_id_new, FUN = make.unique))
# 
# # max_study_counts <- list_df_simulated_for_controls_plot %>%
# #   filter(arm_effect_size == "cohens_d_control") %>%
# #   group_by(psy_or_med) %>%
# #   summarise(max_study_count = max(as.numeric(new_study_id)))
# 
# 
# p_new <- list_df_simulated_for_active_plot %>% 
#   filter(arm_effect_size == "cohens_d_active") %>% 
#   filter(!is.na(cohens_d)) %>% 
#   mutate(new_study_id = fct_reorder(new_study_id, cohens_d, .desc = TRUE)) %>%
#   ggplot(aes(y = reorder(new_study_id, cohens_d))) + 
#   theme_classic()
# 
# # Your plotting code with facet_wrap
# p_new <- p_new +
#   geom_point(aes(x = cohens_d, colour = factor(psy_or_med)), shape = 15, size = 4) +
#   geom_linerange(aes(xmin = lower_conf_int, xmax = upper_conf_int)) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(x = "Standardised Mean Difference (negative is better)", y = "") +
#   guides(color = "none") +
#   ggtitle("Metanalysis of Depression Therapy: Within Group Change in the Active Conditions") +
#   scale_colour_manual(values = c("red", "blue")) +
#   facet_wrap(~ labels_for_facet, scales = "free_y", ncol = 1) +
#   # coord_cartesian(ylim = c(1, max(max_study_counts$max_study_count)))+
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),  # Set facet label size
#     plot.title = element_text(size = 18)  # Set plot title size
#   )
# 
# df_for_plotting_Sriv <- list_df_simulated_for_active_plot %>%
#   filter(arm_effect_size == "cohens_d_active" & new_study_id == "Srivastava, 2020") 
# 
# p_new <-p_new  + xlim(-3.8,1)
# 
# p_new <- p_new +  geom_segment(aes(x = cohens_d, xend = lower_conf_int, y = "Srivastava, 2020", 
#                                    yend = "Srivastava, 2020"), 
#                                data = df_for_plotting_Sriv) 
# p_new <- p_new +    geom_segment(aes(x = cohens_d, xend = -3.6, y = "Srivastava, 2020", 
#                                      yend = "Srivastava, 2020"), arrow = arrow(length = unit(0.01, "npc")),
#                                  data = df_for_plotting_Sriv)  
# 
# pdf(file = "forestplot_from_metareg_active.pdf", width = 15, height = 20)
# p_new 
# dev.off()
# 
# 
# 
# 
# 
# 


#### Explore a few more things
####Year
list_df_simulated[[1]]$year <- as.integer(str_extract(list_df_simulated[[1]]$new_study_id, "\\d+"))
list_df_simulated[[1]] %>% 
     filter(arm_effect_size == "cohens_d_control") %>% 
     ggplot(aes(year, cohens_d, colour = as.factor(psy_or_med)))+
       geom_point()

list_df_simulated[[1]] %>% 
  filter(arm_effect_size == "cohens_d_control") %>% 
  ggplot(aes(baseline_n, cohens_d, colour = as.factor(psy_or_med)))+
  geom_point()




# ############ARGYRIS SOME OLD CODE, KEEP FOR THE TIME BEING##########
# 
# # Start here------------------------------------------------------
# 
# #Need to use SMDs, ie our Cohen's d and then use Standard error of SMD, to achieve this
# # I need reliabilities.
# 
# # note re: CDRS reliability from here https://www.liebertpub.com/doi/epdf/10.1089/104454601317261546 
# # Using a   2-week interval, and different psychiatrists from the first to the second assessment, 
# # Poz-nanski et    al. (1984) demonstrated high reliability (r=   0.86) 
# # for the CDRS-R total score in 53 clinic-referred6- to 12-year-olds.
# 
# 
# # this is a loop that creates SEs for various test-retest correlation coefficients
# 
# # the formula for the calculation of the standard error
# # comes from here: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html#s-md
# 
# range_test_retest_corr <- seq(0.5, 0.9, by =0.1)
# list_SE_active <- list()
# list_SE_control <- list()
# for (i in 1: length(range_test_retest_corr)){
# 
#   list_SE_active[[i]] <- sqrt(((2*(1-range_test_retest_corr[i]))/df_appl_v_orange$baseline_n_active) + 
#                                       ( df_appl_v_orange$cohens_d_active^2/(2*df_appl_v_orange$baseline_n_active)))
#   
#   list_SE_control[[i]] <- sqrt(((2*(1-range_test_retest_corr[i]))/df_appl_v_orange$baseline_n_control) + 
#                                 ( df_appl_v_orange$cohens_d_control^2/(2*df_appl_v_orange$baseline_n_control)))
#   
#   
# }
# 
# df_ses <- do.call("cbind", c(list_SE_active, list_SE_control))
# colnames(df_ses) <- c(paste0("se_change_active_",range_test_retest_corr ), paste0("se_change_control_",range_test_retest_corr ))
# df_appl_v_orange <-  cbind(df_appl_v_orange, df_ses )
# 
# 
# ### A few more tidying things from Argyris before doing metanalyses
# # # discovered an error in the percentage women o fthe Fristad study. I have checked in the
# # # cuijpers dataset and the correct percentage is 43.1, but could not verify with the paper as it is not in 
# # # our folder and after a quick search I could not find it online either. Messaged Charlotte on Discord to
# # # check again.
# df_appl_v_orange[df_appl_v_orange$study_ID=="Fristad, 2019_cbt + placebo_placebo",]$percent_women <-43.1
# 
# 
# # # We also need to calculate SE for proportion women
# # # for proportions, this is calculated as sqrt(p(1-p)/n), which I implement stepwise below
# 
# product_perc_women <-  (df_appl_v_orange$percent_women/100)*
#   (1-(df_appl_v_orange$percent_women/100) ) 
# 
# total_n <- df_appl_v_orange$baseline_n_active + 
#   df_appl_v_orange$baseline_n_control
# 
# df_appl_v_orange$percent_women_std_error <- sqrt(product_perc_women/total_n )
# 
# # # We also need to calculate SE for baseline severity
# # 
# df_appl_v_orange$baseline_st_error_active <- 
#   df_appl_v_orange$baseline_sd_active/sqrt(df_appl_v_orange$baseline_n_active)
# # 
# df_appl_v_orange$baseline_st_error_control <-
#   df_appl_v_orange$baseline_sd_control/sqrt(df_appl_v_orange$baseline_n_control)
# # 
# 
# 
# ### Important: create a dataset that will have unique control studies (see problem that we identified with Charlotte, 
# # namely common control conditions)
# 
# # create new id with Charlotte to help with better identification and work with duplicates (see below) 
# df_appl_v_orange  <- df_appl_v_orange  %>%
#   mutate(new_study_id = case_when(psy_or_med == 0 ~ paste(study,year, sep = ", "),
#                                   .default = study ))
# 
# 
# # check this is right
# df_appl_v_orange [df_appl_v_orange $psy_or_med==0, ]$new_study_id == 
#   df_appl_v_orange [df_appl_v_orange $psy_or_med==0, ]$study
# 
# df_appl_v_orange [df_appl_v_orange $psy_or_med==1, ]$new_study_id == 
#   df_appl_v_orange[df_appl_v_orange $psy_or_med==1, ]$study
# 
# 
# # now create the distinct dataset
# df_appl_v_orange_distinct_control <-  df_appl_v_orange %>%          
#   group_by(new_study_id) %>% 
#   filter(instrument_value == min(instrument_value)) %>% 
#   distinct(control_type,.keep_all = TRUE)
# 
# # check that all are unique studies
# df_appl_v_orange_distinct_control [duplicated(df_appl_v_orange_distinct_control $study_ID),] 
# 
# # check that the common control (placebo) was kept for a study that had two actives and one control                                                        
# df_appl_v_orange_distinct_control [df_appl_v_orange_distinct_control $new_study_id=="Atkinson, 2014",]
# 
# 
# 
# # now create a new dataframe that will only have the new_study_id, the cohensd and the ses
# 
# df_for_test_control_change <- df_appl_v_orange_distinct_control %>% 
#   dplyr:: select (new_study_id, cohens_d_control, psy_or_med, active_type, starts_with("se_change_control"))
# 
# colnames(df_for_test_control_change)
# 
# 
# 
# 
# 
# 
# # # # 1a. Separate metanalyses for cohens d in keeping with 
# # # https://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates
# # 
# # med_dataset_es <- df_for_test_control_change %>% 
# #    filter(psy_or_med == 0)
# #  med_dataset_es$yi <- med_dataset_es$cohens_d_control
# #  med_dataset_es$vi <- med_dataset_es$se_change_control_0.7
# #  
# #  
# # psy_dataset_es <- df_for_test_control_change %>% 
# #    filter(psy_or_med == 1)
# # psy_dataset_es$yi <- psy_dataset_es$cohens_d_control
# # psy_dataset_es$vi <- psy_dataset_es$se_change_control_0.7 
# #  
# #  
# # # now run the metanalyses using these different datasets
# # 
# # rand_eff_metanalysis_med_cohens_d <- metafor:: rma(yi, vi , data = med_dataset_es)
# # rand_eff_metanalysis_psy_cohens_d <- metafor:: rma(yi, vi,  data = psy_dataset_es)
# # 
# # # now comnbine their estimates
# #  df_combine_meta_psy_med <- data.frame(estimate = c(coef(rand_eff_metanalysis_med_cohens_d ), 
# #                                                     coef(rand_eff_metanalysis_psy_cohens_d)), 
# #                                        stderror = c(rand_eff_metanalysis_med_cohens_d$se, rand_eff_metanalysis_psy_cohens_d$se),
# #                                        meta = c("medication","psychotherapy"), tau2 = round(c(rand_eff_metanalysis_med_cohens_d$tau2, 
# #                                                                                               rand_eff_metanalysis_psy_cohens_d$tau2),3))
# # # and reanalyse
# #  combine_meta_psy_med <- metafor:: rma(estimate, sei=stderror, mods = ~ meta, 
# #                                        method="FE", data=df_combine_meta_psy_med, digits=3)
# #  
# # 
# # summary(combine_meta_psy_med) # notice the zvalue is identical with that above. 
# #  
# # z_value <- with(df_combine_meta_psy_med, 
# #                  round(c(zval = (estimate[1] - estimate[2])/sqrt(stderror[1]^2 + stderror[2]^2)), 3))
# # 
# # 
# # 
# # forest(rand_eff_metanalysis_psy_cohens_d, order = "obs", leftlabs = "new_study_id")
# # forest(rand_eff_metanalysis_med_cohens_d, order = "obs", leftlabs = "new_study_id")
# # 
# 
# 
# # 1b. Another way of doing this according to: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/subgroup.html
# 
# # first creating a variable with string labels to make easier the labelling
# library(metafor)
# library(dmetar)
# library(meta)
#    
# df_for_test_control_change$treatment_modality <-
#                   ifelse(df_for_test_control_change$psy_or_med == 0, "pill placebo",
#                          "psy control")
# 
# # also exclude the NA rows 
# df_for_test_control_change <- df_for_test_control_change %>% 
#   filter_at(vars(cohens_d_control, se_change_control_0.7), all_vars(!is.na(.))) # chose one correlation will loop over others
#   
#   
# 
# metan_overall <- metagen(TE = cohens_d_control,
#                  seTE = se_change_control_0.7,
#                  studlab = new_study_id,
#                  data = df_for_test_control_change,
#                  sm = "SMD",
#                  fixed = FALSE,
#                  random = TRUE,
#                  method.tau = "REML",
#                  hakn = TRUE,
#                  title = "Apples vs Oranges")
# 
# 
# #### Now you can run the subgroup metanalysis here and extract the estimates for that
# 
# subgroup_metan <-update.meta(metan_overall , 
#                              subgroup = df_for_test_control_change$treatment_modality, subgroup.name = "treatment modality",
#                              tau.common = FALSE)
# 
# 
# 
# # summary(subgroup_metan)[[40]]
# # summary(subgroup_metan)[[46]]
# # summary(subgroup_metan)[[47]]
# 
# summary(subgroup_metan)[21]$random # use another $ to get access to the between group difference, e.g. 
# summary(subgroup_metan)[21]$random$TE #is the between group effect and this is the SE
# summary(subgroup_metan)[21]$random$seTE # etc
# summary(subgroup_metan)[146] # this gives you the random effects for each group and
# summary(subgroup_metan)[146]$TE.random.w[1] # this woudl give you psychotherapy
# summary(subgroup_metan)[149]  # get p-value
# summary(subgroup_metan)[151] # get lower CI
# summary(subgroup_metan)[152]# get upper CI
# summary(subgroup_metan)[185] # USE THIS FOR THE Q value
# summary(subgroup_metan)[188]# Q value df
# summary(subgroup_metan)[190] # Q value p-value
# 
# 
# 
# pdf(file = "forestplot_2.pdf", width = 10, height = 23)
# forest(subgroup_metan, layout = "JAMA",
#        col.by = "black", 
#        digits.sd = 2,
#        digits.tau2 = 2,
#        colgap = "0.5cm",
#        order = "obs",
#        xlim=c(-4,1), cex=0.9,
#        fonts = 12,
#        colgap.forest = "1cm",)
# dev.off()
# 
# 
# ###### but it may be better to create the ggplot yourself as below using the metagen object
# summary(metan_overall)
# 
# df_for_plotting <- data.frame(
# names_to_change = summary(metan_overall)[[1]], # just a trick to avoid duplicates see below
# names = summary(metan_overall)[[1]],
# treatment_modality = df_for_test_control_change[df_for_test_control_change$new_study_id %in% summary(metan_overall)[[1]], ]
# $treatment_modality,
# smds = summary(metan_overall)[[4]], # the SMDs
# lower_ci = summary(metan_overall)[[10]], # lower CIs
# upper_ci = summary(metan_overall)[[11]]# uppper CIs
# )
# 
# # Also, to avoid the problem of duplicate values, try the following
# df_for_plotting  <- within(df_for_plotting , names <- ave(names, names_to_change, FUN = make.unique))
# 
# # Also add two empty rows to add the stats for the groups
# # df_for_plotting [nrow(df_for_plotting )+2,] <- NA
# 
# # try this to annotate the create the plot in ggplot
# # https://www.khstats.com/blog/forest-plots/
# 
# library(forcats)
# 
# 
# 
# p <- df_for_plotting %>% 
#   mutate(names = fct_reorder(names, smds)) %>%
#   ggplot(aes(y = names)) + 
#   theme_classic()
# 
# p <- p +
#   geom_point(aes(x=smds, colour = factor(treatment_modality)), shape=15, size=4) +
#   geom_linerange(aes(xmin=lower_ci, xmax=upper_ci)) 
# p <- p 
#   
# 
# 
# p <- p +
#   geom_vline(xintercept = 0, linetype="dashed") +
#   labs(x="Standardised Mean Difference (negative is better)", y="")
# p <- p + ggtitle ("Metanalysis of Control Arms of Antidepressant and Psychotherapy Trials",
#                   subtitle = "Standardised Mead Differences (SMDs) with 95% CIs")
# 
# 
#  p <- p +
#    coord_cartesian(ylim=c(1,78))
#  p <-  p + scale_colour_manual(values = c("red", "blue"))
# 
#   p <-  p +
#      annotate("rect", xmin = -1.96, xmax = -1.44, ymin = "Srivastava, 2020", ymax = "Rohde, 2004",
#               alpha = .4,fill = "red")+
#    
#    geom_vline(xintercept = summary(subgroup_metan)[146]$TE.random.w[[2]], colour = "white")
#  
#   p <- p  + 
#      annotate("rect", xmin = -0.6393278, xmax =-0.3501107, ymin = "Srivastava, 2020", ymax = "Rohde, 2004",
#               alpha = .3,fill = "blue" ) +
#    geom_vline(xintercept = summary(subgroup_metan)[146]$TE.random.w[[1]], colour = "white")
# # 
# p <- p + xlim(-3.8,1)
# p <- p + theme(legend.title=element_blank()) +   theme(legend.text=element_text(size=12))
# p <-p + theme(
#   legend.position = c(.03, .5),
#   legend.justification = c("left", "bottom")
# )
# 
# 
# # 
# p <- p + annotate("text", label = 
#                     paste0("group difference:\n", "chi_sq = ", 
#                                          round(summary(subgroup_metan)[185][[1]],2),",\np <0.01 "), 
#                   x = -3, y = "Diamond, 2002", size = 8)
# 
# df_for_plotting_Sriv <- df_for_plotting %>% 
#   filter(names == "Srivastava, 2020")
# 
# p <- p+ geom_segment(aes(x = smds, xend = upper_ci, y = "Srivastava, 2020", 
#                     yend = "Srivastava, 2020"), 
#                 data = df_for_plotting_Sriv)
# 
# 
# p <- p+ geom_segment(aes(x = smds, xend = -3.8, y = "Srivastava, 2020", 
#                       yend = "Srivastava, 2020"), arrow = arrow(length = unit(0.01, "npc")),
#                   data = df_for_plotting_Sriv)
# p <- p + 
#   theme(legend.text = element_text(size=30)) +
#   theme(legend.key.height= unit(2, 'cm')) +
#   theme(axis.text.x=element_text(size=12))+
#   theme(axis.text.y = element_text(size=12))+
#   theme(plot.title=element_text(size=15),
#   plot.subtitle=element_text(size=12))
# 
# p
# # now print as pdf
# 
# pdf(file = "forestplot.pdf", width = 10, height = 20)
# p
# dev.off()
# 
# 
# 
# 
# # 1c. Do the above for CBT and Fluox Escitalopram only
# 
# # # first creating a variable with string labels to make easier the labelling  DONE THIS ABOVE
# # df_for_test_control_change$treatment_modality <-
# #    ifelse(df_for_test_control_change$psy_or_med == 0, "placebo",
# #           "psy control")
# 
# # also exclude the NA rows DONE THIS ABOVE
# # df_for_test_control_change_sri_cbt <- df_for_test_control_change %>% 
# #   filter_at(vars(cohens_d_control, se_change_control_0.7), all_vars(!is.na(.))) # chose one correlation will loop over others
# # 
# 
# # create a new dataset 
# df_for_test_control_change_sri_cbt <- 
#   df_for_test_control_change %>% 
#   filter(active_type == "Fluoxetine"|active_type == "Escitalopram" |active_type == "cbt")
# 
# metan_overall_sri_cbt <- metagen(TE = cohens_d_control,
#                          seTE = se_change_control_0.7,
#                          studlab = new_study_id,
#                          data = df_for_test_control_change_sri_cbt,
#                          sm = "SMD",
#                          fixed = FALSE,
#                          random = TRUE,
#                          method.tau = "REML",
#                          hakn = TRUE,
#                          title = "Apples vs Oranges SRI CBT")
# 
# 
# #### Now you can run the subgroup metanalysis here and extract the estimates for that
# 
# subgroup_metan_sri_cbt <-update.meta(metan_overall_sri_cbt , 
#                              subgroup = df_for_test_control_change_sri_cbt$treatment_modality, subgroup.name = "treatment modality",
#                              tau.common = FALSE)
# 
# # summary(subgroup_metan)[[40]]
# # summary(subgroup_metan)[[46]]
# # summary(subgroup_metan)[[47]]
# 
# summary(subgroup_metan_sri_cbt)[21]$random # use another $ to get access to the between group difference, e.g. 
# summary(subgroup_metan_sri_cbt)[21]$random$TE #is the between group effect and this is the SE
# summary(subgroup_metan_sri_cbt)[21]$random$seTE # etc
# summary(subgroup_metan_sri_cbt)[146] # this gives you the random effects for each group and
# summary(subgroup_metan_sri_cbt)[146]$TE.random.w[1] # this woudl give you psychotherapy
# summary(subgroup_metan_sri_cbt)[149]  # get p-value
# summary(subgroup_metan_sri_cbt)[151] # get lower CI
# summary(subgroup_metan_sri_cbt)[152]# get upper CI
# summary(subgroup_metan_sri_cbt)[185] # USE THIS FOR THE Q value
# summary(subgroup_metan_sri_cbt)[188]# Q value df
# summary(subgroup_metan_sri_cbt)[190] # Q value p-value
# 
# 
# 
# pdf(file = "forestplot_sri_cbt_2.pdf", width = 10, height = 23)
# forest(subgroup_metan, layout = "JAMA",
#        col.by = "black", 
#        digits.sd = 2,
#        digits.tau2 = 2,
#        colgap = "0.5cm",
#        order = "obs",
#        xlim=c(-4,1), cex=0.9,
#        fonts = 12,
#        colgap.forest = "1cm",)
# dev.off()
# 
# 
# ###### but it may be better to create the ggplot yourself as below using the metagen object
# 
# df_for_plotting_sri_cbt <- data.frame(
#   names_to_change = summary(metan_overall_sri_cbt )[[1]], # just a trick to avoid duplicates see below
#   names = summary(metan_overall_sri_cbt )[[1]],
#   treatment_modality = df_for_test_control_change_sri_cbt[df_for_test_control_change_sri_cbt$new_study_id %in% summary(metan_overall_sri_cbt )[[1]], ]
#   $treatment_modality,
#   smds = summary(metan_overall_sri_cbt )[[4]], # the SMDs
#   lower_ci = summary(metan_overall_sri_cbt )[[10]], # lower CIs
#   upper_ci = summary(metan_overall_sri_cbt )[[11]]# uppper CIs
# )
# 
# # Also, to avoid the problem of duplicate values, try the following
# df_for_plotting_sri_cbt <- within(df_for_plotting_sri_cbt, names <- ave(names, names_to_change, FUN = make.unique))
# 
# # Also add two empty rows to add the stats for the groups
# # df_for_plotting [nrow(df_for_plotting )+2,] <- NA
# 
# # try this to annotate the create the plot in ggplot
# # https://www.khstats.com/blog/forest-plots/
# 
# library(forcats)
# 
# p_sri_cbt <- df_for_plotting_sri_cbt %>% 
#   mutate(names = fct_reorder(names, smds)) %>%
#   ggplot(aes(y = names)) + 
#   theme_classic()
# 
# p_sri_cbt <- p_sri_cbt+
#   geom_point(aes(x=smds, colour = factor(treatment_modality)), shape=15, size=4) +
#   geom_linerange(aes(xmin=lower_ci, xmax=upper_ci)) 
# p_sri_cbt <- p_sri_cbt
# 
# 
# p_sri_cbt <- p_sri_cbt+
#   geom_vline(xintercept = 0, linetype="dashed") +
#   labs(x="Standardised Mean Difference (negative is better)", y="")
# p_sri_cbt <- p_sri_cbt + ggtitle ("Metanalysis of Control Arms of SRI and CBT Trials",
#                   subtitle = "Standardised Mead Differences (SMDs) with 95% CIs")
# 
# 
# p_sri_cbt <- p_sri_cbt +
#   coord_cartesian(ylim=c(1,48))
# p_sri_cbt <-  p_sri_cbt + scale_colour_manual(values = c("red", "blue"))
# 
# p_sri_cbt <-  p_sri_cbt +
#   annotate("rect", xmin = summary(subgroup_metan_sri_cbt)[151]$lower.random.w[[1]], 
#            xmax = summary(subgroup_metan_sri_cbt)[152]$upper.random.w[[1]], 
#            ymin = "Srivastava, 2020", 
#            ymax = "Rohde, 2004",
#            alpha = .4,fill = "blue")+
#   
#   geom_vline(xintercept = summary(subgroup_metan_sri_cbt)[146]$TE.random.w[[1]], colour = "white")
# 
# p_sri_cbt <- p_sri_cbt  + 
#   annotate("rect", xmin = summary(subgroup_metan_sri_cbt)[151]$lower.random.w[[2]], 
#            xmax = summary(subgroup_metan_sri_cbt)[152]$upper.random.w[[2]], 
#            ymin = "Srivastava, 2020", ymax = "Rohde, 2004",
#            alpha = .3,fill = "red" ) +
#   geom_vline(xintercept = summary(subgroup_metan_sri_cbt)[146]$TE.random.w[[2]], colour = "white")
# # 
# p_sri_cbt <- p_sri_cbt+ xlim(-3.8,1)
# p_sri_cbt <- p_sri_cbt + theme(legend.title=element_blank()) +   theme(legend.text=element_text(size=12))
# p_sri_cbt <- p_sri_cbt + theme(
#   legend.position = c(.03, .5),
#   legend.justification = c("left", "bottom")
# )
# 
# 
# # 
# p_sri_cbt <- p_sri_cbt + annotate("text", label = 
#                     paste0("group difference:\n", "chi_sq = ", 
#                            round(summary(subgroup_metan_sri_cbt)[185][[1]],2),",\np <0.01 "), 
#                   x = -3.3, y = "Vostanis, 1996", size = 8)
# 
# # df_for_plotting_Sriv <- df_for_plotting %>% 
# #   filter(names == "Srivastava, 2020")
# # 
# # p_sri_cbt <- p_sri_cbt + geom_segment(aes(x = smds, xend = upper_ci, y = "Srivastava, 2020", 
# #                          yend = "Srivastava, 2020"), 
# #                      data = df_for_plotting_Sriv)
# 
# 
# # p_sri_cbt <- p_sri_cbt + geom_segment(aes(x = smds, xend = -3.8, y = "Srivastava, 2020", 
# #                          yend = "Srivastava, 2020"), arrow = arrow(length = unit(0.01, "npc")),
# #                      data = df_for_plotting_Sriv)
# p_sri_cbt <- p_sri_cbt + 
#   theme(legend.text = element_text(size=30)) +
#   theme(legend.key.height= unit(2, 'cm')) +
#   theme(axis.text.x = element_text(size=15)) +
#   theme(axis.text.y = element_text(size=15))+
#   theme(plot.title = element_text(size=25),
#         plot.subtitle = element_text(size=20)) 
# 
# p_sri_cbt <- p_sri_cbt + geom_segment(aes(x = smds, xend = upper_ci, y = "Srivastava, 2020", 
#                    yend = "Srivastava, 2020"), 
#                data = df_for_plotting_Sriv)
# 
# p_sri_cbt <- p_sri_cbt + geom_segment(aes(x = smds, xend = -3.8, y = "Srivastava, 2020", 
#                          yend = "Srivastava, 2020"), arrow = arrow(length = unit(0.01, "npc")),
#                      data = df_for_plotting_Sriv)  
# 
# p_sri_cbt
# # now print as pdf
# 
# pdf(file = "forestplot_sri_cbt.pdf", width = 15, height = 23)
# p_sri_cbt
# dev.off()
# 
# 
# 
# 
# 
# # 1d. exclude waitlist controls -------------------------------------------
# 
# # create a new dataset that excludes waitlist controls
# studies_with_wl <- df_appl_v_orange_distinct_control[df_appl_v_orange_distinct_control$control_type == "wl",]$new_study_id
# 
# df_for_test_control_change_no_wl <- 
#   df_for_test_control_change[-which(df_for_test_control_change$new_study_id %in% studies_with_wl),]  # the small discrepancy
#                                                                                             # is due to common controls
# 
# metan_overall_no_wl <- metagen(TE = cohens_d_control,
#                                  seTE = se_change_control_0.7,
#                                  studlab = new_study_id,
#                                  data = df_for_test_control_change_no_wl,
#                                  sm = "SMD",
#                                  fixed = FALSE,
#                                  random = TRUE,
#                                  method.tau = "REML",
#                                  hakn = TRUE,
#                                  title = "Apples vs Oranges no wl")
# 
# 
# #### Now you can run the subgroup metanalysis here and extract the estimates for that
# 
# subgroup_metan_no_wl <-update.meta(metan_overall_no_wl , 
#                                      subgroup = df_for_test_control_change_no_wl$treatment_modality, subgroup.name = "treatment modality",
#                                      tau.common = FALSE)
# 
# 
# 
# 
# 
# 
# # 2.Separate metanalyses for percent female and  baseline severity 
# 
# ### I have before inspected graphically the individual studies, should return to it for the paper too. 
# 
# ################ create and test function for mean and sd combo ##################
# 
# #In order to test for differences at baseline, we need to combine ns, means and sds
# 
# # formalisms to combine means and sds from two samples from
# # https://math.stackexchange.com/questions/2971315/how-do-i-combine-standard-deviations-of-two-groups
# # derivation seems correct there and I am verifying this below in sim.
# 
# set.seed(1974)
# x <- rnorm(100, 10, 5)
# x1 <- x[1:30]
# x2 <- x[-which(x %in% x1)]
# 
# n1 <- length(x1)
# n2 <- length(x2)
# 
# 
# # get combined mean
# combined_mean <- (n1*mean(x1, na.rm = T)+n2*mean(x2, na.rm = T))/(n1+n2) # formula for deriving combined mean
# 
# combined_mean == mean(x, na.rm = T) # checked it works. 
# 
# # get combined sd
# q1 = (n1-1)*var(x1, na.rm = T) + n1*mean(x1, na.rm = T)^2 # helps avoid sums and squares
# 
# q2 = (n2-1)*var(x2, na.rm = T) + n2*mean(x2, na.rm = T)^2 # helps avoid sums and squares
# 
# qc = q1 + q2
# 
# # combined_sd = sqrt( (qc - (n1+n2)*mean(total_sample, na.rm = T)^2)/(n1+n2-1) )
# 
# combined_sd  == sd(total_sample, na.rm = T) # checked it works
# 
# 
# ## put in a generalisable function that takes means, sds and ns as arguments
# 
# combined_means_sds_func <- function(mean_1, mean_2, sd_1, sd_2, n_1, n_2) {
#   # Check if mean_1 is missing
#   if (missing(mean_1)) {
#     combined_mean <- NA
#   } else {
#     # Check if mean_2 is missing
#     if (missing(mean_2)) {
#       combined_mean <- NA
#     } else {
#       combined_mean <- (n_1 * mean_1 + n_2 * mean_2) / (n_1 + n_2) # formula for deriving combined mean
#     }
#   }
#   
#   # Check if sd_1 is missing
#   if (missing(sd_1)) {
#     combined_sd <- NA
#   } else {
#     # Check if sd_2 is missing
#     if (missing(sd_2)) {
#       combined_sd <- NA
#     } else {
#       q1 <- (n_1 - 1) * (sd_1)^2 + n_1 * (mean_1)^2 # avoids dealing with sums of squares
#       q2 <- (n_2 - 1) * (sd_2)^2 + n_2 * (mean_2)^2
#       qc <- q1 + q2
#       combined_sd <- sqrt((qc - (n_1 + n_2) * (combined_mean)^2) / (n_1 + n_2 - 1)) # formula for deriving combined sd
#     }
#   }
#   
#   return(list(combined_mean, combined_sd))
# }
# 
# 
# 
# # now validate the function on the vectors created above.
# test_the_function <- combined_means_sds_func(mean_1 = mean(x1, na.rm = T), 
#                         mean_2 = mean(x2, na.rm = T), 
#                         sd_1 = sd(x1, na.rm = T), 
#                         sd_2 = sd(x2, na.rm = T), 
#                         n_1 = n1, 
#                         n_2 = n2)
# 
# test_the_function [[1]] ==    mean(x, na.rm = T)   
# 
# test_the_function [[2]] ==    sd(x, na.rm = T)  
# 
# ################ function created and tested ##################
# 
# 
# ## Now I will create a function that takes a dataframe as the argument and returns the necessary
# # means and sds so that I don't have to write too much code.
# create_combo_dataframe <- function(input_df) {
#   new_study_ids <- unique(input_df$new_study_id)
#   
#   combo_mean_1 <- numeric(length(new_study_ids))
#   combo_mean_2 <- numeric(length(new_study_ids))
#   combo_sd_1 <- numeric(length(new_study_ids))
#   combo_sd_2 <- numeric(length(new_study_ids))
#   combo_n_1 <- numeric(length(new_study_ids))
#   combo_n_2 <- numeric(length(new_study_ids))
#   study_ids <- numeric(length(new_study_ids))
#   treatment_group <- character(length(new_study_ids))
#   
#   for (i in 1:length(new_study_ids)) {
#     subset_df <- input_df[input_df$new_study_id == new_study_ids[i], ]
#     
#     combo_mean_1[i] <- subset_df$baseline_mean_active
#     combo_mean_2[i] <- subset_df$baseline_mean_control
#     combo_sd_1[i] <- subset_df$baseline_sd_active
#     combo_sd_2[i] <- subset_df$baseline_sd_control
#     combo_n_1[i] <- subset_df$baseline_n_active
#     combo_n_2[i] <- subset_df$baseline_n_control   
#     study_ids[i] <- new_study_ids[i]
#     treatment_group[i] <- subset_df$psy_or_med[1]  
#   }
#   
#   df_combo <- data.frame(
#     combo_study_ids = study_ids,
#     combo_treatment_group = treatment_group,
#     combo_mean_1 = combo_mean_1,
#     combo_mean_2 = combo_mean_2,
#     combo_sd_1 = combo_sd_1,
#     combo_sd_2 = combo_sd_2,
#     combo_n_1 = combo_n_1,
#     combo_n_2 = combo_n_2
#   )
#   
#   return(df_combo)
# }
# 
# df_appl_v_orange_distinct_control_cdrs <-
#   df_appl_v_orange_distinct_control %>% 
#   filter(instrument_name == "cdrs")
# df_appl_v_orange_distinct_control_hamd <-
#   df_appl_v_orange_distinct_control %>% 
#   filter(instrument_name == "hamd")
# 
# df_combo_cdrs <- create_combo_dataframe(df_appl_v_orange_distinct_control_cdrs)
# df_combo_hamd <- create_combo_dataframe(df_appl_v_orange_distinct_control_hamd)
# 
# 
# ## now apply the combining function to add the combined estimates 
# 
# # for CDRS
# combo_results_cdrs <- combined_means_sds_func(
#                         mean_1 = df_combo_cdrs$combo_mean_1,
#                         mean_2 = df_combo_cdrs$combo_mean_2,
#                         sd_1 = df_combo_cdrs$combo_sd_1,
#                         sd_2 = df_combo_cdrs$combo_sd_2,
#                         n_1 = df_combo_cdrs$combo_n_1,
#                         n_2 = df_combo_cdrs$combo_n_2)
# 
# # add results to dataframe
# df_combo_cdrs$combo_mean_combined <- combo_results_cdrs[[1]]
# df_combo_cdrs$combo_sd_combined <- combo_results_cdrs[[2]]
# df_combo_cdrs$combo_n_combined <- df_combo_cdrs$combo_n_1 + df_combo_cdrs$combo_n_2
# 
# 
# # for HAMD
# combo_results_hamd <- combined_means_sds_func(
#   mean_1 = df_combo_hamd$combo_mean_1,
#   mean_2 = df_combo_hamd$combo_mean_2,
#   sd_1 = df_combo_hamd$combo_sd_1,
#   sd_2 = df_combo_hamd$combo_sd_2,
#   n_1 = df_combo_hamd$combo_n_1,
#   n_2 = df_combo_hamd$combo_n_2)
# 
# # add results to dataframe
# df_combo_hamd$combo_mean_combined <- combo_results_hamd[[1]]
# df_combo_hamd$combo_sd_combined <- combo_results_hamd[[2]]
# df_combo_hamd$combo_n_combined <- df_combo_hamd$combo_n_1 + df_combo_hamd$combo_n_2
# 
# 
# # do the subgroup metanalysis for the CDRS
# cdrs_means <- metamean(n = combo_n_combined,
#                    mean = combo_mean_combined,
#                    sd = combo_sd_combined,
#                    studlab = combo_study_ids,
#                    data = df_combo_cdrs,
#                    sm = "MRAW",
#                    fixed = FALSE,
#                    random = TRUE,
#                    method.tau = "REML",
#                    hakn = TRUE,
#                    title = "CDRS at baseline")
# summary(cdrs_means)
# 
# update.meta(cdrs_means, 
#             subgroup = df_combo_cdrs$combo_treatment_group, 
#             tau.common = FALSE)
# 
# # do the subgroup analysis for the HAMD
# hamd_means <- metamean(n = combo_n_combined,
#                        mean = combo_mean_combined,
#                        sd = combo_sd_combined,
#                        studlab = combo_study_ids,
#                        data = df_combo_hamd,
#                        sm = "MRAW",
#                        fixed = FALSE,
#                        random = TRUE,
#                        method.tau = "REML",
#                        hakn = TRUE,
#                        title = "HAMD at baseline")
# summary(hamd_means)
# 
# update.meta(hamd_means, 
#             subgroup = df_combo_hamd$combo_treatment_group, 
#             tau.common = FALSE)
# 
# 
# 
# 
# # 2b. Proportion women ----------------------------------------------------
# 
# # first get the variables you need for this
# df_appl_v_orange_distinct_control$percent_women
# 
# df_appl_v_orange_distinct_control$percent_women_std_error # I had calculated this further up.
# 
# n_total <- (df_appl_v_orange_distinct_control$baseline_n_active+
#               df_appl_v_orange_distinct_control$baseline_n_control)
# 
# df_appl_v_orange_distinct_control$n_women <- df_appl_v_orange_distinct_control$percent_women*n_total/100
# 
# 
# 
# perc_women <- metamean(n = n_women,
#                        mean = percent_women,
#                        sd = percent_women_std_error, # for proportions se and sd are the same for practical purposes and large samples
#                        studlab = new_study_id,
#                        data = df_appl_v_orange_distinct_control,
#                        sm = "MRAW",
#                        fixed = FALSE,
#                        random = TRUE,
#                        method.tau = "REML",
#                        hakn = TRUE,
#                        title = "Perc women at baseline")
# summary(perc_women)
# 
# update.meta(perc_women, 
#             subgroup = df_appl_v_orange_distinct_control$psy_or_med, 
#             tau.common = FALSE)
# 
# 
# 
# # 2c. For age we are missing the sds --------------------------------------
# 
# 
# 
# # 3. metaregression.
# 
# # create dataframes for metareg -------------------------------------------
# 
# ### We need to create dataframes for the metaregressions across the various SEs.
# ### These need to be long data frames, so that we can test the treatment arm by psy_or_med allocation.
# 
# # get the Cohen's ds into long format.
# 
# df_for_cohens_d <- 
#   df_appl_v_orange %>%  
#   dplyr:: select(new_study_id, psy_or_med, cohens_d_control, cohens_d_active) %>% 
#   pivot_longer(cols = c(cohens_d_control, cohens_d_active), 
#                names_to = "arm_effect_size", 
#                values_to = "cohens_d") 
# 
# ## now to the various steps for obtaining the different standard errors 
# 
# # first, get vectors containing names of variables to extract and turn to long
# var_to_long_act <- colnames(df_appl_v_orange[,str_detect(colnames(df_appl_v_orange), "se_change_active")])
# var_to_long_ctr <- colnames(df_appl_v_orange[,str_detect(colnames(df_appl_v_orange), "se_change_control")])
# 
# # second, create loop to get the ses and to join into several different datasets
# transitional_list_df_metareg <- list() 
# list_df_for_metareg_long <- list()
# check_true_vecs_active <- 0
# check_true_vecs_ctr <- 0
# 
# for(i in 1: length(var_to_long_act)){
# 
#   transitional_list_df_metareg[[i]] <- # run the loop and store transitionally in list
#   df_appl_v_orange %>%  
#   dplyr:: select(new_study_id,psy_or_med,  var_to_long_act[i], 
#                  var_to_long_ctr[i] ) %>% 
#   pivot_longer(cols = c(var_to_long_act[i], var_to_long_ctr[i]), 
#                names_to = "arm_se", 
#                values_to = "se_change")
# 
# 
# list_df_for_metareg_long[[i]] <- cbind(df_for_cohens_d , transitional_list_df_metareg[[i]]) # create new list with new dataframs
# 
# 
# check_true_vecs_active[i] <- tapply(list_df_for_metareg_long[[i]]$se_change,  # code to control no error in looping/merging
#               list_df_for_metareg_long[[i]]$arm_se, mean, na.rm = T)[[1]]  == # tests whether means of vars agree with parent dataset.
#          mean(df_appl_v_orange[,var_to_long_act[i]], na.rm = T)
# 
# check_true_vecs_ctr[i] <- tapply(list_df_for_metareg_long[[i]]$se_change,  
#               list_df_for_metareg_long[[i]]$arm_se, mean, na.rm = T)[[2]]  == 
#          mean(df_appl_v_orange[,var_to_long_ctr[i]], na.rm = T)
# 
# }
# 
# # Check if any element is FALSE in check_true_vecs_active or check_true_vecs_ctr
# if (any(!check_true_vecs_active) || any(!check_true_vecs_ctr)) {
#   cat("something has gone wrong, means do not agree with parent dataframe, check\n")
# } else {
#   cat("it seems fine, means agree with parent dataframe\n")
# }
# 
# 
# 
# ### Now run the metaregressions.
# 
# list_mod_1_meta_reg <- list()
# list_mod_2_meta_reg <- list()
# list_mod_3_meta_reg <- list()
# 
# mod_1 <- as.formula(~ psy_or_med)
# mod_2 <- as.formula(~ psy_or_med + arm_effect_size)
# mod_3 <-  as.formula(~ psy_or_med*arm_effect_size)
# 
# for(i in 1: length(var_to_long_act)){
# 
#   list_mod_1_meta_reg[[i]] <- rma(yi = cohens_d, 
#                                     sei = se_change, 
#                                     data = list_df_for_metareg_long[[i]] , 
#                                     method = "ML", 
#                                     mods = mod_1 , 
#                                     test = "knha")    
#   
#   
#   list_mod_2_meta_reg[[i]] <- rma(yi = cohens_d, 
#                       sei = se_change, 
#                       data = list_df_for_metareg_long[[i]] , 
#                       method = "ML", 
#                       mods = mod_2, 
#                       test = "knha")
# 
# 
#   list_mod_3_meta_reg[[i]] <- rma(yi = cohens_d, 
#                                   sei = se_change, 
#                                   data = list_df_for_metareg_long[[i]] , 
#                                   method = "ML", 
#                                   mods = mod_3, 
#                                   test = "knha")
# 
#   
# }
# 
# 
# 
# 
# # Create a function to extract AICc and LRT values
# extract_aicc_lrt <- function(anova_result) {
#   aicc_r <- anova_result$fit.stats.r["AIC"][[1]]
#   aicc_f <- anova_result$fit.stats.f["AIC"][[1]]
#   lrt <- anova_result$LRT  # Assuming LRT is in the second row and fifth column
#   p_value <- anova_result$pval
#   return(cbind(aicc_f, aicc_r , lrt, p_value))
# }
# 
# 
# # Perform comparisons between list_mod_1_meta_reg and list_mod_2_meta_reg and 2 and 3
# comparison_result_1_vs_2 <- list()
# aicc_lrt_values_1_vs_2 <- list()
# comparison_result_2_vs_3 <- list()
# aicc_lrt_values_2_vs_3 <- list()
# 
# for (i in 1:length(var_to_long_act)) {
#   comparison_result_1_vs_2[[i]] <- anova(list_mod_1_meta_reg[[i]], list_mod_2_meta_reg[[i]])
#   
#   aicc_lrt_values_1_vs_2[[i]] <- extract_aicc_lrt(comparison_result_1_vs_2[[i]])
#   
#   
#   comparison_result_2_vs_3[[i]] <- anova(list_mod_2_meta_reg[[i]], list_mod_3_meta_reg[[i]])
# 
#   aicc_lrt_values_2_vs_3[[i]] <- extract_aicc_lrt(comparison_result_2_vs_3[[i]])
#   
# }
# 
# # dataframe for comparing models 1 and 2
# df_mods_1_vs_2 <- data.frame(do.call("rbind", aicc_lrt_values_1_vs_2), 
#            row.names = paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|"))
# 
# # dataframe for comparing models 2 and 3
# df_mods_2_vs_3 <- data.frame(do.call("rbind", aicc_lrt_values_2_vs_3), 
#            row.names = paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|"))
# 
# 
# # the winning model seems to be the one with the interaction 
# # present here the interaction coefficients and p-values
# list_for_coefficients_winning_model <- list ()
# 
# for (i in 1:length(var_to_long_act)) {
#   list_for_coefficients_winning_model[[i]] <-data.frame(broom:: tidy(list_mod_3_meta_reg[[i]]))
#   
#   names(list_for_coefficients_winning_model)[i] <-  paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|")[i]
#     #paste("correlation at:", str_extract(var_to_long_act, "0.[0-9]"), "|")
#   
# }
# 
# 
# list_for_coefficients_winning_model
# 
# 
# 
# # simulate many different models with mixing up SEs
# 
# 
# split_list <- lapply(list_df_for_metareg_long, function(df) split(df, df$psy_or_med))
# length(split_list) == length(var_to_long_act) # see this var above
# 
# flat_list <- unlist(split_list, recursive = FALSE) # now flatten list
# length(flat_list) == 2*length(var_to_long_act) # this seems to work.
# 
# 
# 
# # Now extract these out of the list.
# list_df_meds_for_sims <- flat_list[sapply(flat_list, function(df) unique(df$psy_or_med) == 0)]
# list_df_psy_for_sims <- flat_list[sapply(flat_list, function(df) unique(df$psy_or_med) == 1)]
# 
# length(list_df_meds_for_sims)
# head(list_df_meds_for_sims[[1]])
# 
# length(list_df_psy_for_sims)
# head(list_df_psy_for_sims[[1]])
# 
# 
# # Now create a list that contains 25 (5^2) combinations of psy and med dataframes with all possible (5^2)
# # combinations of SEs.
# combined_list_df_for_sims <- list()
# 
# # Iterate over each dataframe in the first list
# for (i in seq_along(list_df_meds_for_sims)) {
#   
#   # Iterate over each dataframe in the second list
#   for (j in seq_along(list_df_psy_for_sims)) {
#     
#     # Create the combination using rbind
#     combined_df <- rbind(list_df_meds_for_sims[[i]], list_df_psy_for_sims[[j]])
#     
#     #combine dfs
#     combined_df <- as.data.frame(combined_df)
#     
#     # Add the combined dataframe to the list
#     combined_list_df_for_sims[[length(combined_list_df_for_sims) + 1]] <- combined_df
#   }
# }
# 
# length(combined_list_df_for_sims)
# 
# # extract the unique correlation values
# se_combos <- list()
# 
# for(i in 1: length(combined_list_df_for_sims)){
# 
# se_combos[[i]] <- unique(str_extract(combined_list_df_for_sims[[i]]$arm_se, "\\d+\\.\\d+"))
# 
# 
#   
# }
# 
# names(combined_list_df_for_sims) <- se_combos # now use them to name the list of dataframes with the various correlation coefficients
#                                               # will need to fix the ugliness of this later
# length(names(combined_list_df_for_sims)) # but the solution works
# 
# 
# # let's first replicate the findings from above for an SE of 0.7 for both psy and med
# 
# ### Now run the metaregressions.
# 
# 
# list_mod_3_meta_reg_sims <- list()
# 
# 
# mod_3 <-  as.formula(~ psy_or_med*arm_effect_size)
# 
# for(i in 1: length(combined_list_df_for_sims)){
# 
#   
#   list_mod_3_meta_reg_sims [[i]] <- rma(yi = cohens_d, 
#                                   sei = se_change, 
#                                   data = combined_list_df_for_sims[[i]] , 
#                                   method = "ML", 
#                                   mods = mod_3, 
#                                   test = "knha")
# }
# 
# 
# 
# list_for_coefficients_winning_model_sims <- list ()
# 
# for (i in 1:length(combined_list_df_for_sims)) {
#   list_for_coefficients_winning_model_sims[[i]] <-data.frame(broom:: tidy(list_mod_3_meta_reg_sims[[i]]))
#   
#   
#   
# }
# # give them names
# names(list_for_coefficients_winning_model_sims ) <- se_combos 
# 
# # and as we can see they are identical for the two models. 
# list_for_coefficients_winning_model_sims$`0.7` 
# list_for_coefficients_winning_model$`correlation at: 0.7 |` 
# 
# 
# # now extract from the sims all the p-values for the psy_or_med and the interaction term.
# coef_psy_or_med_main_effect_sims <- 0
# coef_psy_or_med_interaction_sims <- 0
# 
# for(i in 1: length(combined_list_df_for_sims)){
# 
# coef_psy_or_med_main_effect_sims[i] <- list_for_coefficients_winning_model_sims[[i]]$p.value[2]
# coef_psy_or_med_interaction_sims[i] <- list_for_coefficients_winning_model_sims[[i]]$p.value[4]
# 
# }
# 
# perc_signif_coef_psy_or_med_main_effect_sims <- 
#   (sum(coef_psy_or_med_main_effect_sims<0.05) / length(coef_psy_or_med_main_effect_sims))*100
# 
# perc_signif_coef_psy_or_med_interaction_sims <- 
#   (sum(coef_psy_or_med_interaction_sims<0.05) / length(coef_psy_or_med_interaction_sims))*100
# 
# 
# ########### now let's simulate in a different way, what if each study had a value from a distribution of 
# ######## of correlations. 
# 
# 
# 
# # I will generate random numbers per study id from a distribution with these parameters. 
# # it is reasonable to generate one random correlation value per study as there is no reason why the correlation should
# # systematically vary within studies
# 
# # to achieve this I need to bring back in the SDs per study. 
# 
# # first get the Cohen's ds into long format.
# set.seed(1974)
# 
# df_for_cohens_d_long <- 
#   df_appl_v_orange %>%  
#   dplyr:: select(new_study_id, psy_or_med, cohens_d_control, cohens_d_active) %>% 
#   pivot_longer(cols = c(cohens_d_control, cohens_d_active), 
#                names_to = "arm_effect_size", 
#                values_to = "cohens_d") 
# 
# df_for_n_long <- 
#   df_appl_v_orange %>%  
#   dplyr:: select(new_study_id, psy_or_med, baseline_n_active, baseline_n_control) %>% 
#   pivot_longer(cols = c(baseline_n_control, baseline_n_active), 
#                names_to = "arm_n", 
#                values_to = "baseline_n") 
# 
# df_with_cohens_d_and_n_long <- cbind(df_for_cohens_d_long, df_for_n_long)
# 
# 
# 
# # using the original dataframe to get unique ids
# ids <- unique(df_appl_v_orange$new_study_id)
# 
# # will use truncnorm to ensure that we don't exceed boundaries
# hist(truncnorm::rtruncnorm(ids, 0.45, 0.9, 0.65, 0.2))
# 
# # will use one of the long datasets that I created above (need to bring over the procedure here)
# df_simulated <- df_with_cohens_d_and_n_long # use the dataset just created with ns and cohens d to calculate SE
# 
# # empty list to store simualted dfs
# list_df_simulated <- list()
# 
# # Number of simulations
# num_repetitions <- 1000
# 
# # something to check correlations
# vec_correlations_test <- 0
# 
# # Loop for sims
# for (i in 1:num_repetitions) {
#   # Simulate the vector
#   simulated_correlations_vector <- truncnorm::rtruncnorm(ids, 0.45, 0.9, 0.65, 0.2)
#   
#   # Create a copy of the original dataframe
#   df_simulated_copy <- df_simulated
#   
#   # add/update the sims column
#   df_simulated_copy$correlation_sim_values <- simulated_correlations_vector[match(df_simulated_copy$new_study_id, unique(df_simulated_copy$new_study_id))] # this match function is fantastic, creates a new
#   #                                                               #vector by expanding the original one.
#   
#   # Add the simulated dataframe to the list
#   list_df_simulated[[i]] <- df_simulated_copy
#   
#   # remove duplicated columns
#   list_df_simulated[[i]] <-  list_df_simulated[[i]][,!duplicated(colnames(list_df_simulated[[i]]))]
#   
#   # calculate the ses
#   list_df_simulated[[i]] <- list_df_simulated[[i]] %>% 
#     group_by(new_study_id, arm_n) %>% 
#     mutate(simulated_se = sqrt(((2*(1-correlation_sim_values))/baseline_n) + 
#                                  (cohens_d^2/(2*baseline_n))))
#   
#   # in order to check whether SEs reasonable test relationship with correlatign to one of the datasets above
#   # where I imposed a uniform correlation of 0.7
#   
#   vec_correlations_test[i] <- cor(list_df_for_metareg_long[[3]]$se_change, list_df_simulated[[i]]$simulated_se, use = "na.or.complete")
#   
# }
# 
# #check this worked
# vec_correlations_test # the correlation should be reasonable and it is, around 0.6 between the vectors
# 
# list_df_simulated[[100]][,c("new_study_id", "correlation_sim_values", "simulated_se")] # looks right
# mean(list_df_simulated[[100]]$correlation_sim_values, na.rm = T) # as expected # the mean is around the parameter I gave
# 
# mean(list_df_for_metareg_long[[3]]$se_change, na.rm = T) # we can see that we get similar SE here and in what I just simulated
# mean(list_df_simulated[[100]]$simulated_se, na.rm = T) 
# 
# 
# # now 
# list_for_random_sims <- list()
# 
# 
# mod_3 <-  as.formula(~ psy_or_med*arm_effect_size)
# 
# for(i in 1: length(list_df_simulated)){
#   
#   
#   list_for_random_sims[[i]] <- rma(yi = cohens_d, 
#                                         sei = simulated_se, 
#                                         data = list_df_simulated[[i]] , 
#                                         method = "ML", 
#                                         mods = mod_3, 
#                                         test = "knha")
# }
# 
# 
# 
# list_for_coefficients_winning_model_random_sims <- list ()
# 
# for (i in 1:length(list_for_random_sims)) {
#   list_for_coefficients_winning_model_random_sims[[i]] <-data.frame(broom:: tidy(list_for_random_sims[[i]]))
#   
#   
#   
# }
# 
# 
# 
# 
# 
# # now extract from the sims all the p-values for the psy_or_med and the interaction term.
# coef_psy_or_med_main_effect_random_sims <- 0
# coef_psy_or_med_interaction_random_sims <- 0
# 
# for(i in 1: length(list_for_coefficients_winning_model_random_sims)){
#   
#   coef_psy_or_med_main_effect_random_sims[i] <- list_for_coefficients_winning_model_random_sims[[i]]$p.value[2]
#   coef_psy_or_med_interaction_random_sims[i] <- list_for_coefficients_winning_model_random_sims[[i]]$p.value[4]
#   
# }
# 
# perc_signif_coef_psy_or_med_main_effect_random_sims <- 
#   (sum(coef_psy_or_med_main_effect_random_sims<0.05) / length(coef_psy_or_med_main_effect_random_sims))*100
# 
# perc_signif_coef_psy_or_med_interaction_random_sims <- 
#   (sum(coef_psy_or_med_interaction_random_sims<0.05) / length(coef_psy_or_med_interaction_random_sims))*100
# 
# 
# # plot the values
# data.frame(coef_psy_or_med_main_effect_random_sims) %>% 
#   ggplot(aes(x = 1:length(coef_psy_or_med_main_effect_random_sims), 
#              y = coef_psy_or_med_main_effect_random_sims)) +
#   geom_point()+
#   geom_hline(yintercept = 0.05)+
#   annotate("text", x =110, y = 0.049, label = "p-value threshold = 0.05")+
#   ggtitle("p-values of coefficients for the main effect of\n whether psychological or medication treatment",
#           subtitle = "within SEs with correlations simulated around\nmean = 0.65, sd = 0.2 and limits at 0.45 and 0.9 " )
# 
# 
# # now plot the values 
# data.frame(coef_psy_or_med_interaction_random_sims) %>% 
#   ggplot(aes(x = 1:length(coef_psy_or_med_interaction_random_sims), 
#              y = coef_psy_or_med_interaction_random_sims)) +
#   geom_point()+
#   geom_hline(yintercept = 0.05)+
#   annotate("text", x =110, y = 0.049, label = "p-value threshold = 0.05")+
#   ggtitle("p-values of coefficients for the interaction effect between\n whether psychological or medication treatment\nand whether control or active",
#           subtitle = "within SEs with correlations simulated around\nmean = 0.65, sd = 0.2 and limits at 0.45 and 0.9 " )
# 
# 
# 
# 
# 
# ## plot the cohens ds and SEs to USE THIS !!!!
# list_df_simulated[[1]]$labels_for_facet <- factor(list_df_simulated[[1]]$psy_or_med, levels = c(0, 1),
#                                                   labels = c("Anti-depressant trials" , "Psychotherapy trials"))
# list_df_simulated[[1]] %>% 
#   filter(cohens_d < 4 | cohens_d > -4) %>% 
#   ggplot(aes(x = as.factor(arm_effect_size), y = cohens_d, colour = arm_effect_size, size = baseline_n/2)) +
#   geom_point(position = position_jitter(width = 0.25, height = 0)) +
#   ylim(-3.5, 1.0)+
#   facet_wrap(~labels_for_facet)+
#   theme_minimal() +
#   guides(color = "none") + 
#   labs(size = "Sample Sizes")+
#   scale_x_discrete(labels = c("Active Arm", "Control Arm")) +
#   ggtitle("Efficacy of Active and Control Arms across Treatment modalities\nin Adolesce Depression Trials" )+
#   labs(x = NULL, y = "Standardised Mean Differences")+
#   theme(
#     text = element_text(size = 14),  # Set the base text size
#     axis.title = element_text(size = 16),  # Set axis title size
#     axis.text = element_text(size = 12),  # Set axis tick label size
#     strip.text = element_text(size = 12, face = "bold"),  # Set facet label size
#     plot.title = element_text(size = 18)  # Set plot title size
#   )
#   
# #  ################ 
# #   geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.2) +
# #   theme_minimal() +
# #   theme(legend.position = "none")
# #   
# # 
# # 
# # 
# # ## plot the cohens ds and SEs to
# # 
# # 
# # list_df_simulated[[1]] %>% 
# # ggplot(aes(x = as.factor(psy_or_med), y = cohens_d, fill = as.factor(arm_effect_size))) + 
# #   geom_boxplot(outlier.shape = NA)   +
# #   ylim(-4, 1.5)
# #   #scale_fill_brewer(discrete = TRUE, alpha=0.6) +
# # #  geom_jitter(color="black", size=0.4, alpha=0.9) +
# # 
# # 
# # 
# # 
# # 
# #   
# #   
# #   #slope graph with means and CIs MY FAVOURITE
# #   list_df_simulated[[1]] %>% 
# #   ggplot(aes(x = as.factor(arm_effect_size), y = cohens_d)) + 
# #   geom_line(aes(group = as.factor(new_study_id)), size=1, color='gray', alpha=0.6)+ 
# #   
# #   geom_point(aes(group=new_study_id),size=5,shape=21, alpha = 0.4)+
# #   facet_wrap(~psy_or_med)+
# #   ggtitle("Standardised Mean Differences of depressed adolescents to medication and psychotherapy",
# #           subtitle = "data from RCTs INCLUDING WL; means and 95%CIs in red")+
# #   ylab("Standardised Mean Differences")+
# #   scale_x_discrete(breaks=c("active_response","control_response"),
# #                    labels=c("response to active", "response to control"))+
# #   theme(axis.text.x.bottom =  element_text(angle = 45, size = 12, vjust = 0.9, hjust = 1))+
# #   theme(strip.text.x = element_text(size = rel(1.5), face = "bold"))+
# #   theme(plot.title = element_text(size = rel(2)))+
# #   theme(plot.subtitle = element_text(size = rel(1.5)))+
# #   theme(axis.title.y = element_text(size= rel(1.5)))+
# #   xlab(NULL) +
# #   stat_summary(aes(y = cohens_d, group = psy_or_med), 
# #                fun.data = "mean_cl_boot", colour = "red", size = 1, geom= "line")+
# #   stat_summary(aes(y = cohens_d, group = psy_or_med), 
# #                fun.data = "mean_cl_boot", colour = "red", size = 1)
# #   
# #   +
# #     theme_classic()
# #   
# #   
# #   
# #   list_df_simulated[[1]] %>% 
# #     ggplot(aes(cohens_d))+
# #     geom_histogram(aes(fill = arm_effect_size), alpha = 0.8, bins = 20)+
# #     facet_wrap(~psy_or_med , ncol = 1)+
# #     theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
# #     xlab(NULL)+
# #     ggtitle("Active and Control Arm Response Rates in Adolescent Depression") +
# #     xlim(-3,1)
# #   
#   
