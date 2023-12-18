library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

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


# Create summary statistics for each instrument  --------


#average statistics for each instrument, collapsed across studies for both med and psy categories 
# this includes duplicates of studies 

meds_d_means <- list()
psy_d_means <- list()
meds_baseline_means <- list()
psy_baseline_means <- list()
meds_post_means <- list()
psy_post_means <- list()

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
            sd_baseline_mean_act = sd(baseline_sd_active, na.rm = T), 
            avg_baseline_mean_ctrl = mean(baseline_mean_control, na.rm = T), 
            sd_baseline_mean_ctrl = sd(baseline_sd_control, na.rm = T))

# for psychotherapy trials, average baseline mean scores for each instrument
psy_baseline_means[[i]] <- df_appl_v_orange  %>% 
  filter(psy_or_med == 1, (instrument_name == inst_names[i])) %>% 
  summarise(n = n(), total_n_control = sum(baseline_n_control, na.rm = T), 
            total_n_active = sum(baseline_n_active, na.rm = T),
             avg_age = mean(mean_age, na.rm = T),
            avg_baseline_mean_act = mean(baseline_mean_active, na.rm = T), 
            sd_baseline_mean_act = sd(baseline_sd_active, na.rm = T), 
            avg_baseline_mean_ctrl = mean(baseline_mean_control, na.rm = T), 
            sd_baseline_mean_ctrl = sd(baseline_sd_control, na.rm = T))

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


# create unique study ids to account for multiple active arms per study name 
df_appl_v_orange <- df_appl_v_orange %>%
  mutate(study_ID = ifelse(psy_or_med == 0, paste(study, year, active_type, sep = "_"), 
                  paste(study, descr_active, sep = "_")))

# arranging by instrument value
df_appl_v_orange <- df_appl_v_orange %>% arrange(study_ID, instrument_value)

# retain only first row
df_first_row <- df_appl_v_orange %>% distinct(study_ID, .keep_all = TRUE) 

# start looking at demographics, baseline and post means, and cohens d per arm
df_demographics <- df_first_row %>% select(study, year, psy_or_med, active_type, control_type, descr_active, descr_control,
                                           instrument_name, baseline_mean_active, baseline_sd_active, baseline_n_active,
                                           baseline_mean_control, baseline_sd_control, baseline_n_control, 
                                           post_mean_active, post_sd_active, post_n_active, post_mean_control, post_sd_control, post_n_control,
                                           cohens_d_active, cohens_d_control,  mean_age, active_mean_age, control_mean_age, 
                                           percent_women, active_percent_women, control_percent_women )
# create overall mean age variable for all studies 
df_demographics <- df_demographics %>%  mutate(overall_mean_age = (active_mean_age * baseline_n_active + control_mean_age * baseline_n_control)
                                               /(baseline_n_active + baseline_n_control))
df_demographics <- df_demographics %>%  mutate(mean_age = coalesce(mean_age, overall_mean_age))

#create overall percent women variable for all studies
df_demographics <- df_demographics %>%  mutate(overall_percent_women = (active_percent_women * baseline_n_active + control_percent_women * baseline_n_control)
                                               /(baseline_n_active + baseline_n_control))
df_demographics <- df_demographics %>%  mutate(percent_women = coalesce(percent_women, overall_percent_women))

# combine arm description variables across psy and med
df_demographics$descr_active <- ifelse(is.na(df_demographics$descr_active), df_demographics$active_type, df_demographics$descr_active)
df_demographics$descr_control <- ifelse(is.na(df_demographics$descr_control), df_demographics$control_type, df_demographics$descr_control)

df_demographics <- df_demographics %>%  select(-c(active_mean_age, control_mean_age, overall_mean_age, 
                                                  active_percent_women, control_percent_women, overall_percent_women,
                                                  active_type, control_type))
df_demographics <- df_demographics %>%  arrange(psy_or_med)

# check how many studies we have baseline means for
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


            
write.csv(df_appl_v_orange, "Apples vs Oranges Dataset.csv")



#openxlsx:: write.xlsx(df_stats_per_instrument, file = "test.xlsx", colNames = T, borders = "columns", asTable = F)

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
# you see that I used tapply rather than dplyr's group_by. Here I am also using aggreagate, another
# R base function. Neat, right :) 
aggregate(df_appl_v_orange$cohens_d_control, by  = list(df_appl_v_orange$psy_or_med), summary)

# from thea above it seemed like we had some studies where the controls have a positive d
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

#check which studies have missing means or sds at baseline or post 
studies_with_missing_values <- df_appl_v_orange %>%
  filter(
     is.na(baseline_mean_active) |
      is.na(baseline_sd_active) |
      is.na(baseline_mean_control) |
      is.na(baseline_sd_control) |
      is.na(post_mean_active) |
      is.na(post_sd_active) |
      is.na(post_mean_control) |
      is.na(post_sd_control)) %>%
select(study_ID)

unique(studies_with_missing_values$study_ID)

# same thing different method
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
  select(psy_or_med, study, year, where(function(x) any(is.na(x))))

# Print columns with missing values for each study
tapply(print(columns_with_missing_values[,1:2]
), columns_with_missing_values$psy_or_med)


openxlsx:: write.xlsx(columns_with_missing_values, file = "columns_with_missing_values.xlsx", colNames = T, borders = "columns", asTable = F)



