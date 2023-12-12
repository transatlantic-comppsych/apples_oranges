library(readxl)
Walkup_Dataset <- read_excel("Walkup_Dataset.xlsx", 
                             col_types = c("text", "text", "text", "numeric", 
                                           "text", "text", "numeric", "numeric", 
                                           "numeric", "numeric"))
View(Walkup_Dataset)

Walkup_Dataset <- Walkup_Dataset %>% rename(walkup_resp_rate_active = observed_resp_rate_active, walkup_resp_rate_control = observed_resp_rate_control)

Walkup_Dataset <- Walkup_Dataset %>%
  mutate(year = ifelse(row_number() %in% c(3, 16), year, paste0(year, ".0")))

#create dataset with variables needed to compare against walkup
df_for_comparison <- merged_dataset %>% select(Column1, study, year, active_type, instrument, observed_resp_rate_active, observed_resp_rate_control)
df_compare_with_walkup <- merge(Walkup_Dataset, df_for_comparison, by = c("study","year", "instrument", "active_type"), all.x = TRUE)

cor(df_compare_with_walkup$walkup_resp_rate_active, df_compare_with_walkup$observed_resp_rate_active, method = "pearson", use = "pairwise.complete.obs")
cor(df_compare_with_walkup$walkup_resp_rate_control, df_compare_with_walkup$observed_resp_rate_control, method = "pearson", use = "pairwise.complete.obs")

df_compare_with_walkup <- df_compare_with_walkup %>%
  mutate(discrepent_active = abs(walkup_resp_rate_active - observed_resp_rate_active) > 0.02)
df_compare_with_walkup <- df_compare_with_walkup %>%
  mutate(discrepent_control = abs(walkup_resp_rate_control - observed_resp_rate_control) > 0.02)

write.csv(df_compare_with_walkup, "Compare response rates with Walkup.csv")
