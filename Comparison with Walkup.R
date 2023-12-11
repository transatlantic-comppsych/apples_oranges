library(readxl)
Walkup_Dataset <- read_excel("Walkup_Dataset.xlsx", 
                             col_types = c("text", "text", "text", "numeric", 
                                           "text", "text", "numeric", "numeric", 
                                           "numeric", "numeric"))
View(Walkup_Dataset)

Walkup_Dataset <- Walkup_Dataset %>%
  mutate(year = ifelse(row_number() != 16, paste0(year, ".0"), year))

#create dataset with variables needed to compare against walkup
df_for_comparison <- merged_dataset %>% select(Column1, study, year, instrument, observed_resp_rate_active, observed_resp_rate_control)
df_compare_with_walkup <- merge(Walkup_Dataset, df_for_comparison, by = c("study","year", "instrument"), all.x = TRUE)