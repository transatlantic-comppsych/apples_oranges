#Looking at mean SMD across simulations for psy vs med and control vs active
library(ggplot2)
library(ggforce)

list_model_1_meta_reg #contains metaregression results for each of the 1000 simulations

coefs_test <- do.call(rbind, lapply(list_dummy_var_means, function(sim) sim$coefficients))
df_coefs_test <- data.frame(coefs_test)

col_names <- c("Medication_Control", "Medication_Active", "Psychotherapy_Control", "Psychotherapy_Active")
colnames(df_coefs_test) <- col_names

df_coefs_long <- gather(df_coefs_test, key = "condition", value = "coefs", Medication_Control:Psychotherapy_Active)

#creating separate histograms

coefs_hist <- ggplot(df_coefs_long, aes(x = coefs)) +
  geom_histogram(binwidth = 0.001, color = "black", fill = "lightblue") +
  facet_wrap(~ condition, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Coefficients by Condition",
       x = "Coefficient",
       y = "Frequency") +
  theme(panel.spacing.x = unit(3, "lines"))

ggsave("simulated_coefficients_by_condition_hist.jpeg", coefs_hist)

#creating one graph with a common x axis

coefs_one_graph <- ggplot(df_coefs_long, aes(x = coefs, fill = condition)) +
  geom_histogram(binwidth = 0.001, color = "black", position = "identity", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Coefficients by Condition",
       x = "Coefficient",
       y = "Frequency") +
  scale_fill_manual(values = c("Medication_Control" = "blue", 
                               "Medication_Active" = "red", 
                               "Psychotherapy_Control" = "green", 
                               "Psychotherapy_Active" = "purple")) +
  theme(legend.title = element_blank(),
        legend.position = "right")
