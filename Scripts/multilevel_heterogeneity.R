
###############2. Simulation function for multilevel metaregression

run_multi_metaregression <- function(list_of_datasets, model_formula) {
  list_model_meta_reg <- list()
  
  for (i in seq_along(list_of_datasets)) {
    # Check if required columns are present
    if(!all(c("cohens_d", "simulated_se", "new_study_id", "arm_id") %in% names(list_of_datasets[[i]]))) {
      stop("Dataset is missing one or more required columns: 'cohens_d', 'simulated_se', 'new_study_id', 'arm_id'")
    }
    
    # Fit the meta-regression model
    list_model_meta_reg[[i]] <- metafor::rma.mv(yi = cohens_d,
                                                V = simulated_se^2,
                                                random = ~ 1 | new_study_id/arm_id,
                                                # this is where 
                                                # we specify the random
                                                # effect structure
                                                data = list_of_datasets[[i]],
                                                method = "ML",
                                                mods = model_formula,
                                                # here we specify the 
                                                # fixed effects as before
                                                test = "knha")
  }
  
  return(list_model_meta_reg)
}


##########4. Function to extract mean coefficients and to extract model characteristics

aggregate_model_results <- function(list_model_1_meta_reg, condition) {
  n_models <- length(list_model_1_meta_reg)
  n_conditions <- length(condition)
  
  df_coefs <- matrix(NA, nrow = n_models, ncol = n_conditions)
  df_se <- matrix(NA, nrow = n_models, ncol = n_conditions)
  df_z_value <- matrix(NA, nrow = n_models, ncol = n_conditions)
  df_lower_ci <- matrix(NA, nrow = n_models, ncol = n_conditions)
  df_upper_ci <- matrix(NA, nrow = n_models, ncol = n_conditions)
  tau_sq <- rep(NA, n_models)
  qe <- rep(NA, n_models)
  k <- rep(NA, n_models)
  
  for (i in seq_len(n_models)) {
    model <- list_model_1_meta_reg[[i]]
    tau_sq[i] <- model$tau2
    qe[i] <- model$QE
    k[i] <- model$k
    coefs <- model$beta
    ses <- model$se
    zvals <- model$zval
    lower_ci <- model$ci.lb
    upper_ci <- model$ci.ub
    
    for (j in seq_len(n_conditions)) {
      if (!is.null(coefs[j])) {
        df_coefs[i, j] <- coefs[j]
        df_se[i, j] <- ses[j]
        df_z_value[i, j] <- zvals[j]
        df_lower_ci[i, j] <- lower_ci[j]
        df_upper_ci[i, j] <- upper_ci[j]
      }
    }
  }
  
  df_coefficients_model <- data.frame(
    coefficients = colMeans(df_coefs, na.rm = TRUE),
    se = colMeans(df_se, na.rm = TRUE),
    z_value = colMeans(df_z_value, na.rm = TRUE),
    lower_ci = colMeans(df_lower_ci, na.rm = TRUE),
    upper_ci = colMeans(df_upper_ci, na.rm = TRUE),
    tau_sq = mean(tau_sq, na.rm = TRUE),
    qe = mean(qe, na.rm = TRUE),
    k = mean(k, na.rm = TRUE)
  )
  
  return(df_coefficients_model)
}


###########4. Function to extract the SMDs  (SMD, ses, CI) for each level of the dummy variable. The output is used below in 5.
# it is a slightly awkward one, because I couldn't' come up with a better way to add the intercept to each coefficient
# given the named output of the built in coef function. here I extract the coefficients

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

############5. Function to get the means out of the SMDs (this is similar to Func 3 and I should at some point create one more general one)
# here the argument list_of_dfs is the list of extracted coefficients that you get out of the extract_coefficients_func
# which itself you get from the run_metaregression function. The argument conditions refers to the levels of the variable that describes each study arm

calculate_mean_coefs_ses <- function(list_of_dfs, conditions) {
  df_coefs <- matrix(NA, nrow = length(list_of_dfs), ncol = length(conditions))
  df_se <- matrix(NA, nrow = length(list_of_dfs), ncol = length(conditions))
  
  for (i in 1:length(list_of_dfs)) {
    for (j in 1:length(conditions)) {
      df_coefs[i, j] <- list_of_dfs[[i]]$coefficients[j]
      df_se[i, j] <- list_of_dfs[[i]]$se[j]
    }
  }
  
  df_mean_coefs_ses <- data.frame(
    coef_means = colMeans(df_coefs),
    se_means = colMeans(df_se)
  )
  
  df_mean_coefs_ses$lower_ci <- df_mean_coefs_ses$coef_means - 1.96 * df_mean_coefs_ses$se_means
  df_mean_coefs_ses$upper_ci <- df_mean_coefs_ses$coef_means + 1.96 * df_mean_coefs_ses$se_means
  
  return(df_mean_coefs_ses)
}

# add a new multilevel variable to the simulated data for the regression

for(i in 1: length(list_df_simulated)){
  
  list_df_simulated[[i]]$arm_effect_size <- factor(list_df_simulated[[i]]$arm_effect_size)
  list_df_simulated[[i]] <- list_df_simulated[[i]] %>% 
    mutate(four_level_var = case_when(psy_or_med == 0 & arm_effect_size == "cohens_d_active" ~ "medication_active",
                                      psy_or_med == 0 & arm_effect_size == "cohens_d_control" ~ "medication_control",
                                      psy_or_med == 1 & arm_effect_size == "cohens_d_active" ~ "psychotherapy_active",
                                      psy_or_med == 1 & arm_effect_size == "cohens_d_control" ~ "psychotherapy_control")
    )
  
  list_df_simulated[[i]]$four_level_var <- factor(list_df_simulated[[i]]$four_level_var) # turn to factor
  
  # relevel so that medication control becomes the reference category for the regression
  #  list_df_simulated[[i]]$four_level_var <- relevel(list_df_simulated[[1]]$four_level_var, ref = "medication_control")
  list_df_simulated[[i]]$four_level_var <-   fct_relevel(list_df_simulated[[i]]$four_level_var,
                                                         "medication_control",
                                                         "medication_active", 
                                                         "psychotherapy_control",
                                                         "psychotherapy_active")
}


# count studies
# count the number of studies
n_unique_studies <- length(unique(df_long_for_metan$new_study_id)) # CHARLOTTE please check
df_count_studies <- count_studies(df_long_for_metan) # CHARLOTTE please check
# CB checked both, both correct.

# specify model
model_1 <- as.formula(~ four_level_var)

# run metareg function 
# here you can use the simulated results directly
list_model_1_meta_reg <- run_multi_metaregression(list_df_simulated, model_1)

# extract coefficients and model characteristics.
condition <- levels(list_df_simulated[[1]]$four_level_var)
aggregate_results_overall <-  aggregate_model_results(list_model_1_meta_reg, condition)
aggregate_results_overall <- cbind(aggregate_results_overall, condition)

# extract SMDs from the  list
list_dummy_var_means <-  lapply(list_model_1_meta_reg, extract_coefficients_func)

# calculate mean SMDs and ses
df_mean_coefs_from_sim <- calculate_mean_coefs_ses(list_dummy_var_means, condition)

df_mean_coefs_from_sim <- data.frame(cbind(condition,df_mean_coefs_from_sim))

# add the ns
df_mean_coefs_from_sim <- left_join(df_mean_coefs_from_sim, df_count_studies[df_count_studies$is_missing == FALSE, ] %>% 
                                      ungroup() %>% 
                                      select(condition, n),  
                                    by = "condition")
