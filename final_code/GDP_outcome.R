########################################
# Load Required Libraries
########################################

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(broom)    
library(tidyr)    
library(knitr)    
library(car)

########################################
# Load Common Data Files
########################################

# Common GDP data
GDP <- read_csv("C:/Users/lolle/Desktop/did/GDP.csv")

# Common inflation data (for all sectors)
Inflation_rate_quarterly <- read_csv("C:/Users/lolle/Desktop/did/Inflation_Rate_Quarterly.csv") %>%
  rename(inflation_rate = Value) %>%
  select(Country, Year, Quarter, inflation_rate)

########################################
# Define the Sector Analysis Function
########################################

analyze_sector <- function(ranking_file, employment_file, sector_name) {
  
  cat("\n\n###############################\n")
  cat(paste("Analysis for", sector_name, "Sector\n"))
  cat("###############################\n\n")
  
  ########################################
  # Section 1: Data Preparation for the Sector
  ########################################
  
  # 1.1 Read in the ranking and employment data for the given sector
  ranking <- read_csv(ranking_file)
  employment <- read_csv(employment_file) %>%
    rename(employment = Value) %>%
    select(Country, Year, Quarter, employment)
  
  # 1.2 Create treatment indicator based on avg_ratio in ranking data using top & bottom 25%
  q25 <- quantile(ranking$avg_ratio, probs = 0.25, na.rm = TRUE)
  q75 <- quantile(ranking$avg_ratio, probs = 0.75, na.rm = TRUE)
  
  ranking <- ranking %>%
    mutate(treatment = if_else(avg_ratio >= q75, 1,
                               if_else(avg_ratio <= q25, 0, NA_real_)))
  
  # Display selected countries for treatment and control groups
  treatment_countries <- ranking %>% filter(treatment == 1) %>% pull(Country)
  control_countries <- ranking %>% filter(treatment == 0) %>% pull(Country)
  
  cat("\n--- Countries Selected as Treatment (Top 25%) ---\n")
  print(treatment_countries)
  cat("\n--- Countries Selected as Control (Bottom 25%) ---\n")
  print(control_countries)
  
  # 1.3 Merge ranking information into GDP by Country and filter out missing treatments
  data_sector <- GDP %>%
    left_join(ranking %>% select(Country, treatment), by = "Country") %>%
    filter(!is.na(treatment))
  
  # 1.4 Merge the control variables (employment and inflation) into data_sector
  data_sector <- data_sector %>%
    left_join(employment, by = c("Country", "Year", "Quarter")) %>%
    left_join(Inflation_rate_quarterly, by = c("Country", "Year", "Quarter"))
  
  # 1.5 Ensure control variables are numeric
  data_sector <- data_sector %>%
    mutate(
      employment = as.numeric(employment),
      inflation_rate = as.numeric(inflation_rate)
    )
  
  # 1.6 Create a unified quarter numeric variable, derive Month, create date, and assign the post-treatment indicator.
  data_sector <- data_sector %>%
    mutate(qnum = case_when(
      Quarter == "Q1" ~ 1,
      Quarter == "Q2" ~ 2,
      Quarter == "Q3" ~ 3,
      Quarter == "Q4" ~ 4
    ),
    Month = (qnum - 1) * 3 + 1,
    date = as.Date(paste(Year, Month, "01", sep = "-")),
    # Post-treatment: 1 if date >= 2022-07-01 (start of Q3 2022), 0 otherwise.
    post = if_else(date >= as.Date("2022-07-01"), 1, 0)
    )
  
  ########################################
  # Section 1.7: Log Transformation for GDP and Employment
  ########################################
  
  # Create log-transformed variables for GDP and employment.
  data_sector <- data_sector %>%
    mutate(log_gdp = log(Value),
           log_employment = log(employment))
  
  # Confirm transformation: output summaries to verify that the transformation makes sense
  cat("\n--- Log Transformation Check ---\n")
  cat("Summary of original GDP (Value):\n")
  print(summary(data_sector$Value))
  cat("Summary of log-transformed GDP (log_gdp):\n")
  print(summary(data_sector$log_gdp))
  
  cat("\nSummary of original Employment:\n")
  print(summary(data_sector$employment))
  cat("Summary of log-transformed Employment (log_employment):\n")
  print(summary(data_sector$log_employment))
  
  #######################
  #COMMON TREND ASSUMPTION 
  ########################################
  # Section 2: Visual Inspection of GDP Trends (Pre and Post)
  ########################################
  
  data_avg <- data_sector %>%
    group_by(date, treatment) %>%
    summarize(mean_log_gdp = mean(log_gdp, na.rm = TRUE), .groups = "drop")
  
  p_trends <- ggplot(data_avg, aes(x = date, y = mean_log_gdp, color = factor(treatment))) +
    geom_line(size = 1) +
    geom_point() +
    geom_vline(xintercept = as.Date("2022-07-01"), linetype = "dashed", color = "black") +
    labs(title = paste(sector_name, "Sector Log GDP Trends"),
         x = "Date",
         y = "Average Log Real GDP",
         color = "Group") +
    scale_color_manual(values = c("0" = "blue", "1" = "red"),
                       labels = c("Control", "Treatment"))
  
  print(p_trends)
  
  ########################################
  # Section 3: Event-Study DID with Controls
  ########################################
  
  # Calculate event time relative to Q3 2022
  data_sector <- data_sector %>%
    mutate(event_time = (Year - 2022) * 4 + (qnum - 3))
  
  # Convert event_time to a factor and set Q2 2022 (event_time = -1) as the reference period.
  data_sector$event_time <- as.factor(data_sector$event_time)
  data_sector$event_time <- relevel(data_sector$event_time, ref = "-1")
  
  event_study_model <- lm(log_gdp ~ treatment * event_time + log_employment + inflation_rate,
                          data = data_sector)
  cat("\n--- Event Study Regression Summary ---\n")
  print(summary(event_study_model))
  
  es_coef <- tidy(event_study_model, conf.int = TRUE) %>%
    filter(grepl("treatment:event_time", term)) %>%
    mutate(event_time = as.numeric(sub("treatment:event_time", "", term)))
  
  p_event <- ggplot(es_coef, aes(x = event_time, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste(sector_name, "Sector Event Study"),
         x = "Event Time (quarters relative to treatment, baseline = -1)",
         y = "Estimated Treatment Effect")
  
  print(p_event)
  
  ########################################
  # Section 4: Falsification Check (Placebo Test) with Controls (Pre-Treatment Only)
  ########################################
  
  # Restrict to the pre-treatment period only (post == 0)
  data_pre_placebo <- data_sector %>% 
    filter(post == 0) %>%
    mutate(post_placebo = if_else(date >= as.Date("2021-07-01"), 1, 0))
  
  placebo_model <- lm(log_gdp ~ treatment * post_placebo + log_employment + inflation_rate,
                      data = data_pre_placebo)
  cat("\n--- Placebo Test Regression Summary (Pre-Treatment Only) ---\n")
  print(summary(placebo_model))
  
  
  #######################################
  ## 2. PARTICIPATION INTO TREATMENT IS INDIPENDENT OF IDIOSYNCRATIC SHOCKS # 
  
  ########################################
  # Section 5: Ashenfelter’s Dip
  ########################################
  
   
  # Filter the pre-treatment period 
  pre_treatment_data <- data_sector %>%
    filter(post == 0)
  
  # Compute average log GDP by Year and treatment group
  trend_data <- pre_treatment_data %>%
    group_by(Year, treatment) %>%
    summarise(mean_log_gdp = mean(log_gdp, na.rm = TRUE), .groups = "drop")
  
  # 5 Statistical Test: Pre-Treatment GDP Change
  # Calculate the change in log GDP for each country over consecutive periods.
  pre_treatment_data <- pre_treatment_data %>%
    arrange(Country, Year, Quarter) %>%
    group_by(Country) %>%
    mutate(gdp_change = log_gdp - lag(log_gdp, 1)) %>%
    ungroup()
  
  # Run a regression to test if the change in log GDP is significantly associated with treatment status,
  # without additional controls or fixed effects.
  ashenfelter_test <- lm(gdp_change ~ treatment, data = pre_treatment_data)
  cat("\n--- Ashenfelter's Dip Test Regression Summary (Pre-Treatment GDP Change without Controls and Fixed Effects) ---\n")
  print(summary(ashenfelter_test))
  
  
  
  
  
  ########################################
  # Section 6: Check for Absence of Systematic Composition Changes
  ########################################
  
  # --- T-Tests on Employment ---
  t_test_log_employment_treated <- t.test(log_employment ~ post, 
                                          data = data_sector %>% filter(treatment == 1))
  t_test_log_employment_control <- t.test(log_employment ~ post, 
                                          data = data_sector %>% filter(treatment == 0))
  
  cat(paste("\n", sector_name, "Sector - Treated Group - Log Employment:\n"))
  print(t_test_log_employment_treated)
  cat(paste("\n", sector_name, "Sector - Control Group - Log Employment:\n"))
  print(t_test_log_employment_control)
  
  
  # --- T-Tests on Inflation Rate  ---
  t_test_inflation_treated <- t.test(inflation_rate ~ post, 
                                     data = data_sector %>% filter(treatment == 1))
  t_test_inflation_control <- t.test(inflation_rate ~ post, 
                                     data = data_sector %>% filter(treatment == 0))
  
  cat(paste("\n", sector_name, "Sector - Treated Group - Inflation Rate:\n"))
  print(t_test_inflation_treated)
  cat(paste("\n", sector_name, "Sector - Control Group - Inflation Rate:\n"))
  print(t_test_inflation_control)
  
  
  
  # Regression for  Employment
  comp_change_model_log_employment <- lm(log_employment ~ post * treatment, data = data_sector)
  cat(paste("\n", sector_name, "Sector - Regression: Log Employment ~ post * treatment:\n"))
  print(summary(comp_change_model_log_employment))
  
  # Regression for Inflation Rate
  comp_change_model_inflation <- lm(inflation_rate ~ post * treatment, data = data_sector)
  cat(paste("\n", sector_name, "Sector - Regression: Inflation Rate ~ post * treatment:\n"))
  print(summary(comp_change_model_inflation))
  
  
  
  
  
  
  
  ########################################
  # Section 7: Final DiD Estimation with Controls and Fixed Effects (Three Models)
  ########################################
  
  # Model 1: Fixed effects with controls (log_employment and inflation_rate)
  did_model1 <- lm(log_gdp ~ treatment * post + log_employment + inflation_rate +
                     factor(Country) + factor(date), data = data_sector)
  cat("\n--- Final DiD Estimation Regression Summary: Fixed Effects + Controls ---\n")
  print(summary(did_model1))
  
  # Model 2: Fixed effects without controls
  did_model2 <- lm(log_gdp ~ treatment * post + factor(Country) + factor(date), data = data_sector)
  cat("\n--- Final DiD Estimation Regression Summary: Fixed Effects Only (No Controls) ---\n")
  print(summary(did_model2))
  
  # Model 3: Controls Only (No Fixed Effects)
  did_model3 <- lm(log_gdp ~ treatment * post + log_employment + inflation_rate, data = data_sector)
  cat("\n--- Final DiD Estimation Regression Summary: Controls Only (No Fixed Effects) ---\n")
  print(summary(did_model3))
  
  
  ########################################
  # Section 8: Outlier Check in log_gdp using z-score
  ########################################
  
  # Define outlier threshold
  outlier_threshold <- 3
  cat("\nOutlier criterion: observations with |z_log_gdp| > ", outlier_threshold, " will be considered outliers.\n", sep = "")
  
  # Calculate z-score for log_gdp
  data_sector <- data_sector %>%
    mutate(z_log_gdp = (log_gdp - mean(log_gdp, na.rm = TRUE)) / sd(log_gdp, na.rm = TRUE))
  
  # Identify and display outliers (|z| > threshold)
  outliers <- data_sector %>% filter(abs(z_log_gdp) > outlier_threshold)
  if(nrow(outliers) > 0) {
    cat("\n--- Outliers in log_gdp identified (|z_log_gdp| > ", outlier_threshold, ") ---\n", sep = "")
    print(outliers)
  } else {
    cat("\n--- No outliers in log_gdp identified based on z-score threshold of ", outlier_threshold, " ---\n", sep = "")
  }
  
  
  ########################################
  # Additional Code: Creating Comparison Table of DID Estimates
  ########################################
  
  # Create a helper function to generate significance stars based on p-value.
  get_stars <- function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  }
  
  # Extract the DID (treatment:post) coefficient for each model using broom.
  library(broom)
  model1_coef <- tidy(did_model1) %>% filter(term == "treatment:post")
  model2_coef <- tidy(did_model2) %>% filter(term == "treatment:post")
  model3_coef <- tidy(did_model3) %>% filter(term == "treatment:post")
  
  r2_model1 <- summary(did_model1)$r.squared
  r2_model2 <- summary(did_model2)$r.squared
  r2_model3 <- summary(did_model3)$r.squared
  
  # Create the comparison table including significance stars.
  comparison_table <- data.frame(
    Model = c("Fixed Effects + Controls", "Fixed Effects Only", "Controls Only (No Fixed Effects)"),
    DID_Estimate = c(model1_coef$estimate, model2_coef$estimate, model3_coef$estimate),
    Std_Error = c(model1_coef$std.error, model2_coef$std.error, model3_coef$std.error),
    P_Value = c(model1_coef$p.value, model2_coef$p.value, model3_coef$p.value),
    R_Squared = c(r2_model1, r2_model2, r2_model3)
  )
  comparison_table$Significance <- sapply(comparison_table$P_Value, get_stars)
  
  cat("\n--- Comparison Table of DID Estimates (including significance stars) ---\n")
  print(kable(comparison_table, digits = 3, 
              caption = "Comparison of DID Estimates across Models (Significance Stars Indicated)"))
  
  ########################################
  # Section 9: Check for Multicollinearity
  ########################################
  
  
  model_vif <- lm(log_gdp ~ post * treatment + log_employment + inflation_rate, data = data_sector)
  
  
  cat("\n--- Variance Inflation Factor (VIF) Check ---\n")
  vif_values <- suppressWarnings(vif(model_vif))
  print(vif_values)
  
  data_sector %>%
    group_by(post) %>%
    summarize(
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      count = n()
    ) %>%
    mutate(range = paste(min_date, "to", max_date))
  
  
  
  ########################################
  # Section 10: Heterogeneity Analysis on GDP
  ########################################
  
  # Heterogeneity by Employment Level
  # Create a dummy indicating whether log_employment is above its median.
  data_sector <- data_sector %>%
    mutate(High_Employment = if_else(log_employment > median(log_employment, na.rm = TRUE), 1, 0))
  
  # Estimate a model with a triple interaction: treatment * post * High_Employment.
  # Here, we control for inflation_rate and include fixed effects for Country and date.
  het_model_employment <- lm(log_gdp ~ treatment * post * High_Employment + inflation_rate +
                               factor(Country) + factor(date), data = data_sector)
  cat("\n--- Heterogeneity Analysis: GDP by Employment Level ---\n")
  print(summary(het_model_employment))
  
  
  # Heterogeneity by Inflation Level
  # Create a dummy indicating whether the inflation_rate is above its median.
  data_sector <- data_sector %>%
    mutate(High_Inflation = if_else(inflation_rate > median(inflation_rate, na.rm = TRUE), 1, 0))
  
  # Estimate a model with a triple interaction: treatment * post * High_Inflation.
  # In this case, we control for log_employment and include fixed effects for Country and date.
  het_model_inflation <- lm(log_gdp ~ treatment * post * High_Inflation + log_employment +
                              factor(Country) + factor(date), data = data_sector)
  cat("\n--- Heterogeneity Analysis: GDP by Inflation Level ---\n")
  print(summary(het_model_inflation))
  
  
}  # End of analyze_sector function


########################################
# Run Analysis for Each Sector
########################################

# Primary Sector Analysis
analyze_sector("C:/Users/lolle/Desktop/did/ranking_primary.csv",
               "C:/Users/lolle/Desktop/did/Employment_primary_sector.csv",
               "Primary")

# Secondary Sector Analysis
analyze_sector("C:/Users/lolle/Desktop/did/ranking_secondary.csv",
               "C:/Users/lolle/Desktop/did/Employment_secondary_sector.csv",
               "Secondary")

# Tertiary Sector Analysis
analyze_sector("C:/Users/lolle/Desktop/did/ranking_tertiary.csv",
               "C:/Users/lolle/Desktop/did/Employment_tertiary_sector.csv",
               "Tertiary")
