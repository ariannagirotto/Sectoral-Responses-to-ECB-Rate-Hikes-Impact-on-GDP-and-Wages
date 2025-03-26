### LIBRARIES ###


library(readr)
library(dplyr)
library(ggplot2)
library(fixest)
library(did)
library(car)


### IMPORTING THE DATASETS ###


employment_primary_sector <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/Employment_primary_sector.csv")
employment_secondary_sector <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/Employment_secondary_sector.csv")
employment_tertiary_sector <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/Employment_tertiary_sector.csv")
Inflation_rate_quarterly <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/Inflation_Rate_Quarterly.csv")
ranking_primary <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/ranking_primary.csv")
ranking_secondary <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/ranking_secondary.csv")
ranking_tertiary <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/ranking_tertiary.csv")
Wages_and_Salaries <- read_csv("/Users/ariannagirotto/Desktop/Causal Inference/Wages_and_Salaries.csv")


### REMOVING "EUROAREA" ###


employment_primary_sector <- employment_primary_sector %>% filter(Country != "Euroarea")
employment_secondary_sector <- employment_secondary_sector %>% filter(Country != "Euroarea")
employment_tertiary_sector <- employment_tertiary_sector %>% filter(Country != "Euroarea")
Wages_and_Salaries <- Wages_and_Salaries %>% filter(Country != "Euroarea")
Inflation_rate_quarterly <- Inflation_rate_quarterly %>% filter(Country != "Euroarea")


### ASSIGNING THE TREATMENT ###


# Primary Sector
treat_threshold_primary <- quantile(ranking_primary$avg_ratio, probs = 0.75, na.rm = TRUE)
control_threshold_primary <- quantile(ranking_primary$avg_ratio, probs = 0.25, na.rm = TRUE)

ranking_primary <- ranking_primary %>%
  mutate(treatment = case_when(
    avg_ratio >= treat_threshold_primary ~ 1,  
    avg_ratio <= control_threshold_primary ~ 0, 
    TRUE ~ NA_real_  
  ))

# Secondary Sector
treat_threshold_secondary <- quantile(ranking_secondary$avg_ratio, probs = 0.75, na.rm = TRUE)
control_threshold_secondary <- quantile(ranking_secondary$avg_ratio, probs = 0.25, na.rm = TRUE)

ranking_secondary <- ranking_secondary %>%
  mutate(treatment = case_when(
    avg_ratio >= treat_threshold_secondary ~ 1,
    avg_ratio <= control_threshold_secondary ~ 0,
    TRUE ~ NA_real_
  ))

# Tertiary Sector
treat_threshold_tertiary <- quantile(ranking_tertiary$avg_ratio, probs = 0.75, na.rm = TRUE)
control_threshold_tertiary <- quantile(ranking_tertiary$avg_ratio, probs = 0.25, na.rm = TRUE)

ranking_tertiary <- ranking_tertiary %>%
  mutate(treatment = case_when(
    avg_ratio >= treat_threshold_tertiary ~ 1,
    avg_ratio <= control_threshold_tertiary ~ 0,
    TRUE ~ NA_real_
  ))


### TRASFORMING IN LOG THE VARIABLES: WAGES, EMPLOYMENT AND INFLATION ###


# Wages and Salaries
Wages_and_Salaries$log_Value <- log(Wages_and_Salaries$Value)
Wages_and_Salaries <- Wages_and_Salaries[-4]

# Employment Primary Sector
employment_primary_sector$log_Value <- log(employment_primary_sector$Value)
employment_primary_sector <- employment_primary_sector[-5]

# Employment Secondary Sector
employment_secondary_sector$log_Value <- log(employment_secondary_sector$Value)
employment_secondary_sector <- employment_secondary_sector[-5]

# Employment Tertiary Sector
employment_tertiary_sector$log_Value <- log(employment_tertiary_sector$Value)
employment_tertiary_sector <- employment_tertiary_sector[-5]

# Inflation
# Using an adjust logaritmic transformation since there are negative values

min_inflation <- min(Inflation_rate_quarterly$Value)
c <- abs(min_inflation) + 1
Inflation_rate_quarterly$log_Value <- log(Inflation_rate_quarterly$Value + c)

# Checking the distribution
ggplot(Inflation_rate_quarterly, aes(x = log_Value)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribuzione della Inflazione (Trasformazione Logaritmica Aggiustata)", 
       x = "Log(Inflation + c)", 
       y = "Frequenza") +
  theme_minimal()

Inflation_rate_quarterly <- Inflation_rate_quarterly[-4]

############################## PRIMARY SECTOR ##################################


### MERGING THE DATASETS ###


data_primary <- Wages_and_Salaries %>%
  rename(Wages = log_Value) %>%
  left_join(ranking_primary %>% select(Country, treatment), by = "Country") %>%
  filter(!is.na(treatment)) %>% 
  filter(treatment == 0 | treatment == 1)  

data_primary <- data_primary %>%
  left_join(employment_primary_sector %>% rename(Employment = log_Value), by = c("Country", "Year", "Quarter")) %>%
  left_join(Inflation_rate_quarterly %>% rename(Inflation = log_Value), by = c("Country", "Year", "Quarter"))


### CHECKING THE ASSUMPTIONS ###


# 1. COMMON TREND ASSUMPTION #

# 1.A Visual Inspection

trend_data <- data_primary %>%
  group_by(Year, treatment) %>%
  summarise(mean_wages = mean(Wages, na.rm = TRUE), .groups = "drop")

ggplot(trend_data, aes(x = Year, y = mean_wages, color = factor(treatment))) +
  geom_line(aes(group = treatment), size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(trend_data$Year), max(trend_data$Year), by = 5)) +  
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("0" = "Control", "1" = "Treatment")) +  
  labs(title = "Visual Inspection",
       x = "Date",
       y = "Average log Wages",
       color = "Group") +
  theme_gray() +  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)  
  ) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "black")


# 1.B Falsification Check

dataset_falsification <- data_primary %>%
  mutate(post = ifelse((Year > 2021) | (Year == 2021 & Quarter >= "Q3"), 1, 0))

falsification_did <- feols(Wages ~ post * treatment + Inflation + 
                             Employment | Year + Quarter, data = dataset_falsification)

summary(falsification_did)

# 1.C Event Study DiD

data_event <- data_primary %>%
  mutate(qnum = case_when(
    Quarter == "Q1" ~ 1,
    Quarter == "Q2" ~ 2,
    Quarter == "Q3" ~ 3,
    Quarter == "Q4" ~ 4
  ),
  event_time = (Year - 2022) * 4 + (qnum - 3))

data_event$event_time <- as.factor(data_event$event_time)
data_event$event_time <- relevel(data_event$event_time, ref = "-1")

event_study_model <- lm(Wages ~ treatment * event_time + Employment + Inflation,
                        data = data_event)
summary(event_study_model)

es_coef <- tidy(event_study_model, conf.int = TRUE) %>%
  filter(grepl("treatment:event_time", term)) %>%
  mutate(event_time = as.numeric(sub("treatment:event_time", "", term)))

ggplot(es_coef, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Event Study: Treatment Effect by Event Time",
       x = "Event Time (quarters relative to treatment, baseline = -1)",
       y = "Estimated Treatment Effect")


# 2. PARTICIPATION INTO TREATMENT IS INDIPENDENT OF IDIOSYNCRATIC SHOCKS # 

# 2.A Statistical Test: Pre Treatment Wage Drop

pre_treatment_data <- data_primary %>%
  filter(Year < 2022 | (Year == 2022 & Quarter < "Q3")) 

trend_data <- pre_treatment_data %>%
  group_by(Year, treatment) %>%
  summarise(mean_wages = mean(Wages, na.rm = TRUE), .groups = "drop")

pre_treatment_data <- pre_treatment_data %>%
  arrange(Country, Year, Quarter) %>%
  group_by(Country) %>%
  mutate(wage_change = Wages - lag(Wages, 1)) %>%
  ungroup()

ashenfelter_test <- feols(wage_change ~ treatment, data = pre_treatment_data)

summary(ashenfelter_test)


# 3. ABSENCE OF SYSTEMTIC COMPOSITION CHANGES WITHIN EACH GROUP #

# 3.A Covariate balance test between pre and post

data_post <- data_primary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

balance_check <- data_post %>%
  group_by(post, treatment) %>%
  summarise(
    mean_employment = mean(Employment, na.rm = TRUE),
    mean_inflation = mean(Inflation, na.rm = TRUE),
    .groups = "drop"
  )

balance_check

t.test(Employment ~ post, data = data_post, subset = (treatment == 1))
t.test(Employment ~ post, data = data_post, subset = (treatment == 0))

t.test(Inflation ~ post, data = data_post, subset = (treatment == 1))
t.test(Inflation ~ post, data = data_post, subset = (treatment == 0))


# 3.B Regression on covariates to assess compositional changes

comp_change_model <- feols(Employment ~ post * treatment, data = data_post)
summary(comp_change_model)

comp_change_model <- feols(Inflation ~ post * treatment, data = data_post)
summary(comp_change_model)


### DiD ###


final_data_primary <- data_primary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

did_model_primary <- feols(Wages ~ treatment * post + Employment + Inflation, data = final_data_primary)

summary(did_model_primary)


### CHECKING THE ROBUSTNESS OF THE MODEL ###


# 1. VARIANTS OF THE MODEL #

# Model with Fixed Effects and Controls 

# Adding fixed effects for Country and time (date) to control for time-invariant and common time-specific shocks.
did_with_controls <- lm(Wages ~ treatment * post + Employment + Inflation +
                  factor(Country) + factor(Year) + factor(Quarter), data = final_data_primary)
cat("\n--- Final DiD Estimation Regression Summary with Fixed Effects ---\n")
print(summary(did_with_controls))
did_with_controls <- feols(Wages ~ treatment * post + Employment + Inflation | Country + Year + Quarter, data = final_data_primary)


# Model with Fixed Effects and without Controls

did_without_controls <- feols(Wages ~ post * treatment | Country + Year + Quarter, 
                              data = final_data_primary)
summary(did_without_controls)


# 2. OUTLIERS # 

# 2.A Identification (z-score)

final_data_primary$z_score <- scale(final_data_primary$Wages)
outliers <- final_data_primary %>% filter(abs(z_score) > 3)
print(outliers)


# 3. ETEROGENITY OF TREATMENTS #

# 3.A Heterogeneity by employment level

final_data_primary_empl <- final_data_primary %>%
  mutate(High_Employment = ifelse(Employment > median(Employment, na.rm = TRUE), 1, 0))

model_heterogeneity_empl_primary <- feols(Wages ~ post * treatment * High_Employment + Inflation | Country + Year + Quarter, 
                             data = final_data_primary_empl)
summary(model_heterogeneity_empl_primary)


# 3.B Heterogeneity by inflation level 

final_data_primary_eter_inflation <- final_data_primary %>%
  mutate(High_Inflation = ifelse(Inflation > median(Inflation, na.rm = TRUE), 1, 0))

model_heterogeneity_infl_primary <- feols(Wages ~ post * treatment * High_Inflation + Employment | Country + Year + Quarter, 
                                       data = final_data_primary_eter_inflation)
summary(model_heterogeneity_infl_primary)


# 4. CHECK FOR MULTICOLLINEARITY


model_vif <- lm(Wages ~ post * treatment + Employment + Inflation, data = final_data_primary)
vif(model_vif)


############################## SECONDARY SECTOR ################################


### MERGING THE DATASETS ###


data_secondary <- Wages_and_Salaries %>%
  rename(Wages = log_Value) %>%
  left_join(ranking_secondary %>% select(Country, treatment), by = "Country") %>%
  filter(!is.na(treatment))

data_secondary <- data_secondary %>%
  left_join(employment_secondary_sector %>% rename(Employment = log_Value), by = c("Country", "Year", "Quarter")) %>%
  left_join(Inflation_rate_quarterly %>% rename(Inflation = log_Value), by = c("Country", "Year", "Quarter"))


### CHECKING THE ASSUMPTIONS ###


# 1. COMMON TREND ASSUMPTION #

# 1.A Visual Inspection

trend_data <- data_secondary %>%
  group_by(Year, treatment) %>%
  summarise(mean_wages = mean(Wages, na.rm = TRUE), .groups = "drop")

ggplot(trend_data, aes(x = Year, y = mean_wages, color = factor(treatment))) +
  geom_line(aes(group = treatment), size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(trend_data$Year), max(trend_data$Year), by = 5)) +  
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("0" = "Control", "1" = "Treatment")) +  
  labs(title = "Visual Inspection",
       x = "Date",
       y = "Average log Wages",
       color = "Group") +
  theme_gray() +  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)  
  ) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "black")

# 1.B Falsification Check

dataset_falsification <- data_secondary %>%
  mutate(post = ifelse((Year > 2021) | (Year == 2021 & Quarter >= "Q3"), 1, 0))

falsification_did <- feols(Wages ~ post * treatment + Inflation + 
                             Employment | Year + Quarter, data = dataset_falsification)

summary(falsification_did)

# 1.C Event Study DiD

data_event <- data_secondary %>%
  mutate(qnum = case_when(
    Quarter == "Q1" ~ 1,
    Quarter == "Q2" ~ 2,
    Quarter == "Q3" ~ 3,
    Quarter == "Q4" ~ 4
  ),
  event_time = (Year - 2022) * 4 + (qnum - 3))

data_event$event_time <- as.factor(data_event$event_time)
data_event$event_time <- relevel(data_event$event_time, ref = "-1")

event_study_model <- lm(Wages ~ treatment * event_time + Employment + Inflation,
                        data = data_event)
summary(event_study_model)

es_coef <- tidy(event_study_model, conf.int = TRUE) %>%
  filter(grepl("treatment:event_time", term)) %>%
  mutate(event_time = as.numeric(sub("treatment:event_time", "", term)))

ggplot(es_coef, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Event Study: Treatment Effect by Event Time",
       x = "Event Time (quarters relative to treatment, baseline = -1)",
       y = "Estimated Treatment Effect")


# 2. PARTICIPATION INTO TREATMENT IS INDIPENDENT OF IDIOSYNCRATIC SHOCKS # 

# 2.A Statistical Test: Pre Treatment Wage Drop

pre_treatment_data <- data_secondary %>%
  filter(Year < 2022 | (Year == 2022 & Quarter < "Q3")) 

pre_treatment_data <- pre_treatment_data %>%
  arrange(Country, Year, Quarter) %>%
  group_by(Country) %>%
  mutate(wage_change = Wages - lag(Wages, 1)) %>%
  ungroup()

ashenfelter_test <- feols(wage_change ~ treatment, data = pre_treatment_data)

summary(ashenfelter_test)


# 3. ABSENCE OF SYSTEMTIC COMPOSITION CHANGES WITHIN EACH GROUP #

# 3.A Covariate balance test between pre and post

data_post <- data_secondary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

balance_check <- data_post %>%
  group_by(post, treatment) %>%
  summarise(
    mean_employment = mean(Employment, na.rm = TRUE),
    mean_inflation = mean(Inflation, na.rm = TRUE),
    .groups = "drop"
  )

balance_check

t.test(Employment ~ post, data = data_post, subset = (treatment == 1))
t.test(Employment ~ post, data = data_post, subset = (treatment == 0))

t.test(Inflation ~ post, data = data_post, subset = (treatment == 1))
t.test(Inflation ~ post, data = data_post, subset = (treatment == 0))


# 3.B Regression on covariates to assess compositional changes

comp_change_model <- feols(Employment ~ post * treatment, data = data_post)
summary(comp_change_model)

comp_change_model <- feols(Inflation ~ post * treatment, data = data_post)
summary(comp_change_model)


### DiD ###


final_data_secondary <- data_secondary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

did_model_secondary <- feols(Wages ~ treatment * post + Employment + Inflation, data = final_data_secondary)

summary(did_model_secondary)


### CHECKING THE ROBUSTNESS OF THE MODEL ###


# 1. VARIANTS OF THE MODEL #

# 1.A Model with Fixed Effects and Controls 

did_with_controls <- lm(Wages ~ treatment * post + Employment + Inflation +
                          factor(Country) + factor(Year) + factor(Quarter), data = final_data_secondary)
cat("\n--- Final DiD Estimation Regression Summary with Fixed Effects ---\n")
summary(did_with_controls)


# 1.B Model with Fixed Effects and without Controls

did_without_controls <- feols(Wages ~ post * treatment | Country + Year + Quarter, data = final_data_secondary)
summary(did_without_controls)


# 2. OUTLIERS # 

# 2.A Identification (z-score)

final_data_secondary$z_score <- scale(final_data_secondary$Wages)
outliers <- final_data_secondary %>% filter(abs(z_score) > 3)
outliers


# 3. ETEROGENITY OF TREATMENTS #

# 3.A Heterogeneity by employment level

final_data_secondary_eter_empl <- final_data_secondary %>%
  mutate(High_Employment = ifelse(Employment > median(Employment, na.rm = TRUE), 1, 0))

model_heterogeneity_empl_secondary <- feols(Wages ~ post * treatment * High_Employment + Inflation | Country + Year + Quarter, 
                             data = final_data_secondary_eter_empl)
summary(model_heterogeneity_empl_secondary)

# 3.B Heterogeneity by inflation level 

final_data_secondary_eter_inflation <- final_data_secondary %>%
  mutate(High_Inflation = ifelse(Inflation > median(Inflation, na.rm = TRUE), 1, 0))

model_heterogeneity_infl_secondary <- feols(Wages ~ post * treatment * High_Inflation + Employment | Country + Year + Quarter, 
                                       data = final_data_secondary_eter_inflation)
summary(model_heterogeneity_infl_secondary)


# 4. CHECK FOR MULTICOLLINEARITY


model_vif <- lm(Wages ~ post * treatment + Employment + Inflation, data = final_data_secondary)
vif(model_vif)


############################## TERTIARY SECTOR #################################


### MERGING THE DATASETS ###


data_tertiary <- Wages_and_Salaries %>%
  rename(Wages = log_Value) %>%
  left_join(ranking_tertiary %>% select(Country, treatment), by = "Country") %>%
  filter(!is.na(treatment))

data_tertiary <- data_tertiary %>%
  left_join(employment_tertiary_sector %>% rename(Employment = log_Value), by = c("Country", "Year", "Quarter")) %>%
  left_join(Inflation_rate_quarterly %>% rename(Inflation = log_Value), by = c("Country", "Year", "Quarter"))


### CHECKING THE ASSUMPTIONS ###


# 1. COMMON TREND ASSUMPTION #

# 1.A Visual Inspection

trend_data <- data_tertiary %>%
  group_by(Year, treatment) %>%
  summarise(mean_wages = mean(Wages, na.rm = TRUE), .groups = "drop")

ggplot(trend_data, aes(x = Year, y = mean_wages, color = factor(treatment))) +
  geom_line(aes(group = treatment), size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(trend_data$Year), max(trend_data$Year), by = 5)) +  
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("0" = "Control", "1" = "Treatment")) +  
  labs(title = "Visual Inspection",
       x = "Date",
       y = "Average log Wages",
       color = "Group") +
  theme_gray() +  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)  
  ) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "black")

# 1.B Falsification Check

dataset_falsification <- data_tertiary %>%
  mutate(post = ifelse((Year > 2021) | (Year == 2021 & Quarter >= "Q3"), 1, 0))

falsification_did <- feols(Wages ~ post * treatment + Inflation + 
                             Employment | Year + Quarter, data = dataset_falsification)

summary(falsification_did)

# 1.C Event Study DiD

data_event <- data_tertiary %>%
  mutate(qnum = case_when(
    Quarter == "Q1" ~ 1,
    Quarter == "Q2" ~ 2,
    Quarter == "Q3" ~ 3,
    Quarter == "Q4" ~ 4
  ),
  event_time = (Year - 2022) * 4 + (qnum - 3))

data_event$event_time <- as.factor(data_event$event_time)
data_event$event_time <- relevel(data_event$event_time, ref = "-1")

event_study_model <- lm(Wages ~ treatment * event_time + Employment + Inflation,
                        data = data_event)
summary(event_study_model)

es_coef <- tidy(event_study_model, conf.int = TRUE) %>%
  filter(grepl("treatment:event_time", term)) %>%
  mutate(event_time = as.numeric(sub("treatment:event_time", "", term)))

ggplot(es_coef, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Event Study: Treatment Effect by Event Time",
       x = "Event Time (quarters relative to treatment, baseline = -1)",
       y = "Estimated Treatment Effect")


# 2. PARTICIPATION INTO TREATMENT IS INDIPENDENT OF IDIOSYNCRATIC SHOCKS # 

# 2.A Statistical Test: Pre Treatment Wage Drop

pre_treatment_data <- data_tertiary %>%
  filter(Year < 2022 | (Year == 2022 & Quarter < "Q3")) 

trend_data <- pre_treatment_data %>%
  group_by(Year, treatment) %>%
  summarise(mean_wages = mean(Wages, na.rm = TRUE), .groups = "drop")

pre_treatment_data <- pre_treatment_data %>%
  arrange(Country, Year, Quarter) %>%
  group_by(Country) %>%
  mutate(wage_change = Wages - lag(Wages, 1)) %>%
  ungroup()

ashenfelter_test <- feols(wage_change ~ treatment, data = pre_treatment_data)

summary(ashenfelter_test)


# 3. ABSENCE OF SYSTEMTIC COMPOSITION CHANGES WITHIN EACH GROUP #

# 3.A Covariate balance test between pre and post

data_post <- data_tertiary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

balance_check <- data_post %>%
  group_by(post, treatment) %>%
  summarise(
    mean_employment = mean(Employment, na.rm = TRUE),
    mean_inflation = mean(Inflation, na.rm = TRUE),
    .groups = "drop"
  )

balance_check

t.test(Employment ~ post, data = data_post, subset = (treatment == 1))
t.test(Employment ~ post, data = data_post, subset = (treatment == 0))

t.test(Inflation ~ post, data = data_post, subset = (treatment == 1))
t.test(Inflation ~ post, data = data_post, subset = (treatment == 0))


# 3.C Regression on covariates to assess compositional changes

comp_change_model <- feols(Employment ~ post * treatment, data = data_post)
summary(comp_change_model)

comp_change_model <- feols(Inflation ~ post * treatment, data = data_post)
summary(comp_change_model)


### DiD ###


final_data_tertiary <- data_tertiary %>%
  mutate(post = ifelse(Year > 2022 | (Year == 2022 & Quarter >= "Q3"), 1, 0))

did_model_tertiary <- feols(Wages ~ treatment * post + Employment + Inflation, data = final_data_tertiary)

summary(did_model_tertiary)


### CHECKING THE ROBUSTNESS OF THE MODEL ###


# 1. VARIANTS OF THE MODEL #

# 1.A Model with Fixed Effects and Controls 

did_with_controls <- lm(Wages ~ treatment * post + Employment + Inflation +
                          factor(Country) + factor(Year) + factor(Quarter), data = final_data_tertiary)
cat("\n--- Final DiD Estimation Regression Summary with Fixed Effects ---\n")
summary(did_with_controls)


# 1.B Model with Fixed Effects and without Controls

did_without_controls <- feols(Wages ~ post * treatment | Country + Year + Quarter, data = final_data_tertiary)
summary(did_without_controls)


# 2. OUTLIERS # 

# 2.A Identification (z-score)

final_data_tertiary$z_score <- scale(final_data_tertiary$Wages)
outliers <- final_data_tertiary %>% filter(abs(z_score) > 3)
print(outliers)


# 3. ETEROGENITY OF TREATMENTS #

# 3.A Heterogeneity by employment level

final_data_tertiary_eter_empl <- final_data_tertiary %>%
  mutate(High_Employment = ifelse(Employment > median(Employment, na.rm = TRUE), 1, 0))

model_heterogeneity_empl_tertiary <- feols(Wages ~ post * treatment * High_Employment + Inflation | Country + Year + Quarter, 
                             data = final_data_tertiary_eter_empl)
summary(model_heterogeneity_empl_tertiary)

# 3.B Heterogeneity by inflation level 

final_data_tertiary_eter_inflation <- final_data_tertiary %>%
  mutate(High_Inflation = ifelse(Inflation > median(Inflation, na.rm = TRUE), 1, 0))

model_heterogeneity_infl_tertiary <- feols(Wages ~ post * treatment * High_Inflation + Employment | Country + Year + Quarter, 
                                       data = final_data_tertiary_eter_inflation)
summary(model_heterogeneity_infl_tertiary)


# 4. CHECK FOR MULTICOLLINEARITY


model_vif <- lm(Wages ~ post * treatment + Employment + Inflation, data = final_data_tertiary)
vif(model_vif)








