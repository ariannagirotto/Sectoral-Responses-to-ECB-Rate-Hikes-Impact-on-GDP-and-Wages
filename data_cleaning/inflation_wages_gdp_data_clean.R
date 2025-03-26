library(dplyr)
library(lubridate)
library(tidyr)

## DROPPING UNNECESSARY COLUMNS
Wages_and_Salaries <- Wages_and_Salaries[7:9]
GDP <- GDP[7:9]

## AGGREGATION QUARTERLY BY MEAN

Inflation_Rate <- Inflation_Rate %>% 
  mutate(Time = ym(Time),   
         Year = year(Time), 
         Quarter = quarter(Time)) %>%  
  select(-Time)  

Inflation_Rate_Quarterly <- Inflation_Rate %>%
  group_by(Year, Quarter) %>%
  summarise(across(everything(), ~ mean(as.numeric(gsub(",", ".", .)), na.rm = TRUE))) %>%  
  ungroup()

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  mutate(across(where(is.numeric), ~ round(., 2)))  

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  mutate(Quarter = case_when(
    Quarter == 1 ~ "Q1",
    Quarter == 2 ~ "Q2",
    Quarter == 3 ~ "Q3",
    Quarter == 4 ~ "Q4"
  ))

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  pivot_longer(cols = -c(Year, Quarter),  
               names_to = "Country",     
               values_to = "Value")      

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  select(Country, Year, Quarter, Value)  

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  mutate(Country = recode(Country,
                          "Croazia" = "Croatia",
                          "Cipro" = "Cyprus",
                          "Estonia" = "Estonia",
                          "Finlandia" = "Finland",
                          "Francia" = "France",
                          "Germania" = "Germany",
                          "Grecia" = "Greece",
                          "Irlanda" = "Ireland",
                          "Italia" = "Italy",
                          "Latvia" = "Latvia",
                          "Lituania" = "Lithuania",
                          "Lussemburgo" = "Luxembourg",
                          "Malta" = "Malta",
                          "Netherlands" = "Netherlands",
                          "Portogallo" = "Portugal",
                          "Slovacchia" = "Slovakia",
                          "Slovenia" = "Slovenia",
                          "Spagna" = "Spain"
  ))


head(Inflation_Rate_Quarterly_Long)



## CHANGING THE NAMES FOR THE COLUMNS AND EUROAREA

GDP <- GDP %>%
  rename(Country = geo, 
         Value = OBS_VALUE)

Wages_and_Salaries <- Wages_and_Salaries %>%
  rename(Country = geo, 
         Value = OBS_VALUE)

GDP <- GDP %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

Wages_and_Salaries <- Wages_and_Salaries %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))


## SPLITTING QUARTERS AND YEARS

# GDP
GDP <- GDP %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

GDP$Year <- as.numeric(GDP$Year)

# WAGES AND SALARY
Wages_and_Salaries <- Wages_and_Salaries %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Wages_and_Salaries$Year <- as.numeric(Wages_and_Salaries$Year)


## RESTRICTING THE TIME PERIOD FROM 2010 TO 2025 (WITHOUT 2020)

GDP <- GDP %>%
  filter(Year >= 2010 & Year != 2020)

Wages_and_Salaries <- Wages_and_Salaries %>%
  filter(Year >= 2010 & Year != 2020)

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  filter(Year >= 2010 & Year != 2020)


## CHECK THAT THERE ARE ALL THE QUARTERS FOR EACH YEAR

# GDP

complete_data <- GDP %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# WAGES AND SALARIES

complete_data <- Wages_and_Salaries %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# INFLATION RATE

complete_data <- Inflation_Rate_Quarterly %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

## DROPPING THE 4TH QUARTER OF 2024

GDP <- GDP %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Wages_and_Salaries <- Wages_and_Salaries %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Inflation_Rate_Quarterly <- Inflation_Rate_Quarterly %>%
  filter(!(Year == 2025))


## CHECKING FOR NA AND DUPLICATES

# Checking for NA
sum(is.na(GDP))
sum(is.na(Wages_and_Salaries))
sum(is.na(Inflation_Rate_Quarterly))

# Checking for duplicates
sum(duplicated(GDP))
sum(duplicated(Wages_and_Salaries))
sum(duplicated(Inflation_Rate_Quarterly))


## SAVING THE CLEANED TABLES IN .csv 

write.csv(Inflation_Rate_Quarterly, file = "/Users/ariannagirotto/Desktop/Inflation_Rate_Quarterly.csv", 
          row.names = FALSE)
write.csv(GDP, file = "/Users/ariannagirotto/Desktop/GDP.csv", row.names = FALSE)
write.csv(Wages_and_Salaries, file = "/Users/ariannagirotto/Desktop/Wages_and_Salaries.csv", row.names = FALSE)










