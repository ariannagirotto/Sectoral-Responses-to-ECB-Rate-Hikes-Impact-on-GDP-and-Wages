library(readr)
library(dplyr)
library(tidyr)

### DATA PREPARATION AND DATA CLEANING

### Importing dataset
Employment_tertiary_sector <- read_csv("Desktop/Dataset_Causal/Employment_Wholesale.csv")
Employment_Construction <- read_csv("Desktop/Dataset_Causal/Employment_Construction.csv")
Employment_Manifacturing <- read_csv("Desktop/Dataset_Causal/Employment_Manifacturing.csv")
Employment_Industry_No_Constrution <- read_csv("Desktop/Dataset_Causal/Employment_Industry_No_Constrution.csv")
Employment_primary_sector <- read_csv("Desktop/Dataset_Causal/Employment_Agriculture.csv")

### Removing columns
columns_to_remove <- c("DATAFLOW", "LAST UPDATE", "freq", "unit", "s_adj", "na_item", "OBS_FLAG", "CONF_STATUS")

Employment_tertiary_sector <- Employment_tertiary_sector %>% select(-all_of(columns_to_remove))
Employment_Construction <- Employment_Construction %>% select(-all_of(columns_to_remove))
Employment_Manifacturing <- Employment_Manifacturing %>% select(-all_of(columns_to_remove))
Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>% select(-all_of(columns_to_remove))
Employment_primary_sector <- Employment_primary_sector %>% select(-all_of(columns_to_remove))

### Splitting the column "TIME_PERIOD" in "Year" and "Quarter"
Employment_tertiary_sector <- Employment_tertiary_sector %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Employment_Construction <- Employment_Construction %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Employment_Manifacturing <- Employment_Manifacturing %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Employment_primary_sector <- Employment_primary_sector %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

Employment_tertiary_sector$Year <- as.numeric(Employment_tertiary_sector$Year)
Employment_Construction$Year <- as.numeric(Employment_Construction$Year)
Employment_Manifacturing$Year <- as.numeric(Employment_Manifacturing$Year)
Employment_Industry_No_Constrution$Year <- as.numeric(Employment_Industry_No_Constrution$Year)
Employment_primary_sector$Year <- as.numeric(Employment_primary_sector$Year)

### Renaming the columns
Employment_tertiary_sector <- Employment_tertiary_sector %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

Employment_Construction <- Employment_Construction %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

Employment_Manifacturing <- Employment_Manifacturing %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

Employment_primary_sector <- Employment_primary_sector %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

### Renaming Euroarea
Employment_tertiary_sector <- Employment_tertiary_sector %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

Employment_Construction <- Employment_Construction %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

Employment_Manifacturing <- Employment_Manifacturing %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

Employment_primary_sector <- Employment_primary_sector %>%
  mutate(Country = recode(Country, 
                          "Euro area – 20 countries (from 2023)" = "Euroarea"))

### Selecting only "Year" >= 2010 and "Year" != 2020
Employment_tertiary_sector <- Employment_tertiary_sector %>%
  filter(Year >= 2010 & Year != 2020)

Employment_Construction <- Employment_Construction %>%
  filter(Year >= 2010 & Year != 2020)

Employment_Manifacturing <- Employment_Manifacturing %>%
  filter(Year >= 2010 & Year != 2020)

Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>%
  filter(Year >= 2010 & Year != 2020)

Employment_primary_sector <- Employment_primary_sector %>%
  filter(Year >= 2010 & Year != 2020)


### Checking for each state where there is a missing value in Year and Quarter
# Wholesale
complete_data <- Employment_tertiary_sector %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# Construction
complete_data <- Employment_Construction %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# Manifacturing
complete_data <- Employment_Manifacturing %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# Industry_No_Construction
complete_data <- Employment_Industry_No_Constrution %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)

# Agriculture
complete_data <- Employment_primary_sector %>%
  group_by(Country) %>%
  complete(Year, Quarter)

missing_entries <- complete_data %>%
  filter(is.na(Value))

print(missing_entries, n = Inf)


### Dropping all the rows where "Year" = 2024 and "Quarte" = Q4
Employment_tertiary_sector <- Employment_tertiary_sector %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Employment_Construction <- Employment_Construction %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Employment_Manifacturing <- Employment_Manifacturing %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Employment_Industry_No_Constrution <- Employment_Industry_No_Constrution %>%
  filter(!(Year == 2024 & Quarter == "Q4"))

Employment_primary_sector <- Employment_primary_sector %>%
  filter(!(Year == 2024 & Quarter == "Q4"))



### Checking for NA
colSums(is.na(Employment_tertiary_sector))
colSums(is.na(Employment_Construction))
colSums(is.na(Employment_Manifacturing))
colSums(is.na(Employment_Industry_No_Constrution))
colSums(is.na(Employment_primary_sector))

### Checking for duplicates
sum(duplicated(Employment_tertiary_sector))
sum(duplicated(Employment_Construction))
sum(duplicated(Employment_Manifacturing))
sum(duplicated(Employment_Industry_No_Constrution))
sum(duplicated(Employment_primary_sector))


### SUDDIVIDING INTO THE THREE SECTORS (PRIMARY, SECONDARY, TERZIARY)

Employment_secondary_sector <- bind_rows(Employment_Construction, Employment_Industry_No_Constrution,
                              Employment_Manifacturing)


### Creating csv for cleaned and prepered table
tables <- c("Employment_tertiary_sector", "Employment_secondary_sector", "Employment_primary_sector")

output_dir <- "/Users/sarasartini/Desktop/Dataset_Causal/Employment"


for (table in tables) {
  if (exists(table) && is.data.frame(get(table))) { 
    write.csv(get(table), file = file.path(output_dir, paste0(table, ".csv")), row.names = FALSE)
  }
}






