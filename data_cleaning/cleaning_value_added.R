library(ggplot2)
library(corrplot)
library(cluster)
library(plotly)
library(dplyr)
library(tidyr)

###########################################
# Agriculture
###########################################

# Read the agriculture VA data
data <- read.csv("/Users/sarasartini/Desktop/Dataset_Causal/Value_Addes_Raw/agricolture_va.csv")
summary(data)
colnames(data)

# Drop the specified columns
agr <- data[, -c(1, 2, 3, 5, 6, 7, 11, 12)]

# Split the TIME_PERIOD column into Year and Quarter
agr <- agr %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

# Convert the Year column to numeric, if needed
agr$Year <- as.numeric(agr$Year)

# Optionally remove rows where geo is "Euro area – 20 countries (from 2023)"
# agr <- agr %>% filter(geo != "Euro area – 20 countries (from 2023)")

# Rename columns for clarity
agr <- agr %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

# Rename "Euro area – 20 countries (from 2023)" to "Euroarea"
agr <- agr %>%
  mutate(Country = ifelse(Country == "Euro area – 20 countries (from 2023)", "Euroarea", Country))

# Filter for Year >= 2010 and Year != 2020
agr <- agr %>%
  filter(Year >= 2010 & Year != 2020)

# Drop rows where Year is 2024 and Quarter is Q4
agr <- agr %>% 
  filter(!(Year == 2024 & Quarter == "Q4"))

# Check modifications
colnames(agr)
head(agr)
str(agr)
unique(agr$Country)

###########################################
# Construction
##########################################

# Read the construction VA data
construction <- read.csv("/Users/sarasartini/Desktop/Dataset_Causal/Value_Addes_Raw/construction_va.csv")

# Drop the specified columns
constr <- construction[, -c(1, 2, 3, 5, 6, 7, 11, 12)]

# Split the TIME_PERIOD column into Year and Quarter
constr <- constr %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

# Convert the Year column to numeric, if needed
constr$Year <- as.numeric(constr$Year)

# Optionally remove rows where geo is "Euro area – 20 countries (from 2023)"
# constr <- constr %>% filter(geo != "Euro area – 20 countries (from 2023)")

# Rename columns for clarity
constr <- constr %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

# Rename "Euro area – 20 countries (from 2023)" to "Euroarea"
constr <- constr %>%
  mutate(Country = ifelse(Country == "Euro area – 20 countries (from 2023)", "Euroarea", Country))

# Filter for Year >= 2010 and Year != 2020
constr <- constr %>%
  filter(Year >= 2010 & Year != 2020)

# Drop rows where Year is 2024 and Quarter is Q4
constr <- constr %>% 
  filter(!(Year == 2024 & Quarter == "Q4"))

# Check modifications
colnames(constr)
head(constr)
str(constr)
unique(constr$Country)

###########################################
# Manufactoring
###########################################

# Read the manufacturing VA data
manufactoring <- read.csv("/Users/sarasartini/Desktop/Dataset_Causal/Value_Addes_Raw/manufactoring_va.csv")

# Drop the specified columns
manuf <- manufactoring[, -c(1, 2, 3, 5, 6, 7, 11, 12)]

# Split the TIME_PERIOD column into Year and Quarter
manuf <- manuf %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

# Convert the Year column to numeric, if needed
manuf$Year <- as.numeric(manuf$Year)

# Optionally remove rows where geo is "Euro area – 20 countries (from 2023)"
# manuf <- manuf %>% filter(geo != "Euro area – 20 countries (from 2023)")

# Rename columns for clarity
manuf <- manuf %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

# Rename "Euro area – 20 countries (from 2023)" to "Euroarea"
manuf <- manuf %>%
  mutate(Country = ifelse(Country == "Euro area – 20 countries (from 2023)", "Euroarea", Country))

# Filter for Year >= 2010 and Year != 2020
manuf <- manuf %>%
  filter(Year >= 2010 & Year != 2020)

# Drop rows where Year is 2024 and Quarter is Q4
manuf <- manuf %>% 
  filter(!(Year == 2024 & Quarter == "Q4"))

# Check modifications
colnames(manuf)
head(manuf)
str(manuf)
unique(manuf$Country)

###########################################
# Industry non construction
###########################################

# Read the industry noconstruction VA data
industry_data <- read.csv("/Users/sarasartini/Desktop/Dataset_Causal/Value_Addes_Raw/industry noconstruction_va.csv")

# Drop the specified columns
industry <- industry_data[, -c(1, 2, 3, 5, 6, 7, 11, 12)]

# Split the TIME_PERIOD column into Year and Quarter
industry <- industry %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

# Convert the Year column to numeric, if needed
industry$Year <- as.numeric(industry$Year)

# Optionally remove rows where geo is "Euro area – 20 countries (from 2023)"
# industry <- industry %>% filter(geo != "Euro area – 20 countries (from 2023)")

# Rename columns for clarity
industry <- industry %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

# Rename "Euro area – 20 countries (from 2023)" to "Euroarea"
industry <- industry %>%
  mutate(Country = ifelse(Country == "Euro area – 20 countries (from 2023)", "Euroarea", Country))

# Filter for Year >= 2010 and Year != 2020
industry <- industry %>%
  filter(Year >= 2010 & Year != 2020)

# Drop rows where Year is 2024 and Quarter is Q4
industry <- industry %>% 
  filter(!(Year == 2024 & Quarter == "Q4"))

# Check modifications
colnames(industry)
head(industry)
str(industry)
unique(industry$Country)

###########################################
# Wholesale and retail trade, transport, accommodation and food service activities
###########################################

# Read the Wholesale and retail trade, transport, accommodation and food service activities VA data
tertiary_data <- read.csv("/Users/sarasartini/Desktop/Dataset_Causal/Value_Addes_Raw/Wholesale and retail trade, transport, accommodation and food service activities_va.csv")

# Drop the specified columns
tertiary <- tertiary_data[, -c(1, 2, 3, 5, 6, 7, 11, 12)]

# Split the TIME_PERIOD column into Year and Quarter
tertiary <- tertiary %>%
  separate(TIME_PERIOD, into = c("Year", "Quarter"), sep = "-")

# Convert the Year column to numeric, if needed
tertiary$Year <- as.numeric(tertiary$Year)

# Optionally remove rows where geo is "Euro area – 20 countries (from 2023)"
# tertiary <- tertiary %>% filter(geo != "Euro area – 20 countries (from 2023)")

# Rename columns for clarity
tertiary <- tertiary %>%
  rename(Sector = nace_r2, 
         Country = geo, 
         Value = OBS_VALUE)

# Rename "Euro area – 20 countries (from 2023)" to "Euroarea"
tertiary <- tertiary %>%
  mutate(Country = ifelse(Country == "Euro area – 20 countries (from 2023)", "Euroarea", Country))

# Filter for Year >= 2010 and Year != 2020
tertiary <- tertiary %>%
  filter(Year >= 2010 & Year != 2020)

# Drop rows where Year is 2024 and Quarter is Q4
tertiary <- tertiary %>% 
  filter(!(Year == 2024 & Quarter == "Q4"))

# Check modifications
colnames(tertiary)
head(tertiary)
str(tertiary)
unique(tertiary$Country)

# Check unique years and quarters in the filtered tertiary dataset
unique(tertiary$Year)
unique(tertiary$Quarter)

# Create a table to see the distribution by Year and Quarter
table(tertiary$Year, tertiary$Quarter)

# Verify that no rows exist for unwanted cases
any(tertiary$Year < 2010)                # Should return FALSE
any(tertiary$Year == 2020)               # Should return FALSE
any(tertiary$Year == 2024 & tertiary$Quarter == "Q4")  # Should return FALSE

# Create a complete grid for Country, Year, and Quarter for tertiary data
complete_data <- tertiary %>%
  group_by(Country) %>%
  complete(Year, Quarter)

# Filter for rows where Value is missing and print all entries
missing_entries <- complete_data %>%
  filter(is.na(Value))
print(missing_entries, n = Inf)

###########################################
# Write modified data frames to CSV files
###########################################

write.csv(agr, "/Users/sarasartini/Desktop/Dataset_Causal/Value_Added_Final/modified_primary_va.csv", row.names = FALSE)
write.csv(constr, "modified_construction_va.csv", row.names = FALSE)
write.csv(manuf, "modified_manufactoring_va.csv", row.names = FALSE)
write.csv(industry, "modified_industry_va.csv", row.names = FALSE)
write.csv(tertiary, "/Users/sarasartini/Desktop/Dataset_Causal/Value_Added_Final/modified_tertiary_va.csv", row.names = FALSE)





############################################
# Combinaning tables of second sector in one
############################################

# Unisci le tabelle accodandole
merged_df <- rbind(modified_construction_va, modified_industry_va, modified_manufactoring_va)

# Salva il dataframe risultante in un nuovo CSV
write.csv(merged_df, "/Users/sarasartini/Desktop/Dataset_Causal/Value_Added_Final/modified_secondary_va.csv", row.names = FALSE)
