##### This script involves the data cleaning process for the experimental data

# Load packages
if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, knitr, haven) 

# Load in data ------------------------------------------------------------
# Load raw data (data is available upon request)
data_raw <- read_sav("data_raw.sav")

# Load forthright respondent data
data_respondents <- read_csv("appends.csv")
# note: 12 unfinished surveys

# Combine Data
data_raw <- inner_join(data_raw, data_respondents, by=c("RESPONDENT_ID"="hash_value"))

# Data Wrangling -----------------------------------------------------------
# Select only the columns we care about and fix the scaling on one question
data_subset <- data_raw %>% 
  select(6, 18:39, 41, 44, 45, 46) %>% 
  mutate(Q4_1 = Q4_1-7,
         Q4_2 = Q4_2-7,
         Q4_3 = Q4_3-7,
         Q4_4 = Q4_4-7)

# Rename Columns to be more informative
cols <- colnames(data_subset)
for (i in 1:length(cols)){
  if(grepl(pattern= "^Q1_", x=cols[i])){
    cols[i] <- paste("DV1",cols[i],sep="_")
  } else if(grepl(pattern= "^Q2", x=cols[i])){
    cols[i] <- paste("DV2",cols[i],sep="_")
  } else if(grepl(pattern= "^Q3", x=cols[i])){
    cols[i] <- paste("mediator",cols[i],sep="_")
  } else if(grepl(pattern= "^Q3", x=cols[i])){
    cols[i] <- paste("mediator",cols[i],sep="_")
  } else if(grepl(pattern= "^Q4", x=cols[i])){
    cols[i] <- paste("DV3",cols[i],sep="_")
  } else if(grepl(pattern= "^Q5", x=cols[i])){
    cols[i] <- paste("country",cols[i],sep="_")
  } else if(grepl(pattern= "^Q9", x=cols[i])){
    cols[i] <- paste("open",cols[i],sep="_")
  } else if(grepl(pattern= "^Q15", x=cols[i])){
    cols[i] <- paste("behavior",cols[i],sep="_")
  } else if(grepl(pattern= "^Q14", x=cols[i])){
    cols[i] <- paste("MC",cols[i],sep="_")
  } 
}
colnames(data_subset) = cols

# Replace NA's in manipulation check with zeros
data_subset$MC_Q14_1 <- ifelse(is.na(data_subset$MC_Q14_1), 0, data_subset$MC_Q14_1)
data_subset$MC_Q14_2 <- ifelse(is.na(data_subset$MC_Q14_2), 0, data_subset$MC_Q14_2)
data_subset$MC_Q14_3 <- ifelse(is.na(data_subset$MC_Q14_3), 0, data_subset$MC_Q14_3)
data_subset$MC_Q14_4 <- ifelse(is.na(data_subset$MC_Q14_4), 0, data_subset$MC_Q14_4)

# Count NAs - 14 missed questions
sum(is.na(data_subset[2:17]))

# Impute mean for NAs
for (i in 1:length(data_subset)){
  for (j in 1:nrow(data_subset))
    data_subset[[j,i]] <- ifelse(is.na(data_subset[[j,i]]), mean(data_subset[[i]], na.rm = TRUE), data_subset[[j,i]])
}

# Reverse code questions so that all higher responses are favorable immigration attitudes
reverse_order <- function(data) {
  as.numeric(recode(
    as.character(data),
    "1" = "5",
    "2" = "4",
    "3" = "3",
    "4" = "2",
    "5" = "1"
  ))
}
# Specify which questions need to be reverse coded
reverse_cols <- c("DV1_Q1_1","DV1_Q1_4","DV2_Q2_2","DV2_Q2_3","mediator_Q3_2","DV3_Q4_2","DV3_Q4_4")
for (i in 1:length(cols)) {
  for (j in 1:length(reverse_cols)) {
    if (cols[i] == reverse_cols[j]) {
      data_subset[[i]] <- reverse_order(data_subset[[i]])
    }
  }
}

# change order of behavior question
data_subset$behavior_Q15 <- ifelse(data_subset$behavior_Q15 > 1, 0, data_subset$behavior_Q15)

# code to Dems and Rs
# note: political party: 1- strong Dem, 7 strong R, 98 other, 99 no preference
data_subset$democrat <- NA
data_subset$democrat <- as.numeric(recode(
  as.character(data_subset$political_party_preference),
  "1" = "1",
  "2" = "1",
  "3" = "1",
  "4" = "0",
  "5" = "0",
  "6" = "0",
  "7" = "0",
  "98" = "0",
  "99" = "0"
))
data_subset$republican <- NA
data_subset$republican <- as.numeric(recode(
  as.character(data_subset$political_party_preference),
  "1" = "0",
  "2" = "0",
  "3" = "0",
  "4" = "0",
  "5" = "1",
  "6" = "1",
  "7" = "1",
  "98" = "0",
  "99" = "0"
))

# deal with unknowns for income
data_subset$income <- ifelse(data_subset$income == 99, mean(data_subset$income), data_subset$income)

# Convert to numeric
data_numeric <- data.frame(lapply(data_subset[1:21], as.numeric))
data_clean <- data.frame(cbind(data_numeric, data_subset))
# Subset
data_clean <- data_clean %>% 
  select(1:21,43:50) 

# Create indices
data_clean <- data_clean %>% 
  mutate(DV1_family = (DV1_Q1_1+DV1_Q1_2+DV1_Q1_3+DV1_Q1_4)/4,
         DV2_policy = (DV2_Q2_1+DV2_Q2_2+DV2_Q2_3+DV2_Q2_4)/4,
         DV3a_voluntary = (DV3_Q4_1+DV3_Q4_2)/2,
         DV3b_resposible = (DV3_Q4_3+DV3_Q4_4)/2) %>% 
  rename(empathy = mediator_Q3_1,
         anger = mediator_Q3_2,
         deservingness = mediator_Q3_3,
         responsibleA = DV3_Q4_3,
         responsibleB = DV3_Q4_4,
         MC_economic = MC_Q14_1,
         MC_starvation = MC_Q14_2,
         MC_persecution = MC_Q14_3,
         MC_unsure= MC_Q14_4)

# Fix conditions
data_clean$starvation_threat <- 0
data_clean$persecution_threat <- 0
# Starvation threat
data_clean$starvation_threat <- ifelse(data_clean[[24]] == "Condition 2 (Starvation)",1,data_clean$starvation_threat)
data_clean$starvation_threat <- ifelse(data_clean[[24]] == "Condition 4 (Combined)",1,data_clean$starvation_threat)
# Persecution threat
data_clean$persecution_threat <- ifelse(data_clean[[24]] == "Condition 3 (Persecution)",1,data_clean$persecution_threat)
data_clean$persecution_threat <- ifelse(data_clean[[24]] == "Condition 4 (Combined)",1,data_clean$persecution_threat)
# Convert to factor
data_clean$starvation_threat <- as.factor(data_clean$starvation_threat)
data_clean$persecution_threat <- as.factor(data_clean$persecution_threat)

# Write clean data to csv file
write_csv(data_clean,"data_clean.csv")
