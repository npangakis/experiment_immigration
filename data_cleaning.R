##### This script involves the data cleaning process for the experimental data

# Load packages
if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, knitr, haven, skimr) 

# Load in data ------------------------------------------------------------
# Load raw data (data is available upon request, after confirming with IRB)
data_raw <- read_sav("data_raw.sav")

# Load forthright respondent data
data_respondents <- read_csv("appends.csv") %>% 
  select(hash_value, education, hispanic, income, gender, 
         political_party_preference, region, state, familial_origin)

# Combine experimental data with respondent data
data <- inner_join(data_raw, data_respondents, 
                   by = c("RESPONDENT_ID"="hash_value"))

# Data Cleaning -----------------------------------------------------------

# Create function to reverse code questions so that all higher responses are favorable immigration attitudes
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

# Clean up the data
data_clean <- data %>% 
  select(6, 18:48) %>%
  rename(mc_lina_violence = Q14_1,
         mc_lina_starv = Q14_2,
         mc_imm_violence = Q18_1,
         mc_imm_starv = Q18_2,
         openended = Q9) %>% 
  mutate(
         # Combine attention check question
         intermediary_combined = coalesce(intermediary_Q, intermediary_Q.0, 
                                          intermediary_Q.1),
         
         # Create treatment variables
         control = case_when(condition == "Control" ~ 1, TRUE ~ 0),
         starvation = case_when(condition == "Starvation" ~ 1, TRUE ~ 0),
         violence = case_when(condition == "Violence" ~ 1, TRUE ~ 0),
         condition = as.factor(condition),
         
         # recode partisanship variable
         political_party_preference = case_when(political_party_preference == 98 | 
                                                  political_party_preference == 99 ~ 4, 
                                                TRUE ~ political_party_preference),
         # note: political party: 1- strong Dem, 7 strong R, 98 other, 99 no preference
         
         # create Republican variable
         republican = case_when(political_party_preference == 5 | 
                                  political_party_preference == 6 |
                                  political_party_preference == 7 ~ 1, 
                                TRUE ~ 0),
         # create Democrat variable
         democrat = case_when(political_party_preference <= 3 ~ 1, TRUE ~ 0),
         
         # partisanship reduced
         political_party_preference_reduced = case_when(political_party_preference > 4 
                                                        ~ 3, 
                                                        political_party_preference < 4 ~ 0,
                                                        political_party_preference == 4 ~ 1,
                                                        TRUE ~ political_party_preference),
         
         # recode hispanic
         hispanic = case_when(hispanic == 2 ~ 0, 
                              TRUE ~ 1),
         
         # recode income
         income  = case_when(income == 99 ~ mean(income), 
                             TRUE ~ income),
         
         # fix ordering for some questions
         Q4_1 = Q4_1-7,
         Q4_2 = Q4_2-7,
         Q4_3 = Q4_3-7,
         Q4_4 = Q4_4-7,
         priority_poverty = DV4___1-7,
         priority_violence = DV4___2-7,
         
         # reverse order for certain questions
         DV1___1 = reverse_order(DV1___1),
         DV1___4 = reverse_order(DV1___4),
         DV2___2 = reverse_order(DV2___2),
         Q4_2 = reverse_order(Q4_2),
         Q4_4 = reverse_order(Q4_4),
         
         # manually recode behavior question
         behavior_q = as.numeric(recode(as.character(Q15),
                                        "1" = "4",
                                        "2" = "3",
                                        "3" = "2",
                                        "4" = "1")),
         # manually recode certain questions
         mc_lina_violence_binary = case_when(mc_lina_violence <= 3 ~ 0, TRUE ~ 1),
         mc_lina_starv_binary = case_when(mc_lina_starv <= 3 ~ 0, TRUE ~ 1),
         mc_imm_violence_binary = case_when(mc_imm_violence <= 3 ~ 0, TRUE ~ 1),
         mc_imm_starv_binary = case_when(mc_imm_starv <= 3 ~ 0, TRUE ~ 1),
         
         # Create indices
         DV1_lina = (DV1___1+DV1___2+DV1___3+DV1___4)/4,
         DV2_policy = (DV2___1+DV2___2+DV2___3+priority_poverty+priority_violence)/5,
         DV3_voluntary = ((Q4_1+Q4_2)/2),
         DV4_blame = ((Q4_3+Q4_4)/2),
         DV5_voluntary_combined = ((Q4_1+Q4_2+Q4_3+Q4_4)/4))%>% 
  rename(attention_check = intermediary_combined) %>% 
  # drop unneeded columns
  select(-intermediary_Q, -intermediary_Q.0, -intermediary_Q.1, 
         -RESPONDENT_ID, -DV4___1, -DV4___2, -Q15) 



# Write clean data to csv file
write_csv(data_clean,"data_clean.csv")






