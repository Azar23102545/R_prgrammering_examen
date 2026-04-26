library(tidyverse)
library(here)

# Förutsätter att df_raw har skapats i 01_dataforstaelse.R

# Städa kategoriska variabler (ta bort mellanslag, gör om till gemener)
df_clean <- df_raw %>%
  mutate(
    sex               = str_to_lower(str_trim(sex)),
    region            = str_to_lower(str_trim(region)),
    smoker            = str_to_lower(str_trim(smoker)),
    chronic_condition = str_to_lower(str_trim(chronic_condition)),
    exercise_level    = str_to_lower(str_trim(exercise_level)),
    plan_type         = str_to_lower(str_trim(plan_type))
  )

# Hantera saknade värden
# bmi och annual_checkups: median-imputation
# exercise_level: ersätts med vanligaste kategorin ("medium")
df_clean <- df_clean %>%
  mutate(
    bmi             = if_else(is.na(bmi), median(bmi, na.rm = TRUE), bmi),
    annual_checkups = if_else(is.na(annual_checkups), median(annual_checkups, na.rm = TRUE), annual_checkups),
    exercise_level  = if_else(is.na(exercise_level), "medium", exercise_level)
  )

# Skapa nya variabler
# bmi_category: WHO:s standardkategorier
# age_group: tre åldersgrupper för enklare jämförelse
# risk_score: summerar tidigare olyckor och claims
df_clean <- df_clean %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25   ~ "Normal",
      bmi < 30   ~ "Overweight",
      TRUE       ~ "Obese"
    ),
    age_group = case_when(
      age <= 30 ~ "Young (18-30)",
      age <= 50 ~ "Middle (31-50)",
      TRUE      ~ "Senior (51+)"
    ),
    risk_score = prior_accidents + prior_claims
  )
