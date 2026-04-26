# DATASTÄDNING OCH FÖRBEREDELSE
# Syfte: Hantera saknade värden, städa kategoriska variabler, skapa nya variabler som är relevanta för analysen.

# Förutsätter att df_raw har skapats i 01_dataforstaelse.R.

library(tidyverse)
library(here)


# Städa kategoriska variabler
# Vi använder str_trim() för att ta bort eventuella mellanslag i början/slutet,
# och str_to_lower() för att alla värden ska skrivas i små bokstäver.
# På så sätt slår vi ihop t.ex. "North", "north" och "North " till ett värde.
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
# bmi             : median-imputation (numerisk, robust mot extremvärden)
# annual_checkups : median-imputation (diskret numerisk)
# exercise_level  : ersätter med vanligaste kategorin ("medium")
#
# Motivation: Andelen saknade värden är liten (< 3 %) och variablerna är
# viktiga för analysen. Att ta bort raderna skulle minska datamängden i onödan.
df_clean <- df_clean %>%
  mutate(
    bmi             = if_else(is.na(bmi),
                              median(bmi, na.rm = TRUE),
                              bmi),
    annual_checkups = if_else(is.na(annual_checkups),
                              median(annual_checkups, na.rm = TRUE),
                              annual_checkups),
    exercise_level  = if_else(is.na(exercise_level),
                              "medium",
                              exercise_level)
  )


# Skapa nya variabler
# bmi_category:  Standardkategorisering enligt WHO. Förenklar tolkning och
#                gör det lättare att se mönster i grupper än i råa BMI-värden.
# age_group:     Delar in kunder i tre naturliga åldersgrupper. Användbart för
#                att jämföra kostnader mellan grupper i visualiseringar.
# risk_score:    Kombinerar tidigare olyckor och claims till ett riskmått.
#                En kund med hög risk_score har visat historisk skaderisk.
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


# Konvertera kategoriska variabler till faktorer
# Faktorer ger rätt ordning vid visualisering och regression.
df_clean <- df_clean %>%
  mutate(
    sex               = factor(sex),
    region            = factor(region),
    smoker            = factor(smoker, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    exercise_level    = factor(exercise_level,
                               levels = c("low", "medium", "high")),
    plan_type         = factor(plan_type,
                               levels = c("basic", "standard", "premium")),
    bmi_category      = factor(bmi_category,
                               levels = c("Underweight", "Normal",
                                          "Overweight", "Obese")),
    age_group         = factor(age_group,
                               levels = c("Young (18-30)",
                                          "Middle (31-50)",
                                          "Senior (51+)"))
  )


# Verifiera att städningen fungerat
cat("\n--- Saknade värden efter städning ---\n")
print(colSums(is.na(df_clean)))

cat("\n--- Unika värden i städade kategoriska variabler ---\n")
cat("region:");         print(levels(df_clean$region))
cat("smoker:");         print(levels(df_clean$smoker))
cat("plan_type:");      print(levels(df_clean$plan_type))
cat("exercise_level:"); print(levels(df_clean$exercise_level))

cat("\n--- Struktur på färdigstädat dataset ---\n")
glimpse(df_clean)