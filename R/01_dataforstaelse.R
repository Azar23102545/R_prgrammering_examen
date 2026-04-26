library(tidyverse)
library(here)

# Läs in data
df_raw <- read_csv(here("data", "insurance_costs.csv"))

# Grundläggande dataöversikt
dim(df_raw)
names(df_raw)
glimpse(df_raw)

# Saknade värden per variabel
colSums(is.na(df_raw))

# Unika värden i kategoriska variabler
unique(df_raw$sex)
unique(df_raw$region)
unique(df_raw$smoker)
unique(df_raw$chronic_condition)
unique(df_raw$exercise_level)
unique(df_raw$plan_type)

# Statistisk sammanfattning
summary(df_raw)

# Observerade inkonsekvenser i rådata:
# - bmi, exercise_level och annual_checkups har saknade värden
# - region: inkonsekvent stavning, t.ex. "North" vs "south"
# - smoker: blandade versaler och mellanslag, t.ex. "Yes" vs "no"
# - plan_type: inkonsekvent stavning, t.ex. "basic" vs "Premium"
