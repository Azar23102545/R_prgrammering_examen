# DATAFÖRSTÅELSE
# Syfte: Läsa in datasetet och undersöka dess storlek, struktur, variabeltyper, saknade värden och eventuella inkonsekvenser.

library(tidyverse)
library(here)

# Läs in data 
df_raw <- read_csv(here("data", "insurance_costs.csv"))


# Storlek och struktur 
cat("\n--- Datasetets storlek ---\n")
print(dim(df_raw))

cat("\n--- Variabelnamn ---\n")
print(names(df_raw))

cat("\n--- Struktur (variabeltyper) ---\n")
glimpse(df_raw)


# Saknade värden
cat("\n--- Antal saknade värden per variabel ---\n")
print(colSums(is.na(df_raw)))


#  Unika värden i kategoriska variabler (letar inkonsekvenser)
cat("\n--- Unika värden i kategoriska variabler ---\n")
cat("\nsex:\n");               print(unique(df_raw$sex))
cat("\nregion:\n");            print(unique(df_raw$region))
cat("\nsmoker:\n");            print(unique(df_raw$smoker))
cat("\nchronic_condition:\n"); print(unique(df_raw$chronic_condition))
cat("\nexercise_level:\n");    print(unique(df_raw$exercise_level))
cat("\nplan_type:\n");         print(unique(df_raw$plan_type))


# Statistisk sammanfattning av numeriska variabler
cat("\n--- Statistisk sammanfattning ---\n")
print(summary(df_raw))


# Kommentar om observerade inkonsekvenser
# Vid denna inspektion ser vi att:
# - bmi har saknade värden
# - exercise_level har saknade värden
# - annual_checkups har saknade värden
# - region innehåller inkonsekvent stavning (t.ex. "North", "South ")
# - smoker innehåller "Yes", "no " (stora bokstäver och mellanslag)
# - plan_type innehåller "basic ", "Premium", "Standard"