library(tidyverse)
library(here)

# Förutsätter att df_clean har skapats i 02_datastadning_och_forberedelse.R

# Modell 1: Enkel baseline-modell
# Tre prediktorer med förväntad stark effekt baserat på beskrivande analysen
modell1 <- lm(charges ~ age + bmi + smoker, data = df_clean)
summary(modell1)

# Modell 2: Utökad modell med alla relevanta prediktorer
modell2 <- lm(
  charges ~ age + bmi + children + sex + region + smoker +
            chronic_condition + exercise_level + plan_type +
            prior_accidents + prior_claims + annual_checkups,
  data = df_clean
)
summary(modell2)

# Jämförelse av modellerna
round(summary(modell1)$adj.r.squared, 4)  # Justerat R² modell 1
round(summary(modell2)$adj.r.squared, 4)  # Justerat R² modell 2

round(summary(modell1)$sigma, 2)  # RSE modell 1
round(summary(modell2)$sigma, 2)  # RSE modell 2

AIC(modell1, modell2)
anova(modell1, modell2)

# Modell 2 har markant högre justerat R² (ca 0.74 mot 0.54), lägre RSE och
# lägre AIC. ANOVA visar signifikant förbättring (p < 0.001) — modell 2 väljs.

# Koefficienter med p-värden för modell 2
koef <- summary(modell2)$coefficients
koef_df <- data.frame(
  Variabel    = rownames(koef),
  Koefficient = round(koef[, "Estimate"], 2),
  p_varde     = round(koef[, "Pr(>|t|)"], 4)
)
koef_df

# Starkaste effekter i modell 2:
# smokeryes:         ca +7 400 kr — rökare betalar markant mer (p < 0.001)
# chronic_condition: ca +3 800 kr — kronisk sjukdom ökar kostnaden (p < 0.001)
# plan_type och exercise_level har också signifikanta effekter
