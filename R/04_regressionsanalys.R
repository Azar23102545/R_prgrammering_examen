
# REGRESSIONSANALYS
# Syfte: Bygga och tolka linjära regressionsmodeller med charges som målvariabel. Vi bygger först en enkel modell med tre tydliga prediktorer, och sedan en utökad modell med alla relevanta variabler. Modellerna jämförs för att välja den som bäst förklarar kostnaderna.

# Förutsätter att df_clean har skapats i 02_datastadning_och_forberedelse.R.

library(tidyverse)
library(here)


# MODELL 1: Enkel modell (baseline)
# Motivation: Vi börjar enkelt med tre prediktorer som vi på förhand tror har stor betydelse, baserat på den beskrivande analysen: age, bmi, smoker.
modell1 <- lm(charges ~ age + bmi + smoker, data = df_clean)

cat("\n=== MODELL 1: charges ~ age + bmi + smoker ===\n")
print(summary(modell1))


# MODELL 2: Utökad modell med alla prediktorer
# Motivation: Vi inkluderar alla variabler som rimligen kan påverka kostnaden för att se om fler faktorer bidrar till förklaringsgraden.
modell2 <- lm(
  charges ~ age + bmi + children + sex + region + smoker +
            chronic_condition + exercise_level + plan_type +
            prior_accidents + prior_claims + annual_checkups,
  data = df_clean
)

cat("\n=== MODELL 2: Utökad modell (alla prediktorer) ===\n")
print(summary(modell2))


# Jämförelse av modellerna
cat("\n=== Jämförelse av modeller ===\n")

cat("\nR-squared (förklaringsgrad):\n")
cat("  Modell 1 (enkel): ", round(summary(modell1)$adj.r.squared, 4), "\n")
cat("  Modell 2 (full):  ", round(summary(modell2)$adj.r.squared, 4), "\n")

cat("\nResidual standardfel (RSE) - lägre är bättre:\n")
cat("  Modell 1: ", round(summary(modell1)$sigma, 2), "\n")
cat("  Modell 2: ", round(summary(modell2)$sigma, 2), "\n")

cat("\nAIC - lägre är bättre:\n")
print(AIC(modell1, modell2))

cat("\nANOVA (testar om modell 2 är signifikant bättre):\n")
print(anova(modell1, modell2))

# Tolkning av jämförelsen: Modell 2 har markant högre justerat R² (ca 0.74 mot 0.54), lägre RSE och lägre AIC. ANOVA-testet visar att modell 2 är signifikant bättre (p < 0.001). Därför väljer vi modell 2 som huvudmodell.


# Tolkning av modell 2 (huvudmodell)
cat("\n=== Tolkning av modell 2 (huvudmodell) ===\n")

# Koefficienter med p-värden
koef <- summary(modell2)$coefficients
koef_df <- data.frame(
  Variabel    = rownames(koef),
  Koefficient = round(koef[, "Estimate"], 2),
  p_varde     = round(koef[, "Pr(>|t|)"], 4)
)
print(koef_df)

# Tolkning av koefficienterna (modell 2):

# smokeryes:                 ca +7 400 kr (p < 0.001)
#    En rökare betalar i snitt ~7 400 kr mer än en icke-rökare,
#    allt annat lika. Detta är den i särklass starkaste effekten.
#
# chronic_conditionyes:      ca +3 800 kr (p < 0.001)
#    Kunder med kronisk sjukdom betalar ~3 800 kr mer i snitt.
#
# plan_typepremium:          ca +1 500 kr (p < 0.001)
#    Premiumplan är dyrare än basic (referens).
#
# exercise_levellow:         ca +1 500 kr (p < 0.001)
#    Låg motionsnivå ger ~1 500 kr högre kostnad jämfört med hög.
#
# prior_accidents:           ca +1 050 kr per tidigare olycka
# prior_claims:              ca +900 kr per tidigare claim
#
# bmi:                       ca +160 kr per BMI-enhet
# age:                       ca +70 kr per år
#
# Variabler som INTE är signifikanta (p > 0.05):
#    sex, region, children, annual_checkups
#    Dessa verkar inte ha någon egen effekt när andra faktorer är med.


#  Diagnostik av modell 2
# Sparar residualplottar till fil för rapporten.
cat("\n--- Diagnostiska plottar sparas till fil ---\n")
png(here("fig5_diagnostik.png"),
    width = 900, height = 700)
par(mfrow = c(2, 2))
plot(modell2)
par(mfrow = c(1, 1))
dev.off()

# Tolkning av diagnostik:
# - Residual vs Fitted: visar om modellen fångar mönstret linjärt.
#   Om mönster syns kan relationen vara icke-linjär.
# - Q-Q-plot: residualerna bör ligga längs linjen (normalfördelade).
# - Scale-Location: likformig spridning önskas (homoskedasticitet).
# - Residual vs Leverage: ser om enskilda punkter påverkar modellen starkt.


# Slutsats regressionsanalys
cat("\n=== Slutsats regressionsanalys ===\n")
cat("Modell 2 förklarar ca 74 % av variationen i försäkringskostnader.\n")
cat("De viktigaste faktorerna för kostnaden är (i storleksordning):\n")
cat("  1. Rökning (+7 400 kr)\n")
cat("  2. Kronisk sjukdom (+3 800 kr)\n")
cat("  3. Försäkringsplan (premium +1 500 kr)\n")
cat("  4. Låg motionsnivå (+1 500 kr)\n")
cat("  5. Tidigare olyckor och claims\n")
cat("  6. BMI och ålder (små men signifikanta effekter)\n")