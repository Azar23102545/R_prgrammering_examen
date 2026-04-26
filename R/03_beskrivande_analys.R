library(tidyverse)
library(here)

# Förutsätter att df_clean har skapats i 02_datastadning_och_forberedelse.R

# Tabell 1: Statistisk sammanfattning av charges
charges_summary <- df_clean %>%
  summarise(
    n      = n(),
    medel  = round(mean(charges), 2),
    median = round(median(charges), 2),
    sd     = round(sd(charges), 2),
    min    = round(min(charges), 2),
    max    = round(max(charges), 2)
  )
charges_summary

# Kostnaderna varierar från ca 1 200 kr upp till ca 32 600 kr.
# Medelvärdet (ca 10 060 kr) är högre än medianen (ca 9 124 kr) — högerskev fördelning.

# Figur 1: Fördelning av försäkringskostnader
fig1 <- ggplot(df_clean, aes(x = charges)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean(charges)),
             color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "Fördelning av försäkringskostnader",
    subtitle = "Röd streckad linje = medelvärde",
    x        = "Försäkringskostnad (charges)",
    y        = "Antal kunder"
  ) +
  theme_minimal(base_size = 13)

fig1
ggsave(here("fig1_fordelning_charges.png"), fig1, width = 8, height = 5, dpi = 150)

# De flesta kunder ligger mellan 5 000–12 000 kr, men en mindre grupp har
# betydligt högre kostnader, troligen drivet av riskfaktorer som rökning.

# Figur 2: Kostnad för rökare vs icke-rökare
fig2 <- ggplot(df_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Försäkringskostnader: rökare vs icke-rökare",
    x     = "Rökare",
    y     = "Försäkringskostnad (charges)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

fig2
ggsave(here("fig2_rokare_vs_icke.png"), fig2, width = 8, height = 5, dpi = 150)

# Rökare har tydligt högre medelkostnad och större spridning än icke-rökare.
# Rökning verkar vara en stark prediktor för försäkringskostnad.
