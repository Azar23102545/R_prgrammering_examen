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

# Kostnaderna varierar från ca 1 200 kr till ca 32 600 kr.
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

# De flesta kunder ligger mellan 5 000–12 000 kr. En mindre grupp har betydligt
# högre kostnader, troligen drivet av riskfaktorer som rökning.

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

# Rökare har tydligt högre medelkostnad och större spridning — stark prediktor.

# Figur 3: Ålder vs kostnad uppdelat på rökning
fig3 <- ggplot(df_clean, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Ålder vs försäkringskostnad, uppdelat på rökning",
    subtitle = "Linjära trendlinjer per grupp",
    x        = "Ålder",
    y        = "Försäkringskostnad (charges)",
    color    = "Rökare"
  ) +
  theme_minimal(base_size = 13)

fig3
ggsave(here("fig3_alder_charges.png"), fig3, width = 8, height = 5, dpi = 150)

# Kostnaden ökar med åldern för båda grupper, men rökare ligger konsekvent högre.
# Rökning adderar en tydlig premie oberoende av ålder.

# Figur 4: Medelkostnad per försäkringsplan
fig4 <- df_clean %>%
  group_by(plan_type) %>%
  summarise(medel_charges = mean(charges), .groups = "drop") %>%
  ggplot(aes(x = plan_type, y = medel_charges, fill = plan_type)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(round(medel_charges / 1000, 1), "k")),
            vjust = -0.4, size = 4.5) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Genomsnittlig försäkringskostnad per plan",
    x     = "Försäkringsplan",
    y     = "Medelkostnad (SEK)"
  ) +
  theme_minimal(base_size = 13)

fig4
ggsave(here("fig4_kostnad_per_plan.png"), fig4, width = 8, height = 5, dpi = 150)

# Premium har högst medelkostnad, följt av standard och basic.

# Tabell 2: Medelkostnad per nyckelgrupp
df_clean %>% group_by(smoker) %>%
  summarise(n = n(), medel = round(mean(charges), 2))

df_clean %>% group_by(chronic_condition) %>%
  summarise(n = n(), medel = round(mean(charges), 2))

df_clean %>% group_by(exercise_level) %>%
  summarise(n = n(), medel = round(mean(charges), 2))

df_clean %>% group_by(age_group) %>%
  summarise(n = n(), medel = round(mean(charges), 2))

df_clean %>% group_by(bmi_category) %>%
  summarise(n = n(), medel = round(mean(charges), 2))

# Rökare betalar nästan dubbelt så mycket som icke-rökare.
# Kunder med kronisk sjukdom, lägre motionsnivå och högre ålder betalar mer.
