# BESKRIVANDE ANALYS
# Syfte: Få en inledande förståelse för datat genom statistiska sammanfattningar,tabeller och visualiseringar. Varje figur/tabell följs av en tolkning.

# Förutsätter att df_clean har skapats i 02_datastadning_och_forberedelse.R.

library(tidyverse)
library(here)


# TABELL 1: Statistisk sammanfattning av charges
cat("\n=== TABELL 1: Sammanfattning av försäkringskostnader (charges) ===\n")
charges_summary <- df_clean %>%
  summarise(
    n      = n(),
    medel  = round(mean(charges), 2),
    median = round(median(charges), 2),
    sd     = round(sd(charges), 2),
    min    = round(min(charges), 2),
    max    = round(max(charges), 2)
  )
print(charges_summary)

# Tolkning: Kostnaderna varierar kraftigt, från ca 1 200 kr upp till ca 32 600 kr. Medelvärdet (ca 10 060 kr) är högre än medianen (ca 9 124 kr), vilket tyder 
# på en högerskev fördelning med några kunder som har mycket höga kostnader.


# FIGUR 1: Fördelning av försäkringskostnader
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

print(fig1)
ggsave(here("fig1_fordelning_charges.png"),
       fig1, width = 8, height = 5, dpi = 150)

# Tolkning: ördelningen är högerskev. De flesta kunder har kostnader mellan 5 000 och 12 000 kr, men en mindre grupp har betydligt högre kostnader. Det stämmer
# med att vissa riskfaktorer (t.ex. rökning) driver upp priset rejält.


# FIGUR 2: Kostnad för rökare vs icke-rökare
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

print(fig2)
ggsave(here("fig2_rokare_vs_icke.png"),
       fig2, width = 8, height = 5, dpi = 150)

# Tolkning: Rökare har en tydligt högre medelkostnad än icke-rökare. Spridningen är också större för rökare. Detta antyder att rökning är en mycket stark prediktor för försäkringskostnad.


# FIGUR 3: Ålder vs kostnad med rökning som färg
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

print(fig3)
ggsave(here("fig3_alder_charges.png"),
       fig3, width = 8, height = 5, dpi = 150)

# Tolkning: Kostnaden ökar med åldern för båda grupperna, men rökare ligger konsekvent på en högre nivå. Lutningen är liknande, vilket tyder på att ålder ger samma ökning för båda grupperna, men rökning adderar en tydlig premie.


# FIGUR 4: Medelkostnad per försäkringsplan
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

print(fig4)
ggsave(here("fig4_kostnad_per_plan.png"),
       fig4, width = 8, height = 5, dpi = 150)

# Tolkning: Premium har högst medelkostnad, följt av standard och sist basic. Det är förväntat, men skillnaden mellan basic och standard är mindre än mellan standard och premium.


# TABELL 2: Medelkostnad per nyckelgrupp
cat("\n=== TABELL 2: Medelkostnad per nyckelgrupp ===\n")

cat("\n--- Per rökare ---\n")
print(df_clean %>%
        group_by(smoker) %>%
        summarise(n = n(), medel = round(mean(charges), 2)))

cat("\n--- Per kronisk sjukdom ---\n")
print(df_clean %>%
        group_by(chronic_condition) %>%
        summarise(n = n(), medel = round(mean(charges), 2)))

cat("\n--- Per motionsnivå ---\n")
print(df_clean %>%
        group_by(exercise_level) %>%
        summarise(n = n(), medel = round(mean(charges), 2)))

cat("\n--- Per åldersgrupp ---\n")
print(df_clean %>%
        group_by(age_group) %>%
        summarise(n = n(), medel = round(mean(charges), 2)))

cat("\n--- Per BMI-kategori ---\n")
print(df_clean %>%
        group_by(bmi_category) %>%
        summarise(n = n(), medel = round(mean(charges), 2)))

# Tolkning: Rökare betalar nästan dubbelt så mycket som icke-rökare i snitt. Kunder med kronisk sjukdom har också markant högre kostnader.
# Motionsnivå och åldersgrupp visar tydliga skillnader: de som motionerar mer och är yngre betalar mindre i snitt.