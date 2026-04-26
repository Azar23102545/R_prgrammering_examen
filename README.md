# Försäkringskostnader – individuell inlämningsuppgift

Projektets syfte är att analysera vilka faktorer som hänger ihop med kundernas
försäkringskostnader (`charges`) och att bygga en regressionsmodell som kan
användas som stöd för prissättning.

## Paket som används

Analysen använder följande R-paket:

- `tidyverse` – datamanipulering (`dplyr`, `tidyr`, `stringr`) och
visualisering (`ggplot2`)
- `here` – hantering av filsökvägar på ett plattformsoberoende sätt
- `scales` – snyggare axlar i figurerna (används via `scales::comma`)

Installera paketen första gången du kör projektet:

```r
install.packages("tidyverse")
install.packages("here")
```

## Så här kör du analysen

1. Öppna mappen `insurance_analysis_Azar_Mohseni/` i RStudio
2. Kör kommandot nedan i terminalen:

```r
source("run_analysis.R")
```

Skriptet kör varje steg (01 → 02 → 03 → 04) i rätt ordning, skriver ut
tabeller och resultat i konsolen och sparar figurerna som PNG-filer i
projektets rotmapp.
