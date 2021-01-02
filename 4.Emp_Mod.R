# Script 4
## Empirisch Modelleren
# In deze hoofdstuk worden de analyse van hoofstuk 9.2 van het verslag uitgevoerd.

# Script Geschreven volgens de Tidyverse Style Guide
# Programeur: Marwan el Morabet; Contact: marwanelmorabet@gmail.com

# Kiezen van de juiste working directory
setwd("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Geobrain Eind/Data en Scripts")

# Runnen van de tweede script
source("3.CPT_Avg_Feat_Imp.R")

# Selecteren variabelen
Trilling_nls <- Trilling_def %>% select(R0031_VTOP, R0201_AFSTAND_MEETPUNT)
names(Trilling_nls) <- c("Vtop", "r")

# Stap 1: A = 5, n = 0.37
nls_mod <- nls(Vtop ~ A * (1/r)^n ,
               data = Trilling_nls,
               start = list(A = 10, n = 1), trace = T)

