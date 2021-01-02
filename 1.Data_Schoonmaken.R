# Script 1

# Data Manipulatie
# In deze script wordt de trillingsdata, de sonderingen, en data over de sondering ingeladen. 
# Deze data wordt gecombineerd, gemanipuleerd en schoon gemaakt. 

#### prerequisits;
# Script Geschreven volgens de Tidyverse Style Guide
# Programeur: Marwan el Morabet; Contact: marwanelmorabet@gmail.com

#### De juiste working directory keuze
setwd("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Geobrain")

### Programmeer libraries inladen
library(dplyr)
library(readr)
library(data.table)
library(xlsx)

#### Inladen van de data ####

# Trillingsdata inladen
Constructie <- read.csv("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Data/Trillingdata3.csv", 
                        sep=";", stringsAsFactors=FALSE)

# Informatie over locatie van projectsondering inladen
Sondering_location <- read.csv("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Data/Fred/Admin_data.csv", stringsAsFactors=FALSE) %>%
  select(SONDNR, X_CRD, Y_CRD, MV_mm, MAXDEPTH_mm)

# Projectsondering inladen
Sondering <- read.csv("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Data/Fred/Output_data.csv", stringsAsFactors=FALSE)

# Extra DINO loket sonderingen inladen
Sondering <- read_delim("DINO.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  bind_rows(Sondering)

# Locatiegegevens van DINO lokeet sonderingen inladen
DINO_locatie <- read.csv2("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Geobrain/DINO_location.csv", stringsAsFactors=FALSE) %>%
  select(SONDNR, X_CRD, Y_CRD, MV_mm, MAXDEPTH_mm) %>%
  mutate(MV_mm = MV_mm * 1000, MAXDEPTH_mm = MAXDEPTH_mm * 1000)

# Datasets samenvoegen
Sondering_locatie <- rbind(Sondering_location, DINO_locatie) 

# Dimensies trillingsdata reduceren
Constructie_short <- Constructie %>%
  select(Frequentie_Trilblok,Slagkracht, R0104_LOKATIE_X, R0105_LOKATIE_Y, R0201_AFSTAND_MEETPUNT, R0201_AFSTAND_MEETPUNT,
         R0717_IS_OVERSCHRIJDINGEN, R0711_LI_SBR_CATEGORIE, R0713_LI_X_AMPLITUDE, R0718_LI_Y_AMPLITUDE, R0719_LI_Z_AMPLITUDE, 
         R0712_LI_WAARDE_VTOP, R0417_LI_TYPE_DAMWAND, R0515_DIEPTE_BEKEND, R0323_VALGEWICHT, R0108_PLAATSNAAM, R0412_LENGTE_PAAL_DAMWAND, R0413_PUNT_NIVEAU,R0031_VTOP,R0032_VD, R0031_VTOP,                     
         R0033_VKAR, R0034_VGRENS, R0035_VTOETSCALC)

#### Definitieve Code voor vinden dichstbijzijnde waarneming ####

# Functie voor het vinden van het dichtstbijzijnde sondering 
search_close <- function(xinput, yinput) {
  df <- Sondering_locatie %>% filter(MAXDEPTH_mm > 14000)
  df$distance <- sqrt((df$X_CRD - xinput)^2 + (df$Y_CRD - yinput)^2)
  return(df %>% filter(distance == min(df$distance)) %>% select(SONDNR)  )
}

output <- data.frame()

for(i in 1:154) {
  X_coord <- Constructie[i, 17]
  Y_coord <- Constructie[i, 18]
  output[i, "SONDNR"] <- search_close(xinput = X_coord, yinput = Y_coord)
}

# Toevoegen van de juiste sondering en het berekene van de afstand tussen damwand en sondering
Trilling_Join <- cbind(Constructie_short, output) %>%
  left_join(Sondering_locatie, by="SONDNR") %>%
  mutate(distance = sqrt((R0104_LOKATIE_X - X_CRD)^2 + (R0105_LOKATIE_Y - Y_CRD)^2) ) %>%
  filter(R0108_PLAATSNAAM == "'Lekkerkerk'")

# Het creeren van een dataset met damwandinfo met sonderingwaarden
Sondering <- Trilling_Join %>% right_join(Sondering, by = "SONDNR") %>%
  filter(!is.na(R0104_LOKATIE_X)) %>%
  select(SONDNR, R0412_LENGTE_PAAL_DAMWAND, R0413_PUNT_NIVEAU, MAXDEPTH_mm, DEPTH, CONE_RESISTANCE, FRICTION, FRICTION_FACTOR) %>%
  mutate(R0413_PUNT_NIVEAU = abs(R0413_PUNT_NIVEAU))

# kolomnamen aanpassen
names(Sondering) <- c("SONDNR", "LENGTE_DAMWAND", "PUNT_NIVEAU", "MAXDEPTH_CPT", "DEPTH", "CONE_RESISTANCE", "FRICTION", "FRICTION_FACTOR")

# Alleen lange sonderingen kiezen
Sondering <-Sondering %>% filter(DEPTH <= PUNT_NIVEAU) %>%
  mutate(DEPTH_NEG = (1/DEPTH)^2)


#### Manipuleren van sondering ####

# Het berekenen van CPT averaging waarden (max, min, gemiddelde, mediaan, etc..... van conusweerstand, wrijvingsgetal, schatwrijving)
Sondering <- Sondering %>% group_by(SONDNR) %>% 
  summarise(Eerste_meting = min(DEPTH),
            MAX_CONE_RESISTANCE = max(CONE_RESISTANCE),
            MIN_CONE_RESISTANCE = min(CONE_RESISTANCE),
            MEAN_CONE_RESISTANCE = mean(CONE_RESISTANCE),
            MED_CONE_RESISTANCE = median(CONE_RESISTANCE),
            SD_CONE_RESISTANCE = sd(CONE_RESISTANCE),
            FIRST_QUARTILE_CONE_RESISTANCE = summary(CONE_RESISTANCE)[2],
            THIRD_QUARTILE_CONE_RESISTANCE = summary(CONE_RESISTANCE)[5],
            SUM_CONE_RESISTANCE = sum(CONE_RESISTANCE),
            WEIGHTED_MEAN_CONE_RESISTANCE = weighted.mean(CONE_RESISTANCE, DEPTH_NEG),
            MAX_FRICTION = max(FRICTION),
            MIN_FRICTION = min(FRICTION),
            MEAN_FRICTION = mean(FRICTION),
            MED_FRICTION = median(FRICTION),
            SD_FRICTION = sd(FRICTION),
            FIRST_QUARTILE_FRICTION = summary(FRICTION)[2],
            THIRD_QUARTILE_FRICTION = summary(FRICTION)[5],
            SUM_FRICTION = sum(FRICTION),
            WEIGHTED_MEAN_FRICTION = weighted.mean(FRICTION, DEPTH_NEG),
            MAX_FRICTION_FACTOR = max(FRICTION_FACTOR),
            MIN_FRICTION_FACTOR = min(FRICTION_FACTOR),
            MEAN_FRICTION_FACTOR = mean(FRICTION_FACTOR),
            MED_FRICTION_FACTOR = median(FRICTION_FACTOR),
            SD_FRICTION_FACTOR = sd(FRICTION_FACTOR),
            FIRST_QUARTILE_FRICTION_FACTOR = summary(FRICTION_FACTOR)[2],
            THIRD_QUARTILE_FRICTION_FACTOR = summary(FRICTION_FACTOR)[5],
            SUM_FRICTION_FACTOR = sum(FRICTION_FACTOR),
            WEIGHTED_MEAN_FRICTION_FACTOR = weighted.mean(FRICTION_FACTOR, DEPTH_NEG)) 

# Combineren van dataset over trillingen met de berekende CPT averages
Trilling_def2 <- Trilling_Join %>% left_join(Sondering, by="SONDNR") %>% 
  select(Frequentie_Trilblok, Slagkracht, R0201_AFSTAND_MEETPUNT,R0713_LI_X_AMPLITUDE,  R0718_LI_Y_AMPLITUDE, R0104_LOKATIE_X, R0105_LOKATIE_Y, R0417_LI_TYPE_DAMWAND, R0711_LI_SBR_CATEGORIE, MAXDEPTH_mm, distance, Eerste_meting, R0717_IS_OVERSCHRIJDINGEN, SONDNR,
         MAX_CONE_RESISTANCE,MIN_CONE_RESISTANCE,MEAN_CONE_RESISTANCE,MED_CONE_RESISTANCE,SD_CONE_RESISTANCE,FIRST_QUARTILE_CONE_RESISTANCE,THIRD_QUARTILE_CONE_RESISTANCE,
         SUM_CONE_RESISTANCE,WEIGHTED_MEAN_CONE_RESISTANCE,MAX_FRICTION,MIN_FRICTION,MEAN_FRICTION,MED_FRICTION,SD_FRICTION,FIRST_QUARTILE_FRICTION,
         THIRD_QUARTILE_FRICTION,SUM_FRICTION,WEIGHTED_MEAN_FRICTION,MAX_FRICTION_FACTOR,MIN_FRICTION_FACTOR,MEAN_FRICTION_FACTOR,MED_FRICTION_FACTOR,SD_FRICTION_FACTOR,
         FIRST_QUARTILE_FRICTION_FACTOR, THIRD_QUARTILE_FRICTION_FACTOR, SUM_FRICTION_FACTOR,WEIGHTED_MEAN_FRICTION_FACTOR , R0719_LI_Z_AMPLITUDE,
         R0031_VTOP,R0032_VD, R0033_VKAR, R0034_VGRENS, R0035_VTOETSCALC,R0412_LENGTE_PAAL_DAMWAND)

# Overschrijdingen als een factor maken
Trilling_def2$R0717_IS_OVERSCHRIJDINGEN <- as.factor(Trilling_def2$R0717_IS_OVERSCHRIJDINGEN)

# Maatgevende bodemprofiel inladen
Bodemprofiel_Classificatie <- read_delim("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Data/Bodemprofiel Classificatie.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

# Combineren met de data
Trilling_def  <- Trilling_def2 %>% left_join(Bodemprofiel_Classificatie, by = "SONDNR") %>%
  filter(R0711_LI_SBR_CATEGORIE == 3)

# damwandgegevens inladen
damwand_info <- data.frame(R0417_LI_TYPE_DAMWAND = c(0, 116, 119, 120, 124, 127, 130, 399), 
                           Naam_Damwand =c(NA, "AZ 13", "AZ 17", "AZ 18", "AZ 26", "AZ 36", "AZ 48", "AZ 36 - 700"), 
                           As_enkel = c(NA, 91.8, 86.9, 94.8, 124.6, 155.6, 177.8, 151.1),
                           As_dubbel = c(NA, 183.6, 173.9, 189.6, 249.2, 311.2, 355.6, 302.2))

# Definitieve dataset voor het doen van analyses 
Trilling_def <- Trilling_def %>% left_join(damwand_info, by="R0417_LI_TYPE_DAMWAND")

# Verwijderen van niet invloedrijke variabelen
remove(Bodemprofiel_Classificatie, Constructie, Constructie_short, DINO_locatie, output, Sondering, Sondering_locatie, Sondering_location, Trilling_def2, 
       i, X_coord, Y_coord, damwand_info, Trilling_Join, search_close)
