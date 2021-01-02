# Script 3
## CPT averaging Techniques and Feature Importance
# In deze hoofdstuk worden de analyse van hoofstuk 7 en 8 van het verslag uitgevoerd.

# Script Geschreven volgens de Tidyverse Style Guide
# Programeur: Marwan el Morabet; Contact: marwanelmorabet@gmail.com

# Kiezen van de juiste working directory
setwd("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Geobrain/Scripts/Def")

#Inladen libraries
library(Boruta)


# Laden van prerequisits
# Runnen van de tweede script
source("2.CUR166.R")

# Correlatie Vtop vs. Conusweerstand (Figuur 24)
p.mat <- cor.mtest(Trilling_numson[, c(29,2:10)])
m <- cor(Trilling_numson[, c(29,2:10)], method = "spearman")
colnames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
rownames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.05, tl.srt = 10)

# Correlatie Vtop vs. Schachtwrijving (Figuur 25)
p.mat <- cor.mtest(Trilling_numson[ , c(29, 11:19)])
m <- cor(Trilling_numson[ , c(29, 11:19)], method = "spearman")
colnames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
rownames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.05, tl.srt = 10)

# Correlatie Vtop vs. Wrijvingsgetal (Figuur 26)
p.mat <- cor.mtest(Trilling_numson[, c(29, 20:28)])
m <- cor(Trilling_numson[, c(29, 20:28)], method = "spearman")
colnames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
rownames(m) <- c("Trilsnelheid", "Maximum", "Minimum", "Gemiddelde", "Mediaan", "Standaard Deviatie", "Eerste Kwartiel", "Derde Kwartiel", "Som", "Gewogen Gemiddelde")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.05, tl.srt = 10)


# Dataset met belangrijke parameters
Trilling_systeem <- Trilling_def %>% mutate(rinverse = 1/R0201_AFSTAND_MEETPUNT, rinverse_root = sqrt(1/R0201_AFSTAND_MEETPUNT)) %>%
  select(R0031_VTOP, R0201_AFSTAND_MEETPUNT, rinverse, rinverse_root,  Bodemprofiel,  distance, As_dubbel, Frequentie_Trilblok, Slagkracht, R0412_LENGTE_PAAL_DAMWAND) %>%
  mutate(IsRotterdam = Bodemprofiel == "Rotterdam")

# Laatste systeemparameters (Figuur 30)
p.mat <- cor.mtest(Trilling_systeem[, c(1,2,3,4,9,10)])
m <- cor(Trilling_systeem[, c(1,2,3,4,9,10)], method = "pearson", use = "pairwise.complete.obs")
colnames(m) <- c("VTOP", "r", "1/r", "1/sqrt(r)", "Lengte Damwand", "Afstand Sondering")
rownames(m) <- c("VTOP", "r", "1/r", "1/sqrt(r)",  "Lengte Damwand", "Afstand Sondering")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.1, tl.srt = 10, tl.cex = 0.9)


# Numerieke variabelen transformeren naar categorische variabelen
Trilling_systeem$Bodemprofiel <- as.factor(Trilling_systeem$Bodemprofiel)

# Kolomnamen veranderen
names(Trilling_systeem) <- c("VTop", "R", "1/R", "1/root(R)", "Bodemprofiel", "Afstand Sondering", "As Damwand", "Frequentie", "Slagkracht", "L_damwand", "Rotterdam")

# Boruta feature importance
boruta_output <- Boruta(VTop ~ ., data=na.omit(Trilling_systeem))  

# Plotten van boruta plot (Figuur 31)
plot(boruta_output, cex.axis=0.5,pos = 1, las=1, xlab="",ylab = "", main="Variable Importance", horizontal =T,
     whichShadow = c(F,F,F))  

# Boxplots (Figuur 27, 28 en 29)
stat_box_data <- function(y, upper_limit = max(Trilling_def$R0031_VTOP) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Aantal =', length(y), '\n',
                    'Mean =', round(mean(y), 1), '\n')
    )
  )
}

Trilling_def %>% ggplot(aes(y=R0031_VTOP, x=as.factor(Bodemprofiel))) + geom_boxplot() +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  theme_fivethirtyeight()

Trilling_def %>% ggplot(aes(y=R0031_VTOP, x=as.factor(Slagkracht))) + geom_boxplot() +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  theme_fivethirtyeight()

Trilling_def %>% ggplot(aes(y=R0031_VTOP, x=as.factor(Frequentie_Trilblok))) + geom_boxplot() +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  theme_fivethirtyeight()

Trilling_def %>% ggplot(aes(y=R0031_VTOP, x=as.factor(As_dubbel))) + geom_boxplot() + xlab("Oppervlakte") + ylab("Trilsnelheid") +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size = 3
  ) + 
  theme_fivethirtyeight() 



Trilling_save <- Trilling_systeem[, c(1:5, 10, 11)] %>%
  write.csv("Regressie.csv")

Trilling_systeem <- Trilling_def %>% mutate(rinverse = 1/R0201_AFSTAND_MEETPUNT, rinverse_root = sqrt(1/R0201_AFSTAND_MEETPUNT)) %>%
  select(R0031_VTOP, R0201_AFSTAND_MEETPUNT, rinverse, rinverse_root,  Bodemprofiel,  distance, As_dubbel, Frequentie_Trilblok, Slagkracht, R0412_LENGTE_PAAL_DAMWAND, Overschrijding) %>%
  mutate(IsRotterdam = Bodemprofiel == "Rotterdam") %>%
  write.csv("Classificatie.csv")
  
