# Script 2
# CUR 166 analyse

# In deze script wordt de CUR 16 analyse die te zien is in hoofdstuk 4 uitgevoerd
# Script Geschreven volgens de Tidyverse Style Guide
# Programeur: Marwan el Morabet; Contact: marwanelmorabet@gmail.com

# Kiezen van de juiste working directory
setwd("C:/Users/morabet/OneDrive - Stichting Deltares/Desktop/Geobrain Eind/Data en Scripts")

# Laden van prerequisits
# Runnen van de eerste script
source("1.Data_Schoonmaken.R")

# Inladen van programming libraries
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)
 library(corrplot)

# CUR 166 damwand trillingsprognose in een functie
CUR166_Damwand_Trillingsprognose <- function(Bodemprofiel = "Amsterdam", straal = 8, slagkracht = 900, percentage = 95,  statement = FALSE) {
  
  # Bodemparameters volgens de CUR 
  Bodemparameters <- data.frame(Bodemprofiel = c("Amsterdam", "Eindhoven", "Groningen", "Den Haag", "Rotterdam", "Tiel", "x"),
                                V50 = c(1.6, 1.9, 1.7, 1.9, 1.6, 1.6, 1.6),
                                C50 = c(0.002 ,0.002 ,0.002 ,0.002 ,0.002 ,0.002, 0.002),
                                V95 = c(9, 6.7, 13.4, 6.7, 9, 9, 9),
                                C95 = c(0.011, 0.007, 0.016, 0.007, 0.011, 0.011, 0.011),
                                V99 = c(18.3, 11.3, 31.6, 11.3, 18.3, 18.3, 18.3),
                                C99 = c(0.023, 0.012, 0.037, 0.012, 0.023, 0.023, 0.023))
  
  # Het kiezen van de juiste overschreidingskans
  Rij <- which(Bodemparameters$Bodemprofiel == Bodemprofiel)  
  
  if(percentage == 50) {
    kolomv <- 2
    kolomc <- 3
  } else if(percentage == 95) {
    kolomv <- 4
    kolomc <- 5
  } else if(percentage == 99) {
    kolomv <- 6
    kolomc <- 7
  } else {
    return(print("Percentage should be either 50, 95 or 99 %"))
    # Verbreek de functie
  }
  
  # Selecteren van de juiste parameters voor de berekening
  v0 <- Bodemparameters[Rij, kolomv]
  cvel <- Bodemparameters[Rij, kolomc]
  
  #vcor <- 2.5*v0 #+ cvel*(0.5 *slagkracht - 50)
  vcor <- v0 + cvel*(slagkracht - 350)    
  
  # Het berekenen van de topsnelheid horende bij de ingevulde overschrijdingsgrens.
  vmax <<- round(vcor * 0.7 * sqrt(5/straal), digits = 2)
  
  # Indien gewenst een print statement die de output verduidelijkt
  if(statement == T) {
    print(paste("De",percentage,   "% bovengrens van de trilsnelheid staat gelijk aan", vmax, "mm/s"))  
  } 
  
  # De Vmax aanwijzen als output van de functie
  return(vmax)
}

# CUR 166 trillingsprognose over 150 cases
Trilling_def <- Trilling_def %>%
  rowwise() %>% 
  mutate(Vcur = CUR166_Damwand_Trillingsprognose(Bodemprofiel = Bodemprofiel, straal = R0201_AFSTAND_MEETPUNT, slagkracht = Slagkracht, percentage = 50)) %>%
  mutate(Verschil = Vcur - R0031_VTOP, Abs_error = abs(Verschil)) %>%
  mutate(Ispositive = Verschil >= 0, Issmall = R0412_LENGTE_PAAL_DAMWAND <= 14, fout_factor = R0031_VTOP / Vcur) %>%
  mutate(Big_error = fout_factor >= 3)  %>%
  as.data.frame()

# Frequentie en Slagkracht omzetten naar categorische variabelen
# Trilling_def$Frequentie_Trilblok <- as.factor(Trilling_def$Frequentie_Trilblok)
# Trilling_def$Slagkracht <- as.factor(Trilling_def$Slagkracht)

# Opslitsen van dataset in twee datasets naar lengte damwand
korte_damwand <- filter(Trilling_def, Issmall == TRUE) %>% arrange(fout_factor)
lange_damwand <- filter(Trilling_def, Issmall == FALSE) %>% arrange(fout_factor)

#### Maken van statistische berkeningen ####
# Berekenen van spearman ranked correlatie over datasets
CUR_COR <- cor(Trilling_def$Vcur, Trilling_def$R0031_VTOP, method = "spearman")
CUR_COR <- cor(Trilling_def$Vcur, Trilling_def$R0031_VTOP)
CUR_COR14 <- cor(korte_damwand$R0031_VTOP, korte_damwand$Vcur, method = "spearman")
CUR_COR15 <- cor(lange_damwand$R0031_VTOP, lange_damwand$Vcur, method = "spearman")

# Mean average error berekenen
MAE <- mean(abs(Trilling_def$Verschil))
MAE_kort <- mean(abs(korte_damwand$Verschil))
MAE_lang <- mean(abs(lange_damwand$Verschil))

# Standaard Deviatie van error van model
sd_tril <- sd(Trilling_def$Verschil)
sd_kort <- sd(korte_damwand$Verschil)
sd_lang <- sd(lange_damwand$Verschil)

# Root mean squared error bereken van model
RMSE_tril <- sqrt(mean(Trilling_def$Verschil^2))
RMSE_kort <- sqrt(mean(korte_damwand$Verschil^2))
RMSE_lang <- sqrt(mean(lange_damwand$Verschil^2))

# Visualiseren van nauwkeurigheid CUR 166 zonder 14 meter onderscheiding (figuur 11)
ggplot(Trilling_def, aes(x=Vcur, y=R0031_VTOP)) + geom_point(size =2, shape = 3) + 
  geom_abline(slope = 1, color = "red", size = 1.3) + xlab("Trilsnelheid Volgens het CUR 166 model") + ylab("Gemeten top snelheid") + 
  theme_light() +  xlim(0, 11) + ylim(0, 12) +   theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")) ; ggplotly()

# Verdeling van residuen (Figuur 12)
ggplot(Trilling_def, aes(x=Verschil, fill = Ispositive)) +
  geom_histogram(bins = 40, color = "black")+ theme_minimal() +
  xlab("CUR166 - Meting") + ylab("Aantal Waarnemingen") + labs(fill = "") +
  theme(legend.position = c(0.3, 0.58)) +
  scale_fill_discrete(labels=c("Meting hoger dan prognose", "Prognose hoger dan meting"))

# Nul Model Residuen (Figuur 13)
verschil_nulmodel <- data.frame(a =  mean(Trilling_def$R0031_VTOP ) - Trilling_def$R0031_VTOP) %>%
  mutate(Ispositive = a >= 0)
MAE_nul  <- mean(abs(verschil_nulmodel$a))

ggplot(verschil_nulmodel, aes(x=a, fill = Ispositive)) +
  geom_histogram(bins = 35, color = "black")+ theme_minimal()  +
  xlab("Error Null Model") + ylab("Aantal Waarnemingen") + labs(fill = "") +
  theme(legend.position = c(0.25, 0.58)) +
  scale_fill_discrete(labels=c("Meting hoger dan prognose", "Prognose hoger dan meting"))

# Verschil groter of kleiner dan 14 meter (Figuur 15)
ggplot(Trilling_def, aes(x=Vcur, y=R0031_VTOP, color = Issmall)) + geom_point(size =2, shape = 3) + 
   xlab("Trilsnelheid Volgens de CUR 166 model") + ylab("Gemeten top snelheid") +
  geom_abline(slope = 1, color = "deeppink3", size = 1.3)  + geom_smooth(method = "lm", se = F) +
  theme_light() +  xlim(0, 11) + ylim(0, 11) +
  scale_color_discrete(labels=c("Damwand langer dan 14 m.", "Damwand korter dan 14 m.")) + theme(
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"), 
    legend.position = c(0.72, 0.15),
    legend.direction = "vertical", legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")) + annotate("text", x=6.5, y=9.5, label = paste0("R^2 = ", round(CUR_COR^2, digits = 3), "0"), size = 6, color = "brown1") +
  annotate("text", x=6.5, y=8.5, label = paste0("R^2 = ", round(CUR_COR14^2, digits = 3)), size = 6, color = "cyan")

# Lengte Damwand vs. Error (Figuur 16)
ggplot(Trilling_def, aes(x=R0412_LENGTE_PAAL_DAMWAND, y=Abs_error)) + geom_point() +
  xlab("Lengte Damwand [m]") + ylab("Absolute Error Model [mm/s]") + theme_light() + geom_smooth(method = "lm", se=F) +
  theme(
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"), 
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

# Legnte Damwand vs. Vtop (figgur 17)
ggplot(Trilling_def, aes(x=R0412_LENGTE_PAAL_DAMWAND, y=R0031_VTOP)) + geom_point() +
  xlab("Lengte Damwand [m]") + ylab("V top [mm/s]") + theme_light() + geom_smooth(method = "lm", se =F) +
  theme(
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

# Simpele Boxplot grafieken
ggplot(Trilling_def, aes(y=Verschil, x= Frequentie_Trilblok)) +
  geom_boxplot() + coord_flip()

ggplot(Trilling_def, aes(y=Verschil, x= Slagkracht)) +
  geom_boxplot() + coord_flip()

# Error Correlatie Analyse

# Creeeren van een functie voor toevoegen voor statistieken aan grafiek
stat_box_data <- function(y, upper_limit = max(Trilling_def$Abs_error) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Aantal =', length(y), '\n',
                    'Gemiddelde =', round(mean(y), 1), '\n')
    )
  )
}
# Invloed op bodemprofiel op error model
Bodem_freq <- Trilling_def %>%
  group_by(Bodemprofiel) %>% summarise(Aantal = n(), mean_error = mean(Abs_error), mean_vibr = mean(R0031_VTOP)) %>%
  filter(Aantal > 8) 

# (Afbeelding 18)
Trilling_def %>%
  filter(Bodemprofiel %in% Bodem_freq$Bodemprofiel) %>%
  ggplot(aes(x=Bodemprofiel, y= Abs_error)) +
  geom_boxplot() + xlab("Bodemprofiel ") + ylab("Absolute fout model") +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  theme_classic()


# Invloed op type damwandtype op error model
Damwand_freq <- Trilling_def %>%
  group_by(Naam_Damwand) %>% summarise(Aantal = n(), mean_error = mean(Abs_error), mean_vibr = mean(R0031_VTOP)) %>%
  filter(Aantal > 10)

# (Afbeelding 19)
Trilling_def %>%
  filter(Naam_Damwand %in% Damwand_freq$Naam_Damwand) %>%
  ggplot(aes(x=Naam_Damwand, y= Abs_error)) +
  geom_boxplot() + xlab("Damwand ") + ylab("Absolute fout model") +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  theme_classic()

### Correlatieplotten

# Selecteren van de juiste variabelen
Trilling_num <- Trilling_def %>% select(R0031_VTOP, Abs_error, R0412_LENGTE_PAAL_DAMWAND, R0201_AFSTAND_MEETPUNT, distance, Slagkracht, Frequentie_Trilblok, fout_factor)
# Veranderen van kolomnamen
names(Trilling_num) <- c("Trilsnelheid", "Absolute Error", "Lengte Damwand", "Afstand Woning",  "Afstand Sondering","Slagkracht", "Frequentie",  "Fout Factor" )

# Dataset over sondering
Trilling_numson <- Trilling_def[ ,c(55, 15:41,43 )] 

# Correlatie test functie maken
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j],method = "spearman",  ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Correlatiematrix plotten

# Error CUR met Systeemparameters (Figuur 20)
p.mat <- cor.mtest(Trilling_num[2:7])
m <- cor(Trilling_num[2:7], method = "kendall")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.05, tl.srt = 10, tl.cex = 0.6)


# Error CUR met Sondering (Figuur 21)
p.mat <- cor.mtest(Trilling_numson)
m <- cor(Trilling_numson, method = "spearman")
corrplot(m, type = "lower", p.mat = p.mat, sig.level = 0.1, tl.srt = 10, tl.cex = 0.6)


# 95% overschrijdingskans 
Trilling_95 <- Trilling_def %>%
  rowwise() %>%
  mutate(Vcur = CUR166_Damwand_Trillingsprognose(Bodemprofiel = Bodemprofiel, straal = R0201_AFSTAND_MEETPUNT, slagkracht = Slagkracht, percentage = 95)) %>%
  mutate(Verschil = Vcur - R0031_VTOP, Abs_error = abs(Verschil)) %>%
  mutate(Ispositive = Verschil >= 0, Issmall = R0412_LENGTE_PAAL_DAMWAND <= 14, fout_factor = R0031_VTOP / Vcur) %>% filter(fout_factor >0)

# Visualiseren van nauwkeurigheid CUR 166 bij 5% overschrijdingskans (Figuur 22)
ggplot() +
  geom_point(aes(x=Trilling_95$Vcur, y=Trilling_95$R0031_VTOP), size = 2, shape = 3) + xlab("95%-waarde") + 
  geom_abline(slope = 1, color = "red", size = 1.3)  + ylab("Gemeten topsnelheid") +
  geom_abline(slope = 1, intercept = -6.2, color = "blue", size = 1.3, linetype = "dashed") +
  theme_light() +  xlim(0, 29) + ylim(0, 11) +   theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

# 0.5 * 95%-Vcur plaatje (Figuur 23)
ggplot() +
  geom_point(aes(x=0.50*Trilling_95$Vcur, y=Trilling_95$R0031_VTOP), size = 2, shape = 3) + xlab("0.5 * Vcur") + ylab("Gemeten top snelheid") + 
  geom_abline(slope = 1, color = "red", size = 1.3)  + ylab("Gemeten top snelheid") +
  theme_light() +  xlim(0, 29) + ylim(0, 11) +   theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

# CUR 50 categorische evaluatie
Trilling_def$Grens <- Trilling_def$Frequentie_Trilblok * 0.1 / 60 + 1
Trilling_def$Overschrijding <- Trilling_def$Grens < Trilling_def$R0031_VTOP
Trilling_def$CUR_Overschrijding <- Trilling_def$Grens < Trilling_def$Vcur
Trilling_def$Overschrijding2 <- ifelse(Trilling_def$CUR_Overschrijding, "Positive", "Negative")
Trilling_def$Waarheid <- ifelse(Trilling_def$Overschrijding == Trilling_def$CUR_Overschrijding, "True", "False")
Trilling_def$Waarheid <- paste(Trilling_def$Waarheid, Trilling_def$Overschrijding2)
Trilling_def %>% group_by(Waarheid) %>% summarise(aantal = n())

# CUR 95 categorische evaluatie
Trilling_95$Grens <- Trilling_95$Frequentie_Trilblok * 0.1 / 60 + 1
Trilling_95$Overschrijding <- Trilling_95$Grens < Trilling_95$R0031_VTOP
Trilling_95$CUR_Overschrijding <- Trilling_95$Grens < Trilling_95$Vcur
Trilling_95$Overschrijding2 <- ifelse(Trilling_95$CUR_Overschrijding, "Positive", "Negative")
Trilling_95$Waarheid <- ifelse(Trilling_95$Overschrijding == Trilling_95$CUR_Overschrijding, "True", "False")
Trilling_95$Waarheid <- paste(Trilling_95$Waarheid, Trilling_95$Overschrijding2)
Trilling_95 %>% group_by(Waarheid) %>% summarise(aantal = n())


