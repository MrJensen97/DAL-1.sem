#### OLA 1, Opgave 1.1 ####
#Find de to boliger fra boligsiden

boligsiden <- read.csv("boligsiden.csv")
str(boligsiden)
structure(boligsiden)

# Filtrer data for både Tousvej og Egevej med de angivne kriterier
kombineret_bolig <- subset(boligsiden, vej == "tousvej" | (vej == "egevej" & kvmpris == "24.922"))

# Vis det kombinerede dataframe
View(kombineret_bolig)

Boligsiden_dropNA <- na.omit(boligsiden)


#### OLA 1, Opgave 1.2 ####
#Find to tilfældige bolig
# Udvælg to tilfældige ejendomme
to_ejendomme <- Boligsiden_dropNA[sample(1:nrow(Boligsiden_dropNA), 2), ]

# Se de udvalgte ejendomme
print(to_ejendomme)


#### OLA 1, Opgave 1.3 ####
# Antal NA værdier i boligsiden.csv
sum(is.na(boligsiden))
#738

# Viser antal NA i hver kolonne i boligsiden.csv
colSums(is.na(boligsiden))

# Viser hvilke rækker der har NA værdier
which(rowSums(is.na(boligsiden)) > 0)


#### OLA 1, Opgave 1.4 ####
#Spørgsmålet: Med udgangspunkt i Data Science Modellen skal I gøre rede for de skridt, der er blevet taget for at nå frem til csv-filen.
#I skal komme med et bud på et “research goal” som kunne have optimeret processen med at fremskaffe data fra boligsiden.

###################################################################################################
library(ggplot2)
library(scales)
library(corrplot)
library(tidyr)

#### OLA 1, Opgave 2.1 #### Beskrivende statistik
boligsiden <- drop_na(boligsiden)
boligsiden_stat <- boligsiden[, c(1, 6, 7, 8, 9)]
summary(boligsiden_stat)

#### OLA 1, Opgave 2.2 ####
kolonner <- c("pris", "kvmpris", "grund", "mdudg")
boligsiden[kolonner] <- lapply(boligsiden[kolonner], function(x) as.numeric(gsub("[^0-9]", "", x)))

# Fjern unødvendige kolonner
boligsidenkp <- boligsiden[, c(1, 6)]

# Nyt dataframe til cor
boligsidencor <- boligsiden[, c(1, 6, 7, 8, 9, 12)]

boligsidencor_filtreret <- boligsidencor[
  boligsidencor$mdudg >= 1000 &
    boligsidencor$kvmpris >= 5000 &
    boligsidencor$grund >= 250 &
    boligsidencor$størrelse <= 500 &
    boligsidencor$pris < 45000000,
]

# Udskriv det nye dataframe for at kontrollere ændringerne
summary(boligsidencor_filtreret)

# Plot Størrelse / m2 og Pris i kr.
ggplot(boligsidencor_filtreret, aes(x = pris, y = størrelse)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Flest huse under 300 kvm og under 10.000.000 kr.",
       x = "Pris i kr.",
       y = "Størrelse / m2") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

korrelation_kmpris <- cor(boligsiden$pris, boligsiden$størrelse)
print(korrelation_kmpris)
# Korrelationen er 0.2864522



#### OLA 1, Opgave 2.3 ####
# 1 Kvmpris og pris
ggplot(boligsidencor_filtreret, aes(x = pris, y = kvmpris)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Korrelation mellem pris og kvmpris",
       x = "Pris i mio. kr.",
       y = "Kvmpris i tusinde kr.") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))


korrelation1 <- cor(boligsidencor_filtreret$pris, boligsidencor_filtreret$kvmpris)
print(korrelation1)
# Korrelationen er 0.6513573

## 2 Kvmpris og grund ##
ggplot(boligsidencor_filtreret, aes(x = grund, y = kvmpris)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Korrelation mellem kvmpris og grund",
       x = "grund str. i kvm",
       y = "Kvmpris i tusinde kr.") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

korrelation2 <- cor(boligsidencor_filtreret$grund, boligsidencor_filtreret$kvmpris)
print(korrelation2)
# Korrelationen er -0.05406615

### 3 Kvmpris og opført ###
ggplot(boligsidencor_filtreret, aes(x = opført, y = kvmpris)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Korrelation mellem kvmpris og opført",
       x = "Opført i årstal",
       y = "Kvmpris i tusinde kr.") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

korrelation3 <- cor(boligsidencor_filtreret$opført, boligsidencor_filtreret$kvmpris)
print(korrelation3)
# Korrelationen er 0.03220422

#### 4 Kvmpris og mdudg ####
ggplot(boligsidencor_filtreret, aes(x = mdudg, y = kvmpris)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Korrelation mellem kvmpris og månedlig udgift",
       x = "Månedlig udgift i tusinde kr.",
       y = "Kvmpris i tusinde kr.") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

korrelation4 <- cor(boligsidencor_filtreret$mdudg, boligsidencor_filtreret$kvmpris)
print(korrelation4)
# Korrelationen er 0.7681329


##### 5 Kvmpris og størrelse #####
ggplot(boligsidencor_filtreret, aes(x = størrelse, y = kvmpris)) +
  geom_point() +               # Tilføjer datapunkter
  labs(title = "Korrelation mellem kvmpris og postnr",
       x = "Postnr",
       y = "Kvmpris i tusinde kr.") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

korrelation5 <- cor(boligsidencor_filtreret$størrelse, boligsidencor_filtreret$kvmpris)
print(korrelation5)
# Korrelationen er 0.08274224


# Korrelationsmatrix fordelt på 5 variabler
cor_matrix <- cor(boligsidencor_filtreret)
corrplot(cor_matrix, method = "circle", type = "upper", 
         addCoef.col = "black",    # Tilføjer korrelationstal i sort
         tl.col = "red",           # Farve til etiketter (variabelnavne)
         tl.srt = 45,              # Roterer etiketter for bedre læsbarhed
         diag = FALSE)

#### OLA 1, Opgave 2.4 ####
# Besvarelse i PDF #


#### OLA 1, Opgave 3.1 ####

die <- 1:6

dice_throw <- function(die=1:6){
  dice_throw <- sample(die,size=1,replace=TRUE)
  sum(dice_throw)
}

roll25000 <- replicate(25000,dice_throw())

qplot(roll25000,binwidth=0.5)

sum(roll25000==5)

#### OLA 1, Opgave 3.2 ####
dice_throw6 <- function(die=1:6){
  dice_throw6 <- sample(die,size = 6,replace=TRUE)
  sum(dice_throw6)
}

roll10000 <- replicate(10000,dice_throw6())

qplot(roll10000, binwidth = 0.5)

#### OLA 1, Opgave 3.3 ####
dice_throw6 <- function(die=1:6){
  dice_throw6 <- sample(die,size = 6,replace=TRUE)
  sum(dice_throw6)
}

roll1000000 <- replicate(1000000,dice_throw6())

qplot(roll1000000,binwidth=0.5)

#### OLA 1, Opgave 3.4 ####
talsæt <- data.frame(1,2,3,5,6)
tilfældig <- sample(talsæt)

matrix <- cbind(2:6,c(tilfældig))


#### OLA 1, Opgave 4.1 ####
library(dkstat)
library(devtools)
library(tidyr)
library(ggplot2)
library(corrplot)

dst <- dst_get_tables(lang = "da")

alkohol <- dst_meta(table = "FU02", lang = "da")
alkohol$values$KONSUMGRP

print(alkohol$variables)

Forbrug <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = "Faste priser",
  Tid = "*"
)

Alkohol_Forbrug <- dst_get_data(table = "FU02",query = Forbrug,lang = "da")

#Fjerner måneder og dato fra kolonnen TID
Alkohol_Forbrug$TID <- as.numeric(sub("-.*", "", Alkohol_Forbrug$TID)) 

#Fjerner kategori nummer fra vores kolonne navne
Alkohol_Forbrug$KONSUMGRP <- sub("^[^ ]* ", "", Alkohol_Forbrug$KONSUMGRP)

ggplot(Alkohol_Forbrug, aes(x = TID, y = value, color = KONSUMGRP, group = KONSUMGRP)) +
  geom_line(size = 0.5) + 
  theme_minimal() +      
  theme(
    legend.position = "bottom",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) +
  labs(
    x = "År",
    y = "Forbrug (kr. pr. husstand)",
    color = "Type af alkohol",
    title = "Alkoholforbrug pr. Kategori"
  )


Alkohol_Bredt <- pivot_wider(Alkohol_Forbrug, 
                             names_from = KONSUMGRP, 
                             values_from = value, 
                             id_cols = TID)

Alkohol_Bredt <- as.data.frame(Alkohol_Bredt)

rownames(Alkohol_Bredt) <- Alkohol_Bredt$TID
# Eksempel med dine kolonnenavne
colnames(Alkohol_Bredt) <- gsub("^[^ ]+ ", "", colnames(Alkohol_Bredt))


ggplot(Alkohol_Bredt, aes(x = TID)) +  
  geom_line(aes(y = `Spiritus og likør`, color = 'Spiritus & likør')) +  
  geom_line(aes(y = `Vin af druer`, color = 'Vin af druer')) + 
  geom_line(aes(y = `Vin af andre frugter`, color = 'Vin af andre frugter')) + 
  geom_line(aes(y = `Hedvin`, color = 'Hedvin')) + 
  geom_line(aes(y = `Pilsnerøl, guldøl`, color = 'Pilsnerøl, guldøl')) + 
  geom_line(aes(y = `Andre alkoholholdige øl`, color = 'Andre alkoholholdige øl')) + 
  labs(title = "Husstandenes gennemsnitlige forbrug per år", 
       x = "År", 
       y = "Pris (faste priser)",
       color = "Forbrug (kr. pr. husstand)") +
  theme_minimal()


#Normaliserer værdierne
Alkohol_Normal <- Alkohol_Bredt
Alkohol_Normal[, 2:11] <- lapply(Alkohol_Bredt[, 2:11], function(x) {
  (x - min(x)) / (max(x) - min(x))
})

ggplot(Alkohol_Normal, aes(x = TID)) +  
  geom_line(aes(y = `Spiritus og likør`, color = 'Spiritus & likør')) +  
  geom_line(aes(y = `Vin af druer`, color = 'Vin af druer')) + 
  geom_line(aes(y = `Vin af andre frugter`, color = 'Vin af andre frugter')) + 
  geom_line(aes(y = `Hedvin`, color = 'Hedvin')) + 
  geom_line(aes(y = `Pilsnerøl, guldøl`, color = 'Pilsnerøl, guldøl')) + 
  geom_line(aes(y = `Andre alkoholholdige øl`, color = 'Andre alkoholholdige øl')) + 
  labs(title = "Husstandenes gennemsnitlige forbrug per år", 
       x = "År", 
       y = "Pris (faste priser)",
       color = "Forbrug (kr. pr. husstand)") +
  theme_minimal()


#### OLA 1, Opgave 4.2 ####
colnames(Alkohol_Bredt)
Cor_Alkohol <- cor(Alkohol_Bredt[,c(2,4,5,6,8,9)], use = "complete.obs")
print(Cor_Alkohol)

corrplot(Cor_Alkohol, method = "circle", type = "upper", 
         addCoef.col = "black",    # Tilføjer korrelationstal i sort
         tl.col = "black",           # Farve til etiketter (variabelnavne)
         tl.srt = 45,              # Roterer etiketter for bedre læsbarhed
         diag = FALSE)


#### OLA 1, Opgave 5.1 ####
library(tidyr)
#Opgave 5.1
samlet_matrix <- data.frame(
  Klasse = rep(c("A", "B", "C", "D"), each = 9),
  Uge = rep(1:9, times = 4),
  Score = sample(1:36, replace = TRUE)
)

#### OLA 1, Opgave 5.2 ####
navn <- c("Klasse","Uge","Score")

Kvartal <- matrix(data=NA,nrow = 0,ncol = 3)
dfkvartal <- as.data.frame(Kvartal)

colnames(dfkvartal) <- navn

for(i in 1:36){
  if(i%%3==0){
    tempmean <- (mean(c(samlet_matrix[i,3],samlet_matrix[i-1,3],samlet_matrix[i-2,3])))
    klasse <- samlet_matrix[i,1]
    uge <- samlet_matrix[i,2]
    tempV <- c(klasse,uge,tempmean)
    dfkvartal <- rbind(dfkvartal,tempV)
    colnames(dfkvartal) <- navn
  } else {
  }
}

#### OLA 1, Opgave 5.3 ####
dfkvartal$Score <- as.numeric(dfkvartal$Score)
dfkvartal$Uge <- as.numeric(dfkvartal$Uge)

is.data.frame(dfkvartal)

str(dfkvartal)

dfwide <- dfkvartal %>%
  pivot_wider(names_from = "Klasse",values_from = "Score")