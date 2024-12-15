#### OLA 2, Opgave 1.1 ####
library(devtools)
library(dkstat)
library(tidyr)

dst <- dst_get_tables(lang = "da")
ft <- dst_search(string = "befolkning", field = "text")
print(ft)
ft1 <- dst_search(string = "postnumre", field = "variables")
Befolkning <- dst_meta(table = "POSTNR1", lang = "da")
Befolkning$values #Viser alle værdierne
print(Befolkning$variables) #Viser variablerne - kategorierne

Indbyggertal <- list(
  PNR20="*",
  Tid="2024"
)

Indbyggertal <- dst_get_data(table = "POSTNR1",query = Indbyggertal,lang = "da")

# Brug pivot_wider til at omforme dataene
Indbyggertal <- pivot_wider(Indbyggertal, names_from = PNR20, values_from = value)
Indbyggertal <- Indbyggertal[,-1:-2] #Sletter årstal og det samlede antal
Indbyggertal <- as.data.frame(t(Indbyggertal))

#Vi omdøber kolonnen
colnames(Indbyggertal)[1] <- "Indbyggertal"

#Konverter rækkenavne til en ny kolonne 'Postnr'
Indbyggertal$Ny_kolonne <- (rownames(Indbyggertal))#Laver vores rownames til en kolonne (Vi har gjort det her 2 gange)
Indbyggertal$Byer <- sub("^[^ ]* ", "", Indbyggertal$Ny_kolonne) #Her sletter vi alt før første punktum
Indbyggertal$Postnr <- as.numeric(sub(" .*", "", Indbyggertal$Ny_kolonne)) #Her sletter vi alt efter første punktum
Indbyggertal <- Indbyggertal[,-2:-3]


#### OLA 2, Opgave 1.2 ####
Indbyggertal$bycat <- ifelse(Indbyggertal$Indbyggertal < 500, "landet",
                             ifelse(Indbyggertal$Indbyggertal < 1000, "landsby",
                                    ifelse(Indbyggertal$Indbyggertal < 2500, "lille by",
                                           ifelse(Indbyggertal$Indbyggertal < 10000, "almindelig by", 
                                                  ifelse(Indbyggertal$Indbyggertal < 50000, "større by", "storby")))))

#### OLA 2, Opgave 1.3 ####
boligsiden <- read.csv("boligsiden.csv", header = TRUE, sep = ",")
boligsiden <- na.omit(boligsiden) #Fjerner alle rækker med NA-værdier
colnames(boligsiden)[4] <- "Postnr"
Samlet <- merge(Indbyggertal, boligsiden, by = "Postnr")
dupletter <- which(duplicated(Samlet)) #Vi finder dubletter efter vores merge
Samlet[dupletter,] #Viser dubletterne
Samlet <- Samlet[-c(308,1034,1172,1247),] #Sletter dubletterne

#Tjekker for NA-værdier og tomme felter
sum(is.na(Samlet))
sum(Samlet == "")

#### OLA 2, Opgave 1.4 ####
# Udvælg kolonner fra det oprindelige dataset ved at bruge kolonnenumre
Clean <- Samlet[, c(1, 2, 3, 4, 7, 8, 9)]
library(ggplot2)

plot(Clean$størrelse, Clean$kvmpris,
     main = "Scatterplot af kvmpris mod størrelse",
     xlab = "Størrelse (kvm)",
     ylab = "Kvmpris",
     las = 2)

#Fjerner boliger med en kvadratmeterpris over 30.000 DKK
library(dplyr)

Clean_30k <- Clean %>%
  filter(Clean$kvmpris >= 5.000 & Clean$kvmpris <= 100.000)


#Vi prøver igen med
plot(Clean_30k$størrelse, Clean_30k$kvmpris,
     main = "Scatterplot af kvmpris mod størrelse",
     xlab = "Størrelse (kvm)",
     ylab = "Kvmpris",
     las = 2)

# Filtrer data for boliger med størrelse mellem 45 kvm og 260 kvm
Clean_30k <- Clean_30k[Clean_30k$størrelse >= 45 & Clean_30k$størrelse <= 260, ]

#Beregn gennemsnittet
Samlet_mean <- Clean_30k %>%
  group_by(bycat) %>%
  summarise(avg_kvmpris = mean(kvmpris, na.rm = TRUE))

colnames(Samlet_mean)
colnames(Samlet_mean)[colnames(Samlet_mean) == "avg_kvmpris"] <- "kvmpris"

# By_Kategori skal være en faktor og ikke character for at kunne vises
Samlet_mean$bycat <- as.factor(Samlet_mean$bycat)

ggplot(data = Samlet_mean, aes(x = reorder(bycat, -kvmpris), y = kvmpris, fill = bycat)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Bykategori", y = "kvmpris i tusinde kr. ", fill = "Bytype") +
  ggtitle("Kvmprisen er i gennemsnit større nær større byer og storbyer end længere ude mod landet") +  # Tilføj titel her
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotér x-aksens tekst for læsbarhed
