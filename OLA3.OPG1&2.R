#Vi laver FORV's indikator
library(dkstat)
library(tidyr)
library(ggplot2)

FORV <- list(
  INDIKATOR="*",
  TID="*"
)

FORV <- dst_get_data(table = "FORV1",query = FORV,lang = "da")

# Antag, at dataframen hedder 'df'
FORV$INDIKATOR <- gsub("^[^ ]+ ", "", FORV$INDIKATOR)

FORV <- pivot_wider(FORV, 
                  names_from = INDIKATOR, 
                  values_from = value, 
                  id_cols = TID)

#Starter for år 1995
FORV <- FORV[244:nrow(FORV),]
FORV <- as.data.frame(FORV)
row.names(FORV) <- FORV[,1]
FORV <- FORV[,-1]

#Vi skal lave en beregning for december måned 2024
December <- FORV[seq(12, nrow(FORV), by = 12), ]

# Vælg specifikke december-år
December_Subset <- December[rownames(December) %in% c(
  "2013-12-01", "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", 
  "2018-12-01", "2019-12-01", "2020-12-01", "2021-12-01", "2023-12-01"), ]

December_gennemsnit <- colMeans(December_Subset[, 1:13], na.rm = TRUE)
print(December_gennemsnit)

#Laver den nye række med december-data 
December_data <- data.frame(
  row.names = "2024-12-01", 
  t(c(0.77, 1.59, 10.28, 0.44, 2.19, -10.67, 
      -1.19, 19.36, 0.09, -4.81, 61.01, 30.85, 26.26))
)

colnames(December_data) <- colnames(FORV)
FORV <- rbind(FORV,December_data)

#Laver det til kvartaler
FORV <- ts(FORV, start = c(1995, 1), frequency = 12)
FORV <- aggregate(FORV, nfrequency = 4)/3
FORV <- as.data.frame(FORV)

år <- rep(1995:2024, each = 4)  # 28 år (1996-2024), 4 kvartaler pr. år
kvartal <- rep(1:4, 29)         # 4 kvartaler pr. år i 28 år
række_navn <- paste0(år, "K", kvartal) # Kombiner år og kvartal for at lave labels som "1996K1", "1996K2" osv.
række_navn <- række_navn[1:120] # Vælg de første 115 navne, da du har 115 rækker i df_ft
rownames(FORV) <- række_navn # Tilføj de nye rækkenavne til din dataframe df_ft

#Vi trækker nu sidste kvartal ud, da det kun skal bruges til at forudsige forbruget
K42024 <- FORV[120, ]
FORV <- FORV[-120, ]

#Vi indhenter forbruget
ny_datakilde <- dst_meta(table = "NKHC021", lang = "da")
print(ny_datakilde$variables) #Viser variablerne - kategorierne
Forbrugsamlet <- list(
  FORMAAAL="*",
  Tid="*",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret"
)
Forbrugdata <- dst_get_data(table = "NKHC021",query = Forbrugsamlet,lang = "da")

Forbrug_opdelt <- pivot_wider(Forbrugdata, 
                              names_from = TID, 
                              values_from = value, 
                              id_cols = FORMAAAL)

Forbrug_opdelt <- as.data.frame(Forbrug_opdelt)

rownames(Forbrug_opdelt) <- Forbrug_opdelt[, 1]
Forbrug_opdelt <- Forbrug_opdelt[, -1]  # Fjern kolonne 1 fra dataframen
Forbrug_opdelt <- as.data.frame(t(Forbrug_opdelt))

#Trækker det samlede forbrug ud til senere brug
Forbrug_opdelt1 <- data.frame(Forbrug_opdelt[, 1])
rownames(Forbrug_opdelt1) <- rownames(Forbrug_opdelt)
colnames(Forbrug_opdelt1) <- "I_alt"


#Procent - hvor meget er forbruget steget/faldet i kr. fra forrige år
Realvækst <- function(col) {
  return((exp(diff(log(as.numeric(Forbrug_opdelt1[[col]])), lag = 4)) - 1) * 100)
}

Forbrug_opdelt1$Realvækst <- c(rep(NA, 4), Realvækst("I_alt"))

#Nettotal - hvor meget er forbruger steget/faldet i kr. fra forrige år
Nettotal <- function(col) {
  return(diff(as.numeric(Forbrug_opdelt1[[col]]), lag = 4))
}

Forbrug_opdelt1$Nettotal <- c(rep(NA, 4), Nettotal("I_alt"))


Realvækst <- Forbrug_opdelt1[21:nrow(Forbrug_opdelt1),]

Realvækst_Procent <- as.data.frame(Realvækst[,2])
colnames(Realvækst_Procent) <- "Realvækst"

#Binder det sammen
FORV01 <- cbind(FORV, Realvækst_Procent)
FORV01 <- FORV01[21:nrow(FORV01),]
FORV01$Kvartal <- rownames(FORV01)


#Vi trækker FTI ud af datasettet, dette skal bruges senere
DST_FTI <- as.data.frame(FORV01[,1])
colnames(DST_FTI) <- "Forbrugertillidsindikatoren"
FORV01 <- FORV01[,-1]

#Vi vender fortegnene
colnames(FORV01)
FORV01$`Priser i dag, sammenlignet med for et år siden` <- FORV01$`Priser i dag, sammenlignet med for et år siden`*-1
FORV01$`Arbejdsløsheden om et år, sammenlignet med i dag` <- FORV01$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1

#Opg. 1.1
#Vi laver en liste til at gemme alle kombinationer og deres R^2 værdier
kombinationer <- list()
for (i in 1:12) {
  combos <- combn(1:12, i, simplify = FALSE)
  kombinationer <- append(kombinationer, combos)
}

Komb_DF <- data.frame(
  Antal = 1:length(kombinationer),
  Kombinationer = sapply(kombinationer, function(x) paste(x, collapse = "-"))
)

Mean_Komb <- data.frame(matrix(NA, nrow = nrow(FORV01), ncol = nrow(Komb_DF)))
rownames(Mean_Komb) <- rownames(FORV01)
colnames(Mean_Komb) <- Komb_DF$Kombinationer
typeof(Mean_Komb)

r2_values <- numeric(ncol(Mean_Komb))
for (j in 1:ncol(Mean_Komb)) {
  cols <- kombinationer[[j]]
  avg_komb <- rowMeans(FORV01[, cols, drop = FALSE])
  Mean_Komb[, j] <- avg_komb
  model <- lm(FORV01$Realvækst ~ avg_komb)
  r2_values[j] <- summary(model)$r.squared
}



#Opg. 1.2 
#Find den højeste R²-værdi og den tilhørende kombination
bedste_r2 <- which.max(r2_values)
højeste_r2 <- r2_values[bedste_r2]
bedste_kombination <- kombinationer[[bedste_r2]]

print(bedste_kombination)
# 3, 7, 9, 11, 12
print(højeste_r2)
#0.441334 r^2


#Find de 5 bedste kombinationer baseret på R^2
bedste_5_r2 <- order(r2_values, decreasing = TRUE)
højeste_5_r2 <- r2_values[bedste_5_r2[1:5]]
bedste_5_kombinationer <- kombinationer[bedste_5_r2[1:5]]

print(bedste_5_kombinationer)
print(højeste_5_r2)

#Opret et nyt dataframe med udvalgte kolonner fra DI_ny
begge_indikatorer <- subset(FORV01, select = c(Realvækst, Kvartal))
begge_indikatorer <- cbind(begge_indikatorer,DST_FTI)

#Gennemsnittet af de 4 underspørgsmål fra DI
begge_indikatorer$DI_Indikator <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(1,3,5,9)]/4)
  begge_indikatorer$DI_Indikator[i] <- sum 
}

#Den nye
begge_indikatorer$Bedste_Indikator <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(3,7,9,11,12)]/5)
  begge_indikatorer$Bedste_Indikator[i] <- sum 
}


#FORV
model_DI <- lm(Realvækst ~ DI_Indikator, data = begge_indikatorer)
coef(model_DI)
summary(model_DI)
#R2 = 0.3181
cor(begge_indikatorer$Realvækst, begge_indikatorer$DI_Indikator)
#R = 0.564002

#BI
model_BI <- lm(Realvækst ~ Bedste_Indikator, data = begge_indikatorer)
coef(model_BI)
summary(model_BI)
#R2 = 0.4413
cor(begge_indikatorer$Realvækst, begge_indikatorer$Bedste_Indikator)
#R = 0.6643297


ggplot(begge_indikatorer, aes(x = Kvartal)) + 
  # Søjler for Realvækst med legend
  geom_col(aes(y = Realvækst, fill = "Årlig realvækst pr. kvartal i privatforbruget"), show.legend = TRUE) +  
  # Linje for DI Indikator med legend
  geom_line(aes(y = DI_Indikator, color = "DI Indikator"), size = 0.8, group = 1, show.legend = TRUE) +  
  # Linje for Bedste Indikator med legend
  geom_line(aes(y = Bedste_Indikator, color = "Bedste Indikator"), size = 0.8, group = 1, show.legend = TRUE) +  
  # Sekundær akse for linjerne
  scale_y_continuous(sec.axis = sec_axis(~., name = "Procent")) +  
  # Kun K1 vises på x-aksen
  scale_x_discrete(breaks = begge_indikatorer$Kvartal[grepl("K1", begge_indikatorer$Kvartal)]) +  
  # Definer farverne for linjer og søjler
  scale_color_manual(values = c("DI Indikator" = "red",
                                "Bedste Indikator" = "blue")) +  # Vælg farver til dine linjer
  scale_fill_manual(values = c("Årlig realvækst pr. kvartal i privatforbruget" = "lightblue")) +  # Farve til søjlerne
  # Labels til grafen med justerede titler
  labs(y = "Procent", x = "", 
       title = "Den nye indikator (Bedste Indikator) følger forbruget bedre end DI's",
       subtitle = "Den bedste indikator er beregnet som gennemsnittet af underspørgsmålene: 3, 7, 9, 11 og 12") +
  # Brug minimal stil og roter x-aksen for bedre læsbarhed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",  # Placer legenden i bunden
        legend.title = element_blank(),  # Fjern titlen fra legenden
        legend.key = element_blank(),    # Fjern kasserne rundt om linjerne
        legend.text = element_text(size = 10),  # Juster tekststørrelsen i legenden
        plot.title = element_text(size = 14, face = "bold"),  # Gør titlen fed
        plot.subtitle = element_text(size = 10),  # Juster underteksten
        legend.background = element_rect(fill = "white", color = "white")) +  # Hvid baggrund for legenden
  # Brug guide_legend() til at vise linjer som streger og søjler som kasser
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1))),  # Kun linjetyper for linjer
         fill = guide_legend(override.aes = list(fill = "lightblue", shape = NA)))  # Søjler som kasser




#Opg. 1.3
#Kig på flere indikatorer - hvorfor har den bedste så mange spg med? 
#DST, FORV og EU bruger kun mellem 4-5
colnames(FORV01)
print(bedste_kombination)
#3  7  9 11 12
print(colnames(FORV01[,c(3,7,9,11,12)]))


#[3] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"
#[7] "Priser om et år, sammenlignet med i dag"
#[9] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
#[11] "Regner med at kunne spare op i de kommende 12 måneder"                                     
#[12] "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"


#Makro indikator:
colnames(FORV01)

#Gennemsnit af hvad vi mener er makro
begge_indikatorer$Makro_Indikator <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(3,4,6,7,8)]/5)
  begge_indikatorer$Makro_Indikator[i] <- sum 
}

model_Makro <- lm(Realvækst ~ Makro_Indikator, data = begge_indikatorer)
coef(model_Makro)
summary(model_Makro)
#R2 = 0.3196
cor(begge_indikatorer$Realvækst, begge_indikatorer$Makro_Indikator)
#R = 0.5653013

#Opg. 1.4


#Vi laver indikatoren med det fjerde kvartal
Bedste_Indikator_K4 <- rowMeans(K42024[, c(4, 8, 10, 12, 13)], na.rm = TRUE)
DI_Indikator_K4 <- rowMeans(K42024[, c(2, 4, 6, 10)], na.rm = TRUE)

#Forudsigelse med DI-indikator
summary(model_DI)
DI_Q4_2024 <- 2.25569 + 0.18078 * DI_Indikator_K4
DI_Q4_2024
#2024Q4 = 0.814723

#Forudsigelse med BI-indikator
summary(model_BI)
BI_Q4_2024 <- -1.5489 + 0.2749 * Bedste_Indikator_K4
BI_Q4_2024
#2024Q4 = 1.946


#Opg. 1.5
#Find bedste mikro spørgsmål
print(colnames(FORV01[,c(1,2,5,9,10,11,12)]))

kombinationer_mikro <- list()
r2_values_mikro <- numeric()

for (m in 1:7) {
  # Brug combn() til at finde alle kombinationer af størrelse m
  combs_mikro <- combn(c(1,2,5,9,10,11,12), m, simplify = FALSE)
  
  # 3. For hver kombination af kolonner
  for (cols in combs_mikro) {
    # a) Hvis der er kun én kolonne, brug denne direkte
    if (length(cols) == 1) {
      avg_combination_mikro <- FORV01[, cols]
    } else {
      # b) Hvis der er flere kolonner, beregn gennemsnittet af kolonnerne
      avg_combination_mikro <- rowMeans(FORV01[, cols])
    }
    
    # 4. Byg en lineær model med Realvækst som afhængig variabel
    model_mikro <- lm(FORV01$Realvækst ~ avg_combination_mikro)
    
    # 5. Træk R^2 ud og gem det
    r2_mikro <- summary(model_mikro)$r.squared
    r2_values_mikro <- c(r2_values_mikro, r2_mikro)   # Gemmer R^2 værdien
    
    # 6. Gem kombinationen
    kombinationer_mikro <- c(kombinationer_mikro, list(cols))
  }
}

#Find den bedste kombination baseret på det højeste R^2
best_combination_index_mikro <- which.max(r2_values_mikro)  # Find indekset med den højeste R^2
best_r2_mikro <- r2_values_mikro[best_combination_index_mikro]    # Træk den højeste R^2 værdi
best_combination_mikro <- kombinationer_mikro[[best_combination_index_mikro]]  # Træk den bedste kombination

print(best_combination_mikro)
# 10 (10,12 er næsten lige så godt og tager udgangspunkt i 2 spørgsmål)
print(best_r2_mikro)
#0.3457733 r^2

#Find de 5 bedste kombinationer baseret på R^2
sorted_r2_indices_mikro <- order(r2_values_mikro, decreasing = TRUE)
best_5_r2_mikro <- r2_values_mikro[sorted_r2_indices_mikro[1:5]]
best_5_combinations_mikro <- kombinationer_mikro[sorted_r2_indices_mikro[1:5]]
print(best_5_combinations_mikro)
print(best_5_r2_mikro)

#Vi tager den næstbedste som indeholder 2 spg
begge_indikatorer$Mikro_bedst <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(9,11)]/2)
  begge_indikatorer$Mikro_bedst[i] <- sum 
}

#BI_mikro
model_BI_mikro <- lm(Realvækst ~ Mikro_bedst, data = begge_indikatorer)
coef(model_BI_mikro)
summary(model_BI_mikro)
#0.3379 r^2
cor(begge_indikatorer$Realvækst, begge_indikatorer$Mikro_bedst)
#0.5812663



colnames(FORV01)
################################################################################
#2.1
colnames(FORV01)
library(pls)
pcr.fit <- pcr(Realvækst ~ 
                 `Familiens økonomiske situation i dag, sammenlignet med for et år siden` + 
                 `Familiens økonomiske  situation om et år, sammenlignet med i dag` + 
                 `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` + 
                 `Danmarks økonomiske situation om et år, sammenlignet med i dag` + 
                 `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` +
                 `Priser i dag, sammenlignet med for et år siden` + 
                 `Priser om et år, sammenlignet med i dag` + 
                 `Arbejdsløsheden om et år, sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` + 
                 `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` +
                 `Regner med at kunne spare op i de kommende 12 måneder` + 
                 `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`,
               data = FORV01, scale = TRUE, validation = "CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1 <- loadings.pcr.fit[1:12]^2
sum(w.indicators.1)
w.indicators.1
loadings(pcr.fit)

library(knitr)
summary_data <- data.frame(
  Komponenter = 1:12,
  X = c(50.25, 67.52, 79.10, 86.78, 91.66, 94.51, 96.33, 97.48, 98.56, 99.22, 99.76, 100.00),
  Realvækst = c(28.31, 40.56, 40.58, 44.04, 45.21, 45.69, 47.03, 49.24, 50.66, 53.78, 54.21, 54.22)
)
kable(summary_data, caption = "Variance Explained by Components", digits = 2, format = "markdown")

validationplot(pcr.fit, val.type = "MSEP")

#PLS
pls.fit <- plsr(Realvækst ~ 
                  `Familiens økonomiske situation i dag, sammenlignet med for et år siden` + 
                  `Familiens økonomiske  situation om et år, sammenlignet med i dag` + 
                  `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` + 
                  `Danmarks økonomiske situation om et år, sammenlignet med i dag` + 
                  `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` +
                  `Priser i dag, sammenlignet med for et år siden` + 
                  `Priser om et år, sammenlignet med i dag` + 
                  `Arbejdsløsheden om et år, sammenlignet med i dag` +
                  `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` + 
                  `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` +
                  `Regner med at kunne spare op i de kommende 12 måneder` + 
                  `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`,
                data = FORV01, scale = TRUE, validation = "CV")

# Summér resultater
summary(pls.fit)
loadings.pls.fit <- pls.fit$loadings
pls.indicators.1 <- loadings.pls.fit[1:12]^2
sum(pls.indicators.1)
pls.indicators.1
loadings(pls.fit)

library(knitr)
summary_data <- data.frame(
  Komponenter = 1:12,
  X = c(48.51, 66.42, 73.72, 77.18, 82.41, 91.41, 94.60, 96.56, 98.07, 98.67, 99.47, 100.00),
  Realvækst = c(37.30, 45.72, 49.41, 52.79, 53.67, 53.90, 54.14, 54.20, 54.22, 54.22, 54.22, 54.22)
)
kable(summary_data, caption = "Variance Explained by Components", digits = 2, format = "markdown")

validationplot(pls.fit, val.type = "MSEP")

#2.2
#PCR
#Familiens økonomiske situation i dag, sammenlignet med for et år siden (1)
#0.140218046
#Familiens økonomiske  situation om et år, sammenlignet med i dag (2)
#0.120706953
#Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.(9)
#0.141756568

pcr_weights <- data.frame(
  Spørgsmål = c("Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr. (9)",
                "Familiens økonomiske situation i dag, sammenlignet med for et år siden (1)",
                "Familiens økonomiske situation om et år, sammenlignet med i dag (2)"),
  Vægt = c(0.1417, 0.14021, 0.1207)
)

# Vis tabel med kable
kable(pcr_weights, caption = "Top 3 PCR Vægte", digits = 4)

#PLS
#Familiens økonomiske situation i dag, sammenlignet med for et år siden (1)
#0.1357737797
#Danmarks økonomiske situation i dag, sammenlignet med for et år siden (3)
#0.1277594722
#Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr. (9)
#0.1578381605

colnames(FORV)
pls_weights <- data.frame(
  Variabel = c("Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr. (9)",
               "Familiens økonomiske situation i dag, sammenlignet med for et år siden (1)",
               "Danmarks økonomiske situation i dag, sammenlignet med for et år siden (3)"),
  Vægt = c(0.1578, 0.1357, 0.1277)
)

kable(pls_weights, caption = "Top 3 PLS Vægte", digits = 4)


#2.3
K4 <- K42024[1,2:13] 

#Forudsigelse med PCR
pcr.K4 <- predict(pcr.fit, K4, ncomp = 1)
pcr.K4
# 0.96216

#Forudsigelse med PLS
pls.K4 <- predict(pls.fit, K4, ncomp = 1)
pls.K4
# 1.27132


#2.4
sum(Forbrug_opdelt1$I_alt[105:108])
#995,414 milliarder kr. 
mean(Forbrug_opdelt1$Realvækst[105:108])
#3,7%
#975 milliarder kr. 
#2%

#FORV 2016:
#"Med en årlig realvækst på godt 2 pct. i 2016 ender privatforbruget på omkring 975 mia. kr."
#forbrugsvækst på lidt over 2 pct

# Juster margenerne med par(mar)
par(mar = c(7, 5, 3, 5))  # Øger den højre margin for at give plads til teksten

# Opret data for de to sæt værdier
forbrug <- c(975, 995)  # Forudsagt og reelt forbrug i kroner
realvaekst <- c(2, 3.7)  # Årlig realvækst i procent

# Navne på kategorierne for hver søjle
navne <- c("Forudsagt forbrug", "Faktisk forbrug", "Forudsagt realvækst", "Faktisk realvækst")

# Farver til hver kategori
farver <- c("lightblue", "lightcoral", "lightblue", "lightcoral")  # Ændret til lys orange (lightcoral)

# Kombinér data til at passe til fire søjler
values <- c(forbrug, realvaekst)

# Tegn barplot for de fire søjler
bp <- barplot(values, 
              names.arg = navne, 
              col = farver,
              ylim = c(0, max(forbrug) * 1.2),
              ylab = "Privatforbruget",
              main = "Forbruget og den årlige realvækst var højere i 2016, end hvad DI's analyse forudsagde")

# Tilføj værdierne oven på søjlerne for de to første søjler (forbrug) i kroner
text(bp[1:2], forbrug + 50, labels = paste0(forbrug, " mia. kr."), cex = 0.8)

# Tilføj en ekstra y-akse til højre for procenterne (skal kalibreres korrekt)
par(new = TRUE)
barplot(c(NA, NA, realvaekst), 
        names.arg = c("", "", "Forudsagt vækst", "Faktisk vækst"),
        col = c(NA, NA, "lightblue", "lightcoral"),
        ylim = c(0, max(realvaekst) * 1.2),  
        yaxt = "n",        # Fjern venstre y-akse
        xaxt = "n")        # Fjern x-aksen midlertidigt

# Tilføj højre y-akse for procentværdierne og skriv "Årlig realvækst (%)"
#axis(4, at = seq(0, max(realvaekst) * 1.2, by = 1))  

axis(4, at = seq(0, max(realvaekst) * 1.2, by = 1), mgp = c(3, 1, 0))

mtext("Årlig realvækst (%)", side = 4, line = 2)

# Tilføj værdierne oven på søjlerne for de to sidste søjler (vækst) i procent
text(bp[3:4], realvaekst + 0.2, labels = paste0(realvaekst, "%"), cex = 0.8)

# Tilføj en legend under plottet
legend("bottom", legend = c("Forudsagt", "Faktisk"), 
       fill = c("lightblue", "lightcoral"), 
       horiz = TRUE, cex = 0.5, xpd = TRUE, inset = c(0, -0.17))