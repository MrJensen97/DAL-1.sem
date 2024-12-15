#Opg. 3.1 
#Med kun 4 kvartal
Julehandel <- begge_indikatorer[seq(4, nrow(begge_indikatorer), by = 4), ]


#Opretter en ny variabel `retning`, hvor "OP" repræsenterer Realvækst >= 0, og "NED" repræsenterer Realvækst < 0.
#0 = OP
retning <- ifelse(Julehandel$Realvækst >= 0, "OP", "NED")
print(retning)

Julehandel$Dummy_Realkvæksten <- retning

#Konverterer `Dummy_Realkvæksten` til en kategorisk variabel (faktor) med niveauerne "NED" og "OP".
Julehandel$Dummy_Realkvæksten <- factor(Julehandel$Dummy_Realkvæksten, levels = c("NED", "OP"))

#Logit
Jul_model <- glm(Dummy_Realkvæksten ~ 
                   Forbrugertillidsindikatoren,
                   data = Julehandel, 
                   family = binomial)


# Se resultaterne af modellen
summary(Jul_model)

coef(Jul_model)
summary(Jul_model)$coef

#FTI for at lave forudsigelsen
K42024_subset <- K42024[, 1, drop = FALSE]

#Forudsigelse ud fra den trænede model
glm.probs <- predict(Jul_model, type = "response") #Ser om den går op eller ned
summary(glm.probs)
print(glm.probs)

#Forudsigelse af K4
jul.glm.probs <- predict(Jul_model,newdata = K42024_subset, type = "response") #Ser om den går op eller ned
summary(jul.glm.probs)
print(jul.glm.probs)
#Predict = 0.671555


#2024K4 vil stige

#Opg. 3.2
contrasts(Julehandel$Dummy_Realkvæksten) #Viser hvad der er 1 eller 0
glm.pred <- rep("NED",24)
glm.pred[glm.probs >.50]="OP"
length(jul.glm.pred)
length(Julehandel$Dummy_Realkvæksten)
table(glm.pred, Julehandel$Dummy_Realkvæksten)
mean(glm.pred==Julehandel$Dummy_Realkvæksten)

# Definer mulige værdier for kategorierne
categories <- c("NED", "OP")

# Opret confusion matrix med faste levels
confusion_matrix <- table(
  factor(glm.pred, levels = categories), 
  factor(Julehandel$Dummy_Realkvæksten, levels = categories)
)

# Konverter confusion matrix til en data frame
confusion_df <- as.data.frame.matrix(confusion_matrix)

# Tilføj totals
confusion_df$Total <- rowSums(confusion_matrix)
confusion_df <- rbind(confusion_df, Total = colSums(confusion_df))

# Tilføj en kolonne til at angive rækkerne som 'Predicted'
predicted_labels <- c(categories, "Total")
confusion_df <- cbind(Predicted = predicted_labels, confusion_df)

# Beregn modelens nøjagtighed
accuracy <- round(mean(glm.pred == Julehandel$Dummy_Realkvæksten) * 100, 2)

# Tilføj nøjagtighed som en ekstra række uden NA-værdier
accuracy_row <- c("Model Accuracy", rep("", ncol(confusion_df) - 2), paste(accuracy, "%"))
confusion_df <- rbind(confusion_df, accuracy_row)

# Udskriv confusion_df
print(confusion_df)

# Opret en pæn gt-tabel med totalrækker, lodret og vandret linje
library(gt)

gt_table <- gt(data = confusion_df) %>%
  tab_header(
    title = "Modellen viser høj nøjagtighed i at forudsige 'OP'",
    subtitle = "OP = 1 | NED = 0"
  ) %>%
  fmt_number(columns = c("NED", "OP", "Total"), decimals = 0) %>%  # Vis tal uden decimaler
  cols_label(
    Predicted = "Predicted default status",
    NED = "NED",
    OP = "OP",
    Total = "Total"
  ) %>%
  tab_spanner(
    label = "True default status",
    columns = c("NED", "OP", "Total")
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgrey",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "Predicted"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)  # Tykkere sort vandret streg
    ),
    locations = cells_body(
      rows = nrow(confusion_df) - 1  # Læg stregen før 'Model Accuracy'
    )
  )

# Vis tabel
gt_table


library(pROC)

roc_curve <- roc(Julehandel$Dummy_Realkvæksten, glm.probs)

# Plot ROC-kurven med legacy.axes for at ændre x-aksen til at vise False Positive Rate direkte
plot(roc_curve, 
     col = "blue",  # Blå farve for at matche
     lwd = 2,       # Gør linjen tykkere
     main = "Modellen er god til at forudsige stigning i husholdningernes forbrugsudgifter i juleperioden",  # Titel
     xlab = "False positive rate",  # Ændrer akse til False Positive Rate
     ylab = "True positive rate",   # Ændrer y-akse til True Positive Rate
     legacy.axes = TRUE)            # Skifter x-aksen til at starte ved 0

# Tilføj AUC-værdi på plottet
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)


optimal_cutoff1 <- coords(roc_curve1, "best", ret = "threshold")
print(optimal_cutoff1)

# Udskriv den optimale cutoff-værdi
print(paste("Optimal cutoff:", optimal_cutoff1))


### ER IKKE LAVET - KAN BRUGES TIL ANDRE SCENARIE 
glm.probs1 <- predict(Alle_model,type = "response") #Ser om den går op eller ned
glm.probs1
glm.pred1 <- rep("NED",100)
glm.pred1[glm.probs1 >.800490718445643]="OP"
table(glm.pred1, begge_indikatorer$Dummy_Realkvæksten)
mean(glm.pred1==begge_indikatorer$Dummy_Realkvæksten)


#Konfusionmatrice
confusion_matrix2 <- table(glm.pred1, begge_indikatorer$Dummy_Realkvæksten)

# Konverter confusion matrix til en data frame
confusion_df2 <- as.data.frame.matrix(confusion_matrix2)

# Tilføj totals
confusion_df2$Total <- rowSums(confusion_matrix2)
confusion_df2 <- rbind(confusion_df2, Total = colSums(confusion_df2))

# Tilføj en kolonne til at angive rækkerne som 'Predicted'
confusion_df2 <- cbind(Predicted = c("NED", "OP", "Total"), confusion_df2)

# Beregn modelens nøjagtighed
accuracy2 <- round(mean(glm.pred1 == begge_indikatorer$Dummy_Realkvæksten) * 100, 2)

# Tilføj nøjagtighed som en ekstra række uden NA-værdier
confusion_df2 <- rbind(confusion_df2, c("Model Accuracy", "", "", paste(accuracy2, "%")))

# Opret en pæn gt-tabel med totalrækker, lodret og vandret linje
library(gt)

gt_table2 <- gt(data = confusion_df2) %>%
  tab_header(
    title = "Modellen viser høj nøjagtighed i at forudsige 'OP'",
    subtitle = "OP = 1 | NED = 0"
  ) %>%
  fmt_number(columns = c("NED", "OP", "Total"), decimals = 0) %>%  # Vis tal uden decimaler
  cols_label(
    Predicted = "Predicted default status",
    NED = "NED",
    OP = "OP",
    Total = "Total"
  ) %>%
  tab_spanner(
    label = "True default status",
    columns = c("NED", "OP", "Total")
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgrey",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "Predicted"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)  # Tykkere sort vandret streg
    ),
    locations = cells_body(
      rows = nrow(confusion_df2) - 1  # Læg stregen før 'Model Accuracy'
    )
  )

# Vis tabel
gt_table2    




