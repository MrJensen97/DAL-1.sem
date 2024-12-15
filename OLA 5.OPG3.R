library(readr)
library(ggplot2)
library(dplyr)

#### Opg. 3.1 ####
Regnskaber <- read_delim(
  "regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv",
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "ISO-8859-1")
)

#Vi slår dårlige og dårlig sammen som dårlige
Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` <- gsub("^Dårlig$", "Dårlige", Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)
#Vi skal fjerne "Ved ikke"
Regnskaber <- Regnskaber[Regnskaber[[1]] != "Ved ikke", ]


Muligheder <- table(Regnskaber$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

Muligheder_df <- as.data.frame(Muligheder)
colnames(Muligheder_df) <- c("Svar", "Antal")


ggplot(Muligheder_df, aes(x = reorder(Svar, -Antal), y = Antal, fill = Svar)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Antal), vjust = -0.5, size = 3) + # Tilføj antal over hver søjle
  labs(
    title = "Størstedelen af virksomhederne vurderer deres lånemuligheder som gode",
    x = "Svarmulighederne",
    y = "Antal svar"
  ) +
  scale_fill_brewer(palette = "Set3") + # Pæn farvepalette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotér x-aksens labels
    legend.title = element_blank() # Fjern titel på legend (valgfrit)
  )


#Vi laver søjlerne i procent
Muligheder_df1 <- Muligheder_df %>%
  mutate(Procent = Antal / sum(Antal) * 100)

ggplot(Muligheder_df1, aes(x = reorder(Svar, -Procent), y = Procent, fill = Svar)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Procent)), vjust = -0.5, size = 3) + # Procenter som labels
  labs(
    title = "Størstedelen af virksomhederne vurderer deres lånemuligheder som gode",
    x = "Svarmulighederne",
    y = "Procent (%)"
  ) +
  scale_fill_brewer(palette = "Set3") + # Pæn farvepalette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotér x-aksens labels
    legend.title = element_blank() # Fjern titel på legend (valgfrit)
  )



#### Opg 3.2 ####
Regnskaber1 <- Regnskaber
colnames(Regnskaber)

#Cleaning af data

#Erstat kun komma med punktum uden at generere NA for ikke-numeriske tekst
Regnskaber1[] <- lapply(Regnskaber1, function(x) {
  if (is.character(x)) {
    # Tjek om kolonnen indeholder værdier med tal og komma
    if (any(grepl(",", x))) {
      # Forsøg at omdanne komma til punktum og konverter til numerisk
      suppressWarnings({
        new_x <- as.numeric(gsub(",", ".", x))
      })
      # Hvis konverteringen til numerisk ikke giver NA, returnér den
      if (!all(is.na(new_x))) {
        return(new_x)
      } else {
        return(x) # Behold originalen, hvis konvertering fejler
      }
    } else {
      return(x) # Behold originalen, hvis ingen komma
    }
  } else {
    return(x) # Bevar kolonner, der ikke er tekst
  }
})

#Fjerner NA-værdier
colSums(is.na(Regnskaber1))
Regnskaber1 <- Regnskaber1[, colSums(is.na(Regnskaber1)) <= 30]
Regnskaber1 <- na.omit(Regnskaber1)

#Laver det om til tal
Regnskaber1$Lånemuligheder_dummy <- factor(
  Regnskaber1$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  labels = c(1, 2, 3, 4, 5),
  ordered = TRUE
)


table(Regnskaber1$Lånemuligheder_dummy, useNA = "ifany")

Regnskaber1$`Branchekode primær` <- scale(Regnskaber1$`Branchekode primær`) %>% as.vector()

library(ordinal)

Fml <- clm(Lånemuligheder_dummy ~ `Branchekode primær`, data = Regnskaber1)
summary(Fml)
#### Dataframe til clm ####
# Få summariserede resultater fra modellen
summary_Fml <- summary(Fml)

# Ekstraher koefficienter og deres statistikker
koefficienter <- summary_Fml$coefficients

# Opret en data.frame med resultaterne
resultater_Fml <- data.frame(
  Variabel = rownames(koefficienter),
  Estimate = koefficienter[, "Estimate"],
  Std.Error = koefficienter[, "Std. Error"],
  t.value = koefficienter[, "z value"],  # t-værdi svarer til z-værdi i ordinal regression
  Pr = koefficienter[, "Pr(>|z|)"]       # p-værdi
)

# Vis resultaterne
View(resultater_Fml)

###########################
Fml1 <- clm(Lånemuligheder_dummy ~ `Afkastningsgrad 2020 (%)`, data = Regnskaber1)
summary(Fml1)


Fml2 <- clm(Lånemuligheder_dummy ~ `Soliditetsgrad 2020 (%)`, data = Regnskaber1)
summary(Fml2)



# Gruppér svarene og summer procenten
Muligheder_df1_grouped <- Muligheder_df1 %>%
  mutate(Svar_grouped = case_when(
    Svar %in% c("Gode", "Meget gode") ~ "Gode/Meget gode",
    Svar %in% c("Dårlige", "Meget dårlige") ~ "Dårlige/Meget dårlige",
    TRUE ~ Svar
  )) %>%
  group_by(Svar_grouped) %>%
  summarise(Total_Procent = sum(Procent)) # Summerer procenten for hver gruppe


# Plot af spørgsmålet: ”Hvordan ser du mulighederne for at låne penge til din virksomhed?”
# Opret et dataset med den ønskede rækkefølge for svarene
Muligheder_df1_grouped <- Muligheder_df1 %>%
  mutate(Svar_grouped = case_when(
    Svar %in% c("Gode", "Meget gode") ~ "Gode/Meget gode",
    Svar %in% c("Dårlige", "Meget dårlige") ~ "Dårlige/Meget dårlige",
    TRUE ~ Svar  # Dette fanger "Neutrale"
  )) %>%
  group_by(Svar_grouped) %>%
  summarise(Total_Procent = sum(Procent), .groups = 'drop')

# Sørg for, at Svar_grouped er en factor med den ønskede rækkefølge
Muligheder_df1_grouped$Svar_grouped <- factor(Muligheder_df1_grouped$Svar_grouped,
                                              levels = c("Dårlige/Meget dårlige", "Neutrale", "Gode/Meget gode"))

# Opret ggplot
ggplot(Muligheder_df1_grouped, aes(x = Svar_grouped, y = Total_Procent, fill = Svar_grouped)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar chart med adskilte grupper
  geom_text(aes(label = sprintf("%.1f%%", Total_Procent)), vjust = -0.5, size = 3) +  # Procenter som labels
  labs(
    title = "Langt de fleste virksomheder ser deres muligheder for at låne som Gode/Meget gode",
    x = "Svarmuligheder",
    y = "Procent (%)"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Farvepalette
  theme_minimal() +  # Minimalistisk tema
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotér x-aksens labels
    legend.title = element_blank(),  # Fjern legend titel
    plot.title = element_text(hjust = 0.5)  # Centrer titel
  )


#### Opgave 3.3 ####
# Gruppér svarmulighederne og beregn gennemsnittet for Soliditetsgraden
Regnskaber1_grouped_Soliditet <- Regnskaber1 %>%
  mutate(Svar_grouped = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Dårlige", "Meget dårlige") ~ "Dårlige/Meget dårlige",
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode",
    TRUE ~ `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`  # "Neutral" forbliver uændret
  )) %>%
  group_by(Svar_grouped) %>%
  summarise(
    Gennemsnit_Soliditetsgrad = mean(`Soliditetsgrad 2020 (%)`, na.rm = TRUE),  # Beregn gennemsnittet
    Antal_besvarelser = n()  # Beregn antal besvarelser for hver gruppe
  )

Regnskaber1_grouped_Afkast <- Regnskaber1 %>%
  mutate(Svar_grouped = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Dårlige", "Meget dårlige") ~ "Dårlige/Meget dårlige",
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode",
    TRUE ~ `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`  # "Neutral" forbliver uændret
  )) %>%
  group_by(Svar_grouped) %>%
  summarise(
    Gennemsnit_Afkastningsgrad = mean(`Afkastningsgrad 2020 (%)`, na.rm = TRUE),  # Beregn gennemsnittet
    Antal_besvarelser = n()  # Beregn antal besvarelser for hver gruppe
  )


# Plot Soliditetsgrad
# Omarranger rækkefølgen af svargrupperne
Regnskaber1_grouped_Soliditet$Svar_grouped <- factor(Regnskaber1_grouped_Soliditet$Svar_grouped,
                                           levels = c("Dårlige/Meget dårlige", "Neutrale", "Gode/Meget gode"))

# Opret ggplot for at visualisere de grupperede svar og gennemsnittet af soliditetsgrad
ggplot(Regnskaber1_grouped_Soliditet, aes(x = Svar_grouped, y = Gennemsnit_Soliditetsgrad, fill = Svar_grouped)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar chart med adskilte grupper
  geom_text(aes(label = sprintf("%.1f", Gennemsnit_Soliditetsgrad)), vjust = -0.5, size = 3) +  # Gennemsnit som labels
  labs(
    title = "Virksomhederne er lige optimistiske om at kunne låne penge trods deres svar",
    x = "Svarmuligheder",
    y = "Gennemsnitlig Soliditetsgrad 2020 (%)"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Farvepalette
  theme_minimal() +  # Minimalistisk tema
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotér x-aksens labels
    legend.title = element_blank(),  # Fjern legend titel
    plot.title = element_text(hjust = 0.5)  # Centrer titel
  )


# Plot afkastningsgraf
# Omarranger rækkefølgen af svargrupperne
Regnskaber1_grouped_Afkast$Svar_grouped <- factor(Regnskaber1_grouped_Afkast$Svar_grouped,
                                                     levels = c("Dårlige/Meget dårlige", "Neutrale", "Gode/Meget gode"))

ggplot(Regnskaber1_grouped_Afkast, aes(x = Svar_grouped, y = Gennemsnit_Afkastningsgrad, fill = Svar_grouped)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar chart med adskilte grupper
  geom_text(aes(label = sprintf("%.1f", Gennemsnit_Afkastningsgrad)), vjust = -0.5, size = 3) +  # Gennemsnit som labels
  labs(
    title = "Neutrale lånemulighed har i gennemsnit den største afkastningsgrad",
    x = "Svarmuligheder",
    y = "Gennemsnitlig Afkastningsgrad 2020 (%)"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Farvepalette
  theme_minimal() +  # Minimalistisk tema
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotér x-aksens labels
    legend.title = element_blank(),  # Fjern legend titel
    plot.title = element_text(hjust = 0.5)  # Centrer titel
  )


############

# Kombinere de relevante kategorier
Regnskaber1$Lånekategori_combined <- recode(Regnskaber1$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
                                            "Gode" = "Gode/Meget gode",
                                            "Meget gode" = "Gode/Meget gode",
                                            "Dårlige" = "Dårlige/Meget dårlige",
                                            "Meget dårlige" = "Dårlige/Meget dårlige",
                                            "Neutral" = "Neutrale")

# Gruppér virksomhedstyperne
Regnskaber1$`Branchebetegnelse primær` <- dplyr::case_when(
  grepl("handel|detail|engros|salg|butik", Regnskaber1$`Branchebetegnelse primær`, ignore.case = TRUE) ~ "Handelsvirksomhed",
  grepl("fremstilling|produktion|fabrik|industri|metal|møbler|maskiner|byggeri", Regnskaber1$`Branchebetegnelse primær`, ignore.case = TRUE) ~ "Produktionsvirksomhed",
  TRUE ~ "Servicevirksomhed"
)

# Opsummér dataene
Regnskaber1_summary <- Regnskaber1 %>%
  group_by(`Branchebetegnelse primær`, Lånekategori_combined) %>%
  summarise(Count = n()) %>%
  mutate(Procent = (Count / sum(Count)) * 100)

# Reorder Lånekategori_combined så "Neutrale" er i midten
Regnskaber1_summary$Lånekategori_combined <- factor(Regnskaber1_summary$Lånekategori_combined,
                                                    levels = c("Dårlige/Meget dårlige", "Neutrale", "Gode/Meget gode"))

# Plot med 3x3 søjler og "Neutrale" i midten
ggplot(Regnskaber1_summary, aes(x = Lånekategori_combined, y = Procent, fill = Lånekategori_combined)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ `Branchebetegnelse primær`, ncol = 3) + # Tre kolonner for virksomhedstyper
  labs(
    title = "Branchekategori og vurdering af lånemuligheder",
    x = "Lånekategori",
    y = "Procent (%)",
    fill = "Lånekategori"
  ) +
  scale_fill_manual(values = c("Gode/Meget gode" = "#1f77b4",   # Blå
                               "Neutrale" = "#4d4d4d",    # Mørkegrå
                               "Dårlige/Meget dårlige" = "#bfbfbf")) + # Lysegrå
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Drej etiketter for læsbarhed
  geom_text(aes(label = scales::percent(Procent / 100)),  # Tilføj procentetiketter
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4)