# Rens DK
simulated_results$Fra_Land <- "DK"

simulated_results <- simulated_results %>%
  mutate(
    Dato = sub("^(\\d+/\\d{4}).*", "\\1", property),
    Kilometer = sub("^\\d+/\\d{4}(.*)", "\\1", property) 
  )

simulated_results <- simulated_results[,c(-2,-4)]

# Nyt dataset
simulated_results1 <- simulated_results

# ForhandlerID
simulated_results1$forhandlerID[is.na(simulated_results1$forhandlerID)] <- "PrivatForhandler"

# Pris
simulated_results1$price <- gsub("kr.", "", simulated_results1$price)
simulated_results1$price <- gsub("[^0-9.,]", "", simulated_results1$price)
simulated_results1$price <- gsub("\\.", "", simulated_results1$price)
simulated_results1$price <- as.integer(simulated_results1$price)

# Kilometer
simulated_results1$Kilometer <- gsub(" km-", "", simulated_results1$Kilometer)
simulated_results1$Kilometer <- as.numeric(gsub("\\.", "", simulated_results1$Kilometer))
simulated_results1$Kilometer <- as.integer(simulated_results1$Kilometer)

# Carid
simulated_results1$carid <- as.integer(simulated_results1$carid)

# Dato
simulated_results1$Dato <- gsub("^(\\d{1})/", "0\\1/", simulated_results1$Dato)
# Tilføj "01/" foran datoer med format MM/YYYY
simulated_results1$Dato <- ifelse(grepl("^\\d{2}/\\d{4}$", simulated_results1$Dato),
                                  paste0("01/", simulated_results1$Dato),
                                  simulated_results1$Dato)

# Konverter til Date-format
simulated_results1$Dato <- as.Date(simulated_results1$Dato, format = "%d/%m/%Y")


con1 <- dbConnect(
  RMySQL::MySQL(),
  dbname = "Biler",          # Navn på databasen
  host = "localhost",        # Adresse til databasen
  port = 3306,               # Porten (standard er 3306 for MySQL)
  user = "root",       # Dit MySQL-brugernavn
  password = "Rev1l0 hceb" # Dit MySQL-password
)

# Upload til en ny tabel i databasen
dbWriteTable(con1, "simulated_data", simulated_results1, overwrite = TRUE, row.names = FALSE)

# Kontrollér, at tabellen blev oprettet
dbGetQuery(con1, "SHOW TABLES")

