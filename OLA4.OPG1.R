library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(DBI)
library(RMySQL)

getwd()

#Bilbasen - Polestar
#First page
startlink <- "https://www.bilbasen.dk/brugt/bil/polestar?fuel=3&includeengroscvr=true&includeleasing=false"
rawres <- GET(url=startlink)
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

#Transform text to html-node
page <- read_html(rawcontent)

#Extract car-elements from page
carlist <- page %>% html_elements("article")

#tagliste
ptag <- "[class^='Listing_price']"
dettag <- "[class^='ListingDetails_list']"
proptag <- ".Listing_properties___ptWv"
mmtag <- "[class^='Listing_makeModel']"
desctag <- "[class^='Listing_description']"
loctag <- "[class^='Listing_location']"

href="https://www.bilbasen.dk/brugt/bil/polestar/2/long-range-5d/6343717"

src = "https://billeder.bilbasen.dk/bilinfo/ccad9424-113d-49c2-a0b6-bf125d5c7965.jpg?class=S400X400"


#dataframe til opsamling
cn=c("price","property","model","detailitems","description","location","link","carid","forhandlerID","scrapedate")

calldf <- as.data.frame((matrix(data=NA,nrow=0,ncol = 10)))
colnames(calldf) <- cn

total_pages <- page %>%
  html_element("[data-e2e='pagination-total']") %>%
  html_text(trim = TRUE) %>%
  as.numeric()

cat("Antal sider fundet:", total_pages, "\n")

#side 1
for (car in carlist) {
  tryCatch({
    st <- sample(1:5,1)
    Sys.sleep(st)
    print(st)
    price <- car %>% html_element(ptag) %>%html_text()
    property <- car %>% html_element(proptag) %>%html_text()
    model <- car %>% html_element(mmtag) %>%html_text()
    detailitems <- car %>% html_element(dettag) %>%html_text() %>% paste0(collapse = "_")
    description <- car %>% html_element(desctag) %>%html_text() 
    location <- car %>% html_element(loctag) %>%html_text() 
    link <- car %>% html_element("a") %>% html_attr("href")
    carid <- link %>% str_extract("[0-9]{7}")
    forhandler_link <- car %>% html_element("img") %>% html_attr("src")
    forhandlerID <- forhandler_link %>% str_extract("(?<=/bilinfo/)[a-zA-Z0-9-]+(?=\\.)")
    tmpdf <- data.frame(price, property, model, detailitems, description, location, link, carid, forhandlerID,Sys.time())
    calldf <- rbind(calldf,tmpdf)
  },
  error = function(cond) {
    print(price)
  }
  )
}

#Resten
for (i in 2:total_pages){
  
  nextlink <- paste0("https://www.bilbasen.dk/brugt/bil/polestar?fuel=3&includeengroscvr=true&includeleasing=false&page=",i)
  rawres1 <- GET(url=nextlink)
  rawres1$status_code
  rawcontent1 <- httr::content(rawres1,as="text")
  
  #Transform text to html-node
  page1 <- read_html(rawcontent1)
  
  #Extract car-elements from page
  carlist1 <- page1 %>% html_elements("article")
  
  for (car in carlist1) {
    tryCatch({
      st <- sample(1:5,1)
      Sys.sleep(st)
      print(i)
      price <- car %>% html_element(ptag) %>%html_text()
      property <- car %>% html_element(proptag) %>%html_text()
      model <- car %>% html_element(mmtag) %>%html_text()
      detailitems <- car %>% html_element(dettag) %>%html_text() %>% paste0(collapse = "_")
      description <- car %>% html_element(desctag) %>%html_text() 
      location <- car %>% html_element(loctag) %>%html_text() 
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      forhandler_link <- car %>% html_element("img") %>% html_attr("src")
      forhandlerID <- forhandler_link %>% str_extract("(?<=/bilinfo/)[a-zA-Z0-9-]+(?=\\.)")
      tmpdf <- data.frame(price, property, model, detailitems, description, location, link, carid, forhandlerID,Sys.time())
      calldf <- rbind(calldf,tmpdf)
    },
    error = function(cond) {
      print(price)
    }
    )
  }
}

#opg 1.2
results <- calldf %>%
  mutate(
    description_clean = description %>%
      # Fjern emojis og specialtegn
      str_replace_all("🔴|✅|📞|🚗|👉|✍️|➾|%|\\n", " ") %>%
      # Fjern ekstra specialtegn som links eller symboler
      str_replace_all("http[s]?://\\S+|www\\.\\S+", "") %>%
      # Fjern unødvendige tegn som *, &, @, osv.
      str_replace_all("[^[:alnum:][:space:].,]", "") %>%
      # Erstat linjeskift og flere mellemrum med ét mellemrum
      str_replace_all("\\s+", " ") %>%
      # Trim overskydende mellemrum i starten og slutningen
      str_trim()
  )

# OPGAVE 1.3 - Hente nye data - simuleret
library(lubridate)

# Tilføj den oprindelige scrapedato (antag dags dato som start)
results <- results %>%
  mutate(scrapedate = Sys.Date())  # Erstatter Sys.Date() med den faktiske dato, hvis relevant

# Kopi af det oprindelige datasæt
simulated_results <- results

# Nye biler (manuelt tilføjet data)
new_cars <- data.frame(
  price = c("310.000 kr.", "450.000 kr."),
  model = c("Polestar 2Performance Long Range", "Polestar 2Standard Range"),
  location = c("Aarhus, Østjylland", "Esbjerg, Sydjylland"),
  description_clean = c("Ny bil med udvidet garanti.", "Lækkert udstyr, kun kørt få kilometer."),
  link = c("https://www.bilbasen.dk/new_car_1", "https://www.bilbasen.dk/new_car_2"),
  scrapedate = Sys.Date() + 1
)

# Tilføj nye biler til det simulerede datasæt
simulated_results <- bind_rows(simulated_results, new_cars)

# Opdater priser for 3 tilfældige biler
set.seed(42)  # For at sikre gentagelige resultater
rows_to_update <- sample(1:nrow(simulated_results), 3)

# Opdater priser (simuleret ændring)
simulated_results$price[rows_to_update] <- c("299.000 kr.", "369.000 kr.", "419.000 kr.")

# Fjern 5 tilfældige rækker
rows_to_remove <- sample(1:nrow(simulated_results), 5)

# Fjern rækkerne fra datasættet
simulated_results <- simulated_results[-rows_to_remove, ]

# Tæl antallet af ændringer
cat("Antal biler i datasættet: ", nrow(simulated_results), "\n")
cat("Antal nye biler: ", nrow(new_cars), "\n")

simulated_results <- simulated_results %>%
  mutate(
    status = case_when(
      link %in% new_cars$link ~ "ny",
      row_number() %in% rows_to_update ~ "ændret",
      TRUE ~ "uændret"
    )
  )


###########################################################################################################
#####opgave 1.4 ###########################################################################################
###########################################################################################################

#Bilbasen - Polestar
#First page
startlink_de <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmk%5D=123&s%5Bpr_min%5D=3000&s%5By_min%5D=2014"

rawres_de <- GET(url=startlink_de)
rawres_de$status_code
rawcontent_de <- httr::content(rawres_de,as="text")

#Transform text to html-node
page_de <- read_html(rawcontent_de)

#Extract car-elements from page
carlist_de <- page_de %>%
  html_elements("div.columns.car-ad") 

#tagliste
ptag_de <- "[class^='purchase-price']"
dettag_de <- ".reg_year"
model_de <- "[class^='provider-link']"
proptag_de <- ".mileage"


#dataframe til opsamling
cn_de=c("price","Kilometer","model","Dato","forhandlerID","carid","scrapedate")

calldf_de <- as.data.frame((matrix(data=NA,nrow=0,ncol = 7)))
colnames(calldf_de) <- cn_de

#side 1
for (car in carlist_de) {
  tryCatch({
    st <- sample(1:5, 1)
    Sys.sleep(st)
    print(st)
    
    price <- car %>% html_element(ptag_de) %>% html_text()
    Kilometer <- car %>% html_element(proptag_de) %>% html_text()
    model <- car %>% html_element(model_de) %>% html_text()
    Dato <- car %>% html_element(dettag_de) %>% html_text()
    forhandlerID <- car %>% html_element("a") %>% html_attr("data-provider")
    
    carid <- as.character(car) %>% 
      str_extract("car_ad_id=\\d+") %>% 
      str_remove("car_ad_id=")
    
    print(carid)
    
    tmpdf_de <- data.frame(price, Kilometer, model, Dato, forhandlerID, carid, Sys.time())
    calldf_de <- rbind(calldf_de, tmpdf_de)
  },
  error = function(cond) {
    print("Error encountered:")
    print(cond)
  })
}


#Resten
for (i in 2:11){
  
  nextlink_de <- paste0("https://www.12gebrauchtwagen.de/suchen?page=",i,"&s%5Bmk%5D=123&s%5Bpr_min%5D=3000&s%5By_min%5D=2014")
  rawres_de1 <- GET(url=nextlink_de)
  rawres_de1$status_code
  rawcontent_de1 <- httr::content(rawres_de1,as="text")
  
  #Transform text to html-node
  page_de1 <- read_html(rawcontent_de1)
  
  #Extract car-elements from page
  carlist_de1 <- page_de1 %>%
    html_elements("div.columns.car-ad") 
  
  for (car_de in carlist_de1) {
    tryCatch({
      st <- sample(1:5, 1)
      Sys.sleep(st)
      print(i)
      
      price <- car_de %>% html_element(ptag_de) %>% html_text()
      Kilometer <- car_de %>% html_element(proptag_de) %>% html_text()
      model <- car_de %>% html_element(model_de) %>% html_text()
      Dato <- car_de %>% html_element(dettag_de) %>% html_text()
      forhandlerID <- car_de %>% html_element("a") %>% html_attr("data-provider")
      
      
      # Konverter car til tekst og udtræk carid
      carid <- as.character(car_de) %>% 
        str_extract("car_ad_id=\\d+") %>% 
        str_remove("car_ad_id=")
      
      # Print kun carid
      print(carid)
      
      tmpdf_de <- data.frame(price, Kilometer, model, Dato, forhandlerID, carid, Sys.time())
      calldf_de <- rbind(calldf_de, tmpdf_de)
    },
    error = function(cond) {
      print("Error encountered:")
      print(cond)
    }
    )
  }
}

#Rens DE
calldf_de$price <- as.numeric(gsub(" €", "", calldf_de$price))
calldf_de$price_dk <- calldf_de$price*7.46
calldf_de$Fra_Land <- "DE"
calldf_de$Dato <- gsub("EZ ", "", calldf_de$Dato)

colnames(calldf_de)

calldf_de <- calldf_de[,-1]
colnames(calldf_de)[which(colnames(calldf_de) == "price_dk")] <- "price"

calldf_de$Kilometer <- gsub(" km", "", calldf_de$Kilometer)

#Friskt dataset
calldf_de1 <- calldf_de

#Rens DE Kilometer
calldf_de1$Kilometer <- gsub("[^0-9.,]", "", calldf_de1$Kilometer)
calldf_de1$Kilometer <- gsub("\\.", "", calldf_de1$Kilometer)  # Fjern punktum som tusindseparator
calldf_de1$Kilometer <- as.numeric(calldf_de1$Kilometer)

calldf_de1$carid <- as.numeric(calldf_de1$carid)

#Rens DE Pris
calldf_de1$price <- gsub("kr.", "", calldf_de1$price)
calldf_de1$price <- gsub("[^0-9.,]", "", calldf_de1$price)
calldf_de1$price <- gsub("\\.", "", calldf_de1$price)
calldf_de1$price <- as.numeric(calldf_de1$price)

#Rens Dato
typeof(calldf_de1$Dato)

# Fjern kun bindestreger fra værdierne
calldf_de1$Dato <- gsub("-", "", calldf_de1$Dato)

# Fjern non-breaking spaces (u00A0) og almindelige mellemrum
calldf_de1$Dato <- gsub("\u00A0|\\s", "", calldf_de1$Dato)

# Tilføj "01" foran værdier, der starter med "/"
calldf_de1$Dato <- ifelse(substr(calldf_de1$Dato, 1, 1) == "/",
                          paste0("01", calldf_de1$Dato),
                          calldf_de1$Dato)

# Tilføj "01/" foran værdier, der kun har MM/YYYY
calldf_de1$Dato <- ifelse(grepl("^\\d{2}/\\d{4}$", calldf_de1$Dato),
                          paste0("01/", calldf_de1$Dato),
                          calldf_de1$Dato)

# Konverter til Date-format
calldf_de1$Dato <- as.Date(calldf_de1$Dato, format = "%d/%m/%Y")

# Tjek resultatet
print(unique(calldf_de1$Dato))




#Rens DK
calldf$Fra_Land <- "DK"

calldf <- calldf %>%
  mutate(
    Dato = sub("^(\\d+/\\d{4}).*", "\\1", property),
    Kilometer = sub("^\\d+/\\d{4}(.*)", "\\1", property) 
  )

calldf <- calldf[,c(-2,-4)]

#Nyt dataset
calldf1 <- calldf

#ForhandlerID

calldf1$forhandlerID[is.na(calldf1$forhandlerID)] <- "PrivatForhandler"

#Pris
calldf1$price <- gsub("kr.", "", calldf1$price)
calldf1$price <- gsub("[^0-9.,]", "", calldf1$price)
calldf1$price <- gsub("\\.", "", calldf1$price)
calldf1$price <- as.integer(calldf1$price)

#Kilometer
calldf1$Kilometer <- gsub(" km-", "", calldf1$Kilometer)
calldf1$Kilometer <- as.numeric(gsub("\\.", "", calldf1$Kilometer))
calldf1$Kilometer <- as.integer(calldf1$Kilometer)

#Carid
calldf1$carid <- as.integer(calldf1$carid)

#Dato

calldf1$Dato <- gsub("^(\\d{1})/", "0\\1/", calldf1$Dato)
# Tilføj "01/" foran datoer med format MM/YYYY
calldf1$Dato <- ifelse(grepl("^\\d{2}/\\d{4}$", calldf1$Dato),
                       paste0("01/", calldf1$Dato),
                       calldf1$Dato)

# Konverter til Date-format
calldf1$Dato <- as.Date(calldf1$Dato, format = "%d/%m/%Y")



#Merge
Alle_biler <- merge(calldf1, calldf_de1, by = c("price", "Kilometer","carid","model","forhandlerID","Sys.time..","Fra_Land","Dato"), all = TRUE)
colnames(Alle_biler)[which(colnames(Alle_biler) == "Sys.time..")] <- "Scrapedate"


#Sammenligning af priser fra DK og DE
Alle_biler$model <- ifelse(grepl("Single Motor", Alle_biler$description, ignore.case = TRUE), 
                         paste(Alle_biler$model, "Single Motor"), 
                         ifelse(grepl("Dual Motor", Alle_biler$description, ignore.case = TRUE), 
                                paste(Alle_biler$model, "Dual Motor"), 
                                Alle_biler$model))

Alle_biler1 <- Alle_biler

colnames(Alle_biler1)
Alle_biler1 <- Alle_biler1[,c(-5,-6,-9,-10,-11)]

Alle_biler1$model <- gsub("-2", "", Alle_biler1$model)               
Alle_biler1$model <- gsub("2 Long", "2Long", Alle_biler1$model) 
Alle_biler1$model <- gsub("Polestar  +2", "Polestar 2", Alle_biler1$model)

filtered_biler <- Alle_biler1[grep("^Polestar 2Long Range", Alle_biler1$model), ]

filtered_biler$Year <- substr(filtered_biler$Dato, 1, 4)

result_biler <- filtered_biler[filtered_biler$Year == "2022" & 
                                 filtered_biler$Kilometer >= 30000 & 
                                 filtered_biler$Kilometer <= 40000, ]




# OPGAVE 2.2 - indsæt results i MySQL tabellen minebiler


# Opret forbindelse til MySQL
con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "Biler",          # Navn på databasen
  host = "localhost",        # Adresse til databasen
  port = 3306,               # Porten (standard er 3306 for MySQL)
  user = "root",       # Dit MySQL-brugernavn
  password = "Rev1l0 hceb" # Dit MySQL-password
)

# Vælg kun de nødvendige kolonner
colnames(Alle_biler)
data_to_insert <- Alle_biler %>%
  dplyr::select(carid = carid, model = model, Fra_Land = Fra_Land, link = link, price = price, 
                Kilometer = Kilometer, forhandlerID = forhandlerID, Scrapedate = Scrapedate, location = location,
                Dato = Dato)


# Loop til tabel 1: Biler
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    carid <- as.integer(row$carid)
    model <- as.character(row$model)
    Fra_Land <- as.character(row$Fra_Land)
    link <- as.character(row$link)
    forhandlerID <- as.character(row$forhandlerID)
    
    query <- sprintf(
      "INSERT INTO Biler (carid, model, Fra_Land, link, forhandlerID) 
      VALUES (%d, '%s', '%s', '%s', '%s')",
      carid, model, Fra_Land, link, forhandlerID
    )
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i Biler, række:", i, "\n", e$message, "\n")
  })
}

# Loop til tabel 2: Bilpriser
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    carid <- as.integer(row$carid)
    Dato <- as.Date(row$Dato)
    price <- as.integer(row$price)
    Kilometer <- as.integer(row$Kilometer)
    Scrapedate <- as_datetime(row$Scrapedate)
    
    query <- sprintf(
      "INSERT INTO Bilpriser (carid, Dato, price, Kilometer, Scrapedate) 
      VALUES (%d, '%s', %d, %d, '%s')",
      carid, Dato, price, Kilometer, Scrapedate
    )
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i Bilpriser, række:", i, "\n", e$message, "\n")
  })
}


# Loop til tabel 3: Forhandler
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    forhandlerID <- as.character(row$forhandlerID)
    location <- as.character(row$location)
    
    query <- sprintf(
      "INSERT INTO Forhandler (forhandlerID, location) 
      VALUES ('%s', '%s')",
      forhandlerID, location
    )
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i Forhandler, række:", i, "\n", e$message, "\n")
  })
}





