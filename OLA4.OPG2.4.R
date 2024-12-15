library(RSelenium)
library(jsonlite)
library(httr)
library(pak)
library(rvest)
library(xml2)

# Send GET-anmodning for at hente siden
initial_res_hcab <- GET("https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB")

# Parse HTML for at finde tokenet
initial_html_hcab <- read_html(content(initial_res_hcab, "text"))

# Find tokenet i HTML'en
token <- initial_html_hcab %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(token)
}
#5tKvi8Tm4IlZDkzx6xSfdNFv9UxYJZNEe_8ZWXfRSZUQrgNFnFsTkFb9KNcoWTSj0fqdLlfkfGCAcCXZQzehLSNk5VY4QZ17RDncw1OvhHU1

hcab_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_hcab,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Tjek status på POST-anmodningen
if (status_code(hcab_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(hcab_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_hcab <- content(hcab_table_res, "text")
hcab_html <- read_html(content_hcab)

# Udtræk tabellen
hcab_table <- hcab_html %>%
  html_table(fill = TRUE)

if (length(hcab_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  hcab_dataframe <- as.data.frame(hcab_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(hcab_dataframe)
}



url_anholt <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_anholt <- GET(url_anholt)
initial_html_anholt <- read_html(content(initial_res_anholt, "text"))

# Find __RequestVerificationToken i HTML'en
token_anholt <- initial_html_anholt %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_anholt)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_anholt))
}

# Step 2: Send POST-anmodning med tokenet
anholt_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/ANHO"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_anholt,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_anholt
  ),
  encode = "form"
)

# Tjek status på POST-anmodningen
if (status_code(anholt_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(anholt_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_anholt <- content(anholt_table_res, "text")
anholt_html <- read_html(content_anholt)

# Udtræk tabellen som en dataframe
anholt_table <- anholt_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(anholt_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  anholt_dataframe <- as.data.frame(anholt_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(anholt_dataframe)
}


url_aarhus <- "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_aarhus <- GET(url_aarhus)
initial_html_aarhus <- read_html(content(initial_res_aarhus, "text"))

# Find __RequestVerificationToken i HTML'en
token_aarhus <- initial_html_aarhus %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_aarhus)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_aarhus))
}

# Step 2: Send POST-anmodning med tokenet
aarhus_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_aarhus,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_aarhus
  ),
  encode = "form"
)

# Tjek status på POST-anmodningen
if (status_code(aarhus_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(aarhus_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_aarhus <- content(aarhus_table_res, "text")
aarhus_html <- read_html(content_aarhus)

# Udtræk tabellen som en dataframe
aarhus_table <- aarhus_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(aarhus_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  aarhus_dataframe <- as.data.frame(aarhus_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(aarhus_dataframe)
}



url_risoe <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_risoe <- GET(url_risoe)
initial_html_risoe <- read_html(content(initial_res_risoe, "text"))

# Find __RequestVerificationToken i HTML'en
token_risoe <- initial_html_risoe %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_risoe)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_risoe))
}

# Step 2: Send POST-anmodning med tokenet
risoe_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/RISOE"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_risoe,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_risoe
  ),
  encode = "form"
)

# Tjek status på POST-anmodningen
if (status_code(risoe_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(risoe_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_risoe <- content(risoe_table_res, "text")
risoe_html <- read_html(content_risoe)

# Udtræk tabellen som en dataframe
risoe_table <- risoe_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(risoe_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  risoe_dataframe <- as.data.frame(risoe_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(risoe_dataframe)
}

