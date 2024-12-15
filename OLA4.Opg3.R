# Indlæs nødvendige pakker
library(dplyr)
library(stringr)
library(ggplot2)
library(dplyr)

# Læs alle .gz-filer og kombiner dem til én log
log_files <- list.files(pattern = "\\.gz$")
all_logs <- unlist(lapply(log_files, function(file) readLines(gzfile(file))))

# Parse log-linjer med regex og konverter til dataframe
log_pattern <- '^(\\S+) - - \\[(.+?)\\] "(.*?)" (\\d+) (\\d+|\\-)'
parsed_logs <- str_match(all_logs, log_pattern)
log_df <- data.frame(
  ip = parsed_logs[, 2], 
  timestamp = as.POSIXct(parsed_logs[, 3], format = "%d/%b/%Y:%H:%M:%S %z", tz = "GMT"),
  request = parsed_logs[, 4], 
  status = as.numeric(parsed_logs[, 5]),
  stringsAsFactors = FALSE
)

# Konverter timestamp til datoformat
log_df$date <- as.Date(log_df$timestamp)

# Find de 10 mest aktive IP'er
top_ips <- log_df %>%
  group_by(ip) %>%
  summarise(total_requests = n()) %>%
  arrange(desc(total_requests)) %>%
  head(10)

# Plot de mest aktive IP'er
ggplot(top_ips, aes(x = reorder(ip, -total_requests), y = total_requests)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_requests), vjust = -0.3, color = "black") +  # Tilføj mærkater
  labs(title = "192.0.102.40 er den mest aktive ip-adresse", x = "IP-adresse", y = "Antal requests i alt") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Filtrér og find de 404-fejl
error_404_summary <- log_df %>%
  filter(status == 404) %>%
  group_by(ip) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Identificer mistænkelige 404-requests
suspicious_requests <- log_df %>%
  filter(status == 404) %>%
  group_by(request) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Find unikke URL'er med lavt antal requests (status 200)
suspicious_200 <- log_df %>%
  filter(status == 200) %>%
  group_by(request) %>%
  summarise(count = n()) %>%
  filter(count < 5) %>%
  arrange(count)


### Summerer log_df således at alle date, ip, request og request_count bliver samlet pr. døgn
# Gruppér data efter dato og IP, og inkludér requests
summarized_df <- log_df %>%
  group_by(date, ip) %>%                                # Gruppér efter dato og IP
  summarize(
    request_count = n(),                                # Tæl antallet af requests
    requests = paste(unique(request), collapse = ", "), # Kombinér unikke requests
    .groups = "drop"
  ) %>%
  arrange(date, desc(request_count))                   # Sortér efter dato og derefter request_count



### Manuelt samler data pr. døgn
# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_1 <- log_df %>%
  filter(date == "2023-10-26") %>%       # Filtrér for 2023-10-26
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_2 <- log_df %>%
  filter(date == "2023-10-27") %>%       # Filtrér for 2023-10-27
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_3 <- log_df %>%
  filter(date == "2023-10-28") %>%       # Filtrér for 2023-10-28
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_4 <- log_df %>%
  filter(date == "2023-10-29") %>%       # Filtrér for 2023-10-29
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_5 <- log_df %>%
  filter(date == "2023-10-30") %>%       # Filtrér for 2023-10-30
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_6 <- log_df %>%
  filter(date == "2023-10-31") %>%       # Filtrér for 2023-10-31
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_7 <- log_df %>%
  filter(date == "2023-11-01") %>%       # Filtrér for 2023-11-01
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_8 <- log_df %>%
  filter(date == "2023-11-02") %>%       # Filtrér for 2023-11-02
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_9 <- log_df %>%
  filter(date == "2023-11-03") %>%       # Filtrér for 2023-11-03
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_10 <- log_df %>%
  filter(date == "2023-11-04") %>%       # Filtrér for 2023-11-04
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_11 <- log_df %>%
  filter(date == "2023-11-05") %>%       # Filtrér for 2023-11-05
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_12 <- log_df %>%
  filter(date == "2023-11-06") %>%       # Filtrér for 2023-11-06
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

# Filtrér data for en specifik dato, gruppér og sorter efter antal forespørgsler
Sum_døgn_13 <- log_df %>%
  filter(date == "2023-11-07") %>%       # Filtrér for 2023-11-07
  group_by(date, ip) %>%                 # Gruppér efter dato og IP
  summarize(
    request_count = n(),                # Tæl forespørgsler
    requests = paste(unique(request), collapse = ", "), # Saml requests som en tekststreng
    .groups = "drop"                    # Drop gruppering
  ) %>% 
  arrange(desc(request_count))           # Sortér efter antal requests

