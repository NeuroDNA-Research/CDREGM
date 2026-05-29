## Script to prepare the countries dataset for the package.
## Latitudes sourced from: https://worldpopulationreview.com/country-rankings/latitude-by-country
## Country names and ISO-2 codes sourced from datasets/CDR.csv (raw source file).
## natural_resources / natural_resources_year sourced from the indicators dataset.

load("data/indicators.rda")

raw <- read.csv("datasets/CDR.csv", stringsAsFactors = FALSE, na.strings = "")
raw$code[raw$Country == "Namibia" & is.na(raw$code)] <- "NA"
raw$code[raw$Country == "Macedonia"]  <- "MK"
raw$code[raw$Country == "Mauritius"]  <- "MU"
country_list <- unique(raw[, c("Country", "code")])
names(country_list) <- c("country", "code")

# Latitude lookup (decimal degrees; negative = South hemisphere)
# Source: https://worldpopulationreview.com/country-rankings/latitude-by-country
lat_lookup <- c(
  "Albania" = 41.0,
  "Algeria" = 28.0,
  "Angola" = -12.5,
  "Argentina" = -34.0,
  "Armenia" = 40.0,
  "Australia" = -27.0,
  "Austria" = 47.33,
  "Azerbaijan" = 40.5,
  "Bangladesh" = 24.0,
  "Belgium" = 50.83,
  "Bolivia" = -17.0,
  "Bosnia and Herzegovina" = 44.0,
  "Botswana" = -22.0,
  "Brazil" = -10.0,
  "Bulgaria" = 43.0,
  "Canada" = 60.0,
  "Chile" = -30.0,
  "China" = 35.0,
  "Colombia" = 4.0,
  "Croatia" = 45.17,
  "Denmark" = 56.0,
  "Dominican Republic" = 19.0,
  "Ecuador" = -2.0,
  "Egypt" = 27.0,
  "El Salvador" = 13.83,
  "Estonia" = 59.0,
  "Finland" = 64.0,
  "France" = 46.0,
  "Germany" = 51.0,
  "Ghana" = 8.0,
  "Greece" = 39.0,
  "Hungary" = 47.0,
  "India" = 20.0,
  "Indonesia" = -5.0,
  "Iran" = 32.0,
  "Iraq" = 33.0,
  "Ireland" = 53.0,
  "Israel" = 31.5,
  "Italy" = 42.83,
  "Ivory Coast" = 8.0,
  "Jamaica" = 18.25,
  "Japan" = 36.0,
  "Jordan" = 31.0,
  "Kazakhstan" = 48.0,
  "Kenya" = 1.0,
  "Kyrgyzstan" = 41.0,
  "Latvia" = 57.0,
  "Lebanon" = 33.83,
  "Lithuania" = 56.0,
  "Malawi" = -13.5,
  "Malaysia" = 2.5,
  "Mauritius" = -20.28,
  "Mexico" = 23.0,
  "Moldova" = 47.0,
  "Mongolia" = 46.0,
  "Morocco" = 28.5,
  "Namibia" = -22.0,
  "Netherlands" = 52.52,
  "Nigeria" = 10.0,
  "North Macedonia" = 41.83,
  "Norway" = 62.0,
  "Oman" = 21.0,
  "Panama" = 9.0,
  "Peru" = -10.0,
  "Philippines" = 13.0,
  "Poland" = 52.0,
  "Portugal" = 39.5,
  "Romania" = 46.0,
  "Russia" = 60.0,
  "Saudi Arabia" = 25.0,
  "Serbia" = 44.0,
  "Singapore" = 1.37,
  "Slovakia" = 48.67,
  "Slovenia" = 46.12,
  "South Africa" = -29.0,
  "South Korea" = 37.0,
  "Spain" = 40.0,
  "Sweden" = 62.0,
  "Switzerland" = 47.0,
  "Thailand" = 15.0,
  "Trinidad and Tobago" = 11.0,
  "Turkey" = 39.0,
  "Uganda" = 1.0,
  "Ukraine" = 49.0,
  "United Kingdom" = 54.0,
  "United States" = 38.0,
  "Vietnam" = 16.17
)

# Map CDR country names to the lookup keys where they differ
name_map <- c(
  "Cote d'Ivoire" = "Ivory Coast",
  "Kazakstan"     = "Kazakhstan",
  "Korea, South"  = "South Korea",
  "Macedonia"     = "North Macedonia"
)

lookup_key <- country_list$country
lookup_key[lookup_key %in% names(name_map)] <- name_map[lookup_key[lookup_key %in% names(name_map)]]

countries <- data.frame(
  code     = country_list$code,
  country  = country_list$country,
  latitude = lat_lookup[lookup_key],
  stringsAsFactors = FALSE,
  row.names = NULL
)

# natural_resources is constant across years per country — take first non-NA row
nr_per_country <- do.call(rbind, lapply(split(indicators, indicators$iso2c), function(d) {
  i <- which(!is.na(d$natural_resources))
  if (length(i) == 0L)
    data.frame(iso2c = d$iso2c[1L], natural_resources = NA_real_,
               natural_resources_year = NA_integer_, stringsAsFactors = FALSE)
  else
    data.frame(iso2c = d$iso2c[i[1L]], natural_resources = d$natural_resources[i[1L]],
               natural_resources_year = d$natural_resources_year[i[1L]],
               stringsAsFactors = FALSE)
}))
rownames(nr_per_country) <- NULL

countries <- merge(countries, nr_per_country, by.x = "code", by.y = "iso2c", all.x = TRUE)

countries <- countries[order(countries$country), ]
rownames(countries) <- NULL

missing <- countries$country[is.na(countries$latitude)]
if (length(missing) > 0L)
  warning("Missing latitudes for: ", paste(missing, collapse = ", "))

save(countries, file = "data/countries.rda", compress = "xz")
