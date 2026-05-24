# Downloads the country indicator panel and saves it as data/indicators.rda.
# Run this script manually to refresh the bundled dataset:
#   source("data-raw/fetch_indicators.R")
#
# Requires: wbstats, vdemdata (see data-raw/indicator_sources.R for details)

pkgload::load_all()

message("Fetching indicators for CDR countries (2019-", format(Sys.Date(), "%Y"), ")...")

indicators <- get_country_indicators()   # all 79 countries, last 5 years

message("Downloaded ", nrow(indicators), " rows x ", ncol(indicators), " columns.")
message("Years: ", paste(sort(unique(indicators$year)), collapse = ", "))
message("Countries: ", length(unique(indicators$iso2c)))
message("Source: ", paste(names(attr(indicators, "sources")), collapse = ", "))

save(indicators, file = "data/indicators.rda", compress = "xz")
message("Saved to data/indicators.rda")
