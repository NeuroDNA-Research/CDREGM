# Data source reference for get_country_indicators()
#
# This script documents every indicator used by the function, shows
# direct API calls, and notes known data-lag patterns.  It is NOT
# sourced by the package — it is a living reference for maintainers.
#
# Prerequisites:
#   install.packages("wbstats")
#   remotes::install_github("vdeminstitute/vdemdata")

library(wbstats)

# ---------------------------------------------------------------------------
# WORLD BANK WDI / WGI INDICATORS  (fetched via wbstats::wb_data)
# ---------------------------------------------------------------------------
#
# All indicators below are pulled in a single wb_data() call inside
# .fetch_wb_indicators().  The World Bank API is free, no key required.
# Data is updated quarterly; the base URL is:
#   https://api.worldbank.org/v2/
#
# Indicator reference:
#   https://databank.worldbank.org/source/world-development-indicators
#   https://info.worldbank.org/governance/wgi/
#
# Indicator codes and typical availability:
#
# NY.GDP.MKTP.CD   — GDP, current US$
#   Coverage: 200+ countries | Annual | Typically 1960–present (2023)
#
# NY.GDP.PCAP.CD   — GDP per capita, current US$
#   Coverage: 200+ countries | Annual | Typically 1960–present (2023)
#
# CM.MKT.LCAP.GD.ZS — Market capitalisation of listed domestic companies (% of GDP)
#   Coverage: ~120 countries | Annual | Typically 1990–present (2022)
#   NOTE: Many low-income countries and small states have incomplete series.
#         get_country_indicators() back-fills up to 5 years.
#
# GOV_WGI_CC.EST   — Control of Corruption: Governance estimate  [WGI source 3]
#   Coverage: 200+ countries | Annual | 1996–present (2023)
#   Scale: -2.5 (most corrupt) to +2.5 (least corrupt)
#   Source: World Bank Worldwide Governance Indicators
#   NOTE: The legacy code CC.EST no longer works via the WB API; use GOV_WGI_CC.EST.
#
# NY.GDP.TOTL.RT.ZS — Total natural resources rents (% of GDP)
#   Coverage: 200+ countries | Annual | Typically 1970–present (2022)
#   NOTE: Lags 1–2 years; get_country_indicators() back-fills up to 5 years.
#
# SP.POP.TOTL      — Population, total
#   Coverage: 200+ countries | Annual | Typically 1960–present (2023)

# Example: fetch all WB indicators for the 79 CDR countries, 2019-2023
wb_indicators <- c(
  gdp               = "NY.GDP.MKTP.CD",
  gdp_per_capita    = "NY.GDP.PCAP.CD",
  capitalization    = "CM.MKT.LCAP.GD.ZS",
  corruption        = "GOV_WGI_CC.EST",
  natural_resources = "NY.GDP.TOTL.RT.ZS",
  population        = "SP.POP.TOTL"
)

wb_raw <- wbstats::wb_data(
  indicator   = wb_indicators,
  country     = CDREGM::CDR$code,   # 79 ISO2 codes
  start_date  = 2019,
  end_date    = 2024,
  return_wide = TRUE
)
head(wb_raw)

# ---------------------------------------------------------------------------
# V-DEM DEMOCRACY INDEX  (fetched via vdemdata package)
# ---------------------------------------------------------------------------
#
# Install once (not on CRAN):
#   remotes::install_github("vdeminstitute/vdemdata")
#
# The package ships the complete V-Dem dataset as the object `vdemdata::vdem`.
# No API key or internet connection required after installation.
# Dataset size: ~500 MB (use Suggests, not Imports).
#
# Reference:
#   Coppedge, M. et al. (2024). V-Dem Dataset v14.
#   Varieties of Democracy (V-Dem) Project. https://www.v-dem.net/
#
# Indicator used:
#   v2x_polyarchy — Electoral Democracy Index
#   Scale: 0 (least democratic) to 1 (most democratic)
#   Coverage: 182 countries | Annual | 1789–2023
#
# The get_country_indicators() function maps V-Dem's ISO 3-letter
# country_text_id to ISO 2-letter codes using the World Bank country
# metadata (wbstats::wbcountries()).

library(vdemdata)

# Inspect available columns (over 4000 indicators):
# names(vdemdata::vdem)

# Example: extract v2x_polyarchy for 2019-2023
vdem_sub <- vdemdata::vdem[
  vdemdata::vdem$year >= 2019 & vdemdata::vdem$year <= 2023,
  c("country_text_id", "year", "v2x_polyarchy")
]
head(vdem_sub)

# ---------------------------------------------------------------------------
# DATA LAG SUMMARY
# ---------------------------------------------------------------------------
#
# Indicator              Typical last year   Lag
# ─────────────────────  ──────────────────  ────
# gdp                    2023                0-1 yr
# gdp_per_capita         2023                0-1 yr
# capitalization         2022                1-2 yr
# democracy (V-Dem)      2023                0-1 yr
# corruption (WGI)       2023                0-1 yr
# natural_resources      2022                1-2 yr
# population             2023                0-1 yr
#
# For indicators with 1-2 year lags, get_country_indicators() fills missing
# values with the nearest non-NA observation within the preceding 5 years
# and records the actual observation year in a *_year companion column.
