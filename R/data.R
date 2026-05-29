#' Country Indicator Panel Dataset
#'
#' Annual panel of economic and governance indicators for the 79 CDR countries,
#' covering 2021–2025.  Downloaded from the World Bank WDI/WGI APIs and the
#' V-Dem project; see `attr(indicators, "sources")` for full provenance.
#' Refresh with `source("data-raw/fetch_indicators.R")`.
#'
#' @format A data frame with 395 rows and 12 variables:
#' \describe{
#'   \item{iso2c}{ISO 2-letter country code (character)}
#'   \item{year}{Reference year (integer)}
#'   \item{gdp}{GDP, current USD — WB WDI `NY.GDP.MKTP.CD` (numeric)}
#'   \item{gdp_per_capita}{GDP per capita, current USD — WB WDI `NY.GDP.PCAP.CD` (numeric)}
#'   \item{capitalization}{Market capitalisation of listed companies as \% of GDP —
#'     WB WDI `CM.MKT.LCAP.GD.ZS`; back-filled from nearest prior year when missing (numeric)}
#'   \item{capitalization_year}{Actual observation year for `capitalization` when back-filled (integer)}
#'   \item{capitalization_pc}{Per-capita market capitalisation in USD — derived as
#'     `gdp_per_capita * capitalization / 100` (numeric)}
#'   \item{capitalization_pc_year}{Actual observation year for `capitalization_pc`,
#'     same as `capitalization_year` (integer)}
#'   \item{democracy}{V-Dem electoral democracy index, 0–1 — `v2x_polyarchy` (numeric)}
#'   \item{corruption}{WB WGI Control of Corruption estimate, –2.5 to +2.5 —
#'     `GOV_WGI_CC.EST`; higher = less corrupt (numeric)}
#'   \item{population}{Total population — WB WDI `SP.POP.TOTL` (numeric)}
#' }
#' @source
#' World Bank WDI: \url{https://databank.worldbank.org/source/world-development-indicators}
#'
#' World Bank WGI: \url{https://info.worldbank.org/governance/wgi/}
#'
#' V-Dem Dataset v14: \url{https://www.v-dem.net/}
"indicators"

#' Country Reference Dataset
#'
#' ISO-2 country codes, names, and latitude (in decimal degrees) for the 79
#' countries in the CDR research dataset.  Latitudes are sourced from
#' \url{https://worldpopulationreview.com/country-rankings/latitude-by-country}.
#'
#' @format A data frame with 79 rows and 5 variables:
#' \describe{
#'   \item{code}{ISO 2-letter country code (character)}
#'   \item{country}{Country name (character)}
#'   \item{latitude}{Latitude in decimal degrees; negative values indicate the
#'     Southern Hemisphere (numeric)}
#'   \item{natural_resources}{Total natural resources rents as \% of GDP —
#'     World Bank WDI \code{NY.GDP.TOTL.RT.ZS}; back-filled from nearest prior
#'     year when the reference year is unavailable (numeric)}
#'   \item{natural_resources_year}{Actual observation year for
#'     \code{natural_resources} when back-filled (integer)}
#' }
#' @source
#' Latitudes: \url{https://worldpopulationreview.com/country-rankings/latitude-by-country}
#'
#' Natural resources: World Bank WDI
#' \url{https://databank.worldbank.org/source/world-development-indicators}
"countries"

#' Get the file path to a country flag image
#'
#' Returns the absolute path to the PNG flag bundled with the package for the
#' given ISO 2-letter country code.  Flags are available for all 79 CDR
#' countries (see `CDR$code`).
#'
#' @param code Character. ISO 2-letter country code (e.g. `"US"`, `"DE"`).
#'   See `countries$code` for the full list of available codes.
#' @return A character string with the full path to the PNG file, or `NA` if
#'   no flag is available for the requested code.
#' @examples
#' flag_path("US")
#' flag_path("DE")
#' @export
flag_path <- function(code) {
  stopifnot(is.character(code), length(code) == 1L)
  p <- system.file("flags", paste0(code, ".png"), package = "CDREGM")
  if (nzchar(p)) p else NA_character_
}
