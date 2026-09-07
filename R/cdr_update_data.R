#' Refresh the Bundled CDR Datasets
#'
#' Re-downloads the indicator panel (World Bank WDI/WGI and V-Dem) and the
#' natural-resource-rent series, and reassembles the [indicators] and
#' [countries] data frames.  Latitudes are static and carried over from the
#' current [countries] dataset.  Intended for package maintenance -- run
#' it, inspect the result, then save it into `data-raw/`.
#'
#' @param codes ISO-2 codes.  `NULL` (default) uses the CDR country set.
#' @param start_year,end_year Year window for the indicator panel.  `NULL`
#'   uses [get_country_indicators()]'s default (the last five years).
#' @param path If given, a directory into which `indicators.rda` and
#'   `countries.rda` are written (xz-compressed).
#'
#' @return A list with `indicators` and `countries` data frames.
#'
#' @seealso [get_country_indicators()], [cdr_historical_panel()]
#'
#' @examples
#' \dontrun{
#'   fresh <- cdr_update_data()
#'   str(fresh$indicators)
#' }
#'
#' @export
cdr_update_data <- function(codes = NULL, start_year = NULL, end_year = NULL,
                            path = NULL) {
  if (!requireNamespace("wbstats", quietly = TRUE))
    stop("Package 'wbstats' is required.", call. = FALSE)

  base_cty <- CDREGM::countries
  if (is.null(codes)) {
    codes <- base_cty$code
    codes[is.na(codes)] <- "NA"
  }

  indicators <- if (is.null(start_year) && is.null(end_year)) {
    get_country_indicators(countries = codes)
  } else {
    cdr_historical_panel(
      start_year = start_year %||% 2000,
      end_year   = end_year,
      codes      = codes, refresh = TRUE)
  }

  nr <- .fetch_wb_indicators(
    countries = codes,
    codes     = c(natural_resources = "NY.GDP.TOTL.RT.ZS"),
    start_yr  = as.integer(format(Sys.Date(), "%Y")) - 6L,
    end_yr    = as.integer(format(Sys.Date(), "%Y")))
  nr <- nr[!is.na(nr$natural_resources), , drop = FALSE]
  nr <- nr[order(nr$iso2c, -nr$year), , drop = FALSE]
  nr <- nr[!duplicated(nr$iso2c), , drop = FALSE]      # latest per country

  countries <- base_cty
  countries$code[is.na(countries$code)] <- "NA"
  hit <- match(countries$code, nr$iso2c)
  countries$natural_resources <- ifelse(is.na(hit), countries$natural_resources,
                                        nr$natural_resources[hit])
  countries$natural_resources_year <- ifelse(is.na(hit),
                                             countries$natural_resources_year,
                                             nr$year[hit])

  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    e <- new.env()
    assign("indicators", indicators, e)
    assign("countries", countries, e)
    save(list = "indicators", envir = e,
         file = file.path(path, "indicators.rda"), compress = "xz")
    save(list = "countries", envir = e,
         file = file.path(path, "countries.rda"), compress = "xz")
  }

  list(indicators = indicators, countries = countries)
}
