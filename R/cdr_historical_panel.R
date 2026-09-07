#' Fetch a Long Historical CDR Indicator Panel
#'
#' Downloads annual GDP, market capitalization, democracy, corruption, and
#' population for the CDR countries over a multi-year window and caches the
#' result locally, so the time-invariance analyses can be run on a long
#' series rather than the few years shipped in [indicators].
#'
#' @param start_year First year to fetch.  Default `2000`.
#' @param end_year Last year.  `NULL` (default) uses the current year.
#' @param codes ISO-2 codes.  `NULL` (default) uses the CDR country set.
#' @param cache_dir Directory for the cache file.  Default a per-session
#'   temporary directory.
#' @param refresh If `TRUE`, ignore any cache and re-download.
#'
#' @return A data frame with the same columns as [indicators] covering
#'   `start_year:end_year`, carrying a `sources` attribute.  The cache
#'   file path is in `attr(, "cache")`.
#'
#' @seealso [get_country_indicators()], [cdr_update_data()]
#'
#' @examples
#' \dontrun{
#'   hp <- cdr_historical_panel(1995)
#'   table(hp$year)
#' }
#'
#' @export
cdr_historical_panel <- function(start_year = 2000, end_year = NULL,
                                 codes = NULL, cache_dir = tempdir(),
                                 refresh = FALSE) {
  if (!requireNamespace("wbstats", quietly = TRUE))
    stop("Package 'wbstats' is required.", call. = FALSE)
  start_year <- as.integer(start_year)
  end_year   <- as.integer(end_year %||% format(Sys.Date(), "%Y"))
  if (is.null(codes)) {
    codes <- CDREGM::countries$code
    codes[is.na(codes)] <- "NA"
  }

  cache <- file.path(cache_dir,
                     sprintf("cdr_historical_%d_%d.rds", start_year, end_year))
  if (!refresh && file.exists(cache)) {
    out <- readRDS(cache)
    attr(out, "cache") <- cache
    return(out)
  }

  wb <- .fetch_wb_indicators(
    countries = codes,
    codes     = c(gdp = "NY.GDP.MKTP.CD", capitalization = "CM.MKT.LCAP.GD.ZS",
                  corruption = "GOV_WGI_CC.EST", population = "SP.POP.TOTL"),
    start_yr  = start_year, end_yr = end_year)
  vd <- .fetch_vdem_democracy(codes, start_year, end_year)

  out <- if (!is.null(vd))
    merge(wb, vd, by = c("iso2c", "year"), all = TRUE) else wb
  out <- out[out$iso2c %in% codes, , drop = FALSE]
  keep <- c("iso2c", "year", "gdp", "capitalization", "democracy",
            "corruption", "population")
  out <- out[, intersect(keep, names(out)), drop = FALSE]
  out <- out[order(out$iso2c, out$year), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "sources") <- .indicator_sources()[
    intersect(names(.indicator_sources()), names(out))]

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, cache)
  attr(out, "cache") <- cache
  out
}
