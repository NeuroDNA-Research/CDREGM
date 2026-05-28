#' Fetch Country-Level Economic and Governance Indicators
#'
#' Retrieves GDP, stock market capitalisation, democracy index, corruption
#' index, natural resources rents, and population from live web sources
#' (World Bank WDI/WGI and V-Dem).  When a specific `year` is requested,
#' indicators whose most-recent observation pre-dates that year are
#' back-filled from the nearest available prior year (within five years),
#' and the actual observation year is recorded in a companion `*_year`
#' column.
#'
#' @param countries Character vector of ISO 2-letter country codes.
#'   `NULL` (default) uses the 79 countries in the built-in [CDR] dataset.
#' @param year Integer.  Reference year to retrieve.  `NULL` (default) returns
#'   a panel covering the five most recently available years for each
#'   indicator.
#' @param indicators Character vector selecting which indicators to fetch.
#'   Any subset of `c("gdp", "gdp_per_capita", "capitalization",
#'   "democracy", "corruption", "population")`.
#'   Defaults to all six.
#'
#' @return A `data.frame` with columns `iso2c`, `year`, and one column per
#'   requested indicator.  `capitalization` also gets a companion
#'   `capitalization_year` column showing the actual observation year when
#'   back-filled.  The data frame carries a `sources` attribute — a named
#'   character vector documenting the origin of each indicator column.
#'
#' @section Data sources:
#' \describe{
#'   \item{gdp}{World Bank WDI indicator \code{NY.GDP.MKTP.CD} — GDP,
#'     current US\$. Retrieved via \pkg{wbstats}.}
#'   \item{gdp_per_capita}{World Bank WDI indicator \code{NY.GDP.PCAP.CD} —
#'     GDP per capita, current US\$. Retrieved via \pkg{wbstats}.}
#'   \item{capitalization}{World Bank WDI indicator \code{CM.MKT.LCAP.GD.ZS}
#'     — market capitalisation of listed domestic companies as \% of GDP.
#'     Retrieved via \pkg{wbstats}; filled from nearest prior year (≤ 5 yr)
#'     when the requested year is unavailable.}
#'   \item{democracy}{V-Dem electoral democracy index \code{v2x_polyarchy}
#'     (0 = least democratic, 1 = most democratic).  Retrieved via the
#'     \pkg{vdemdata} package (install from GitHub:
#'     \code{remotes::install_github("vdeminstitute/vdemdata")}).}
#'   \item{corruption}{World Bank WGI indicator \code{GOV_WGI_CC.EST} — Control
#'     of Corruption governance estimate (–2.5 to +2.5; higher = less corrupt).
#'     Retrieved via \pkg{wbstats}.}
#'   \item{population}{World Bank WDI indicator \code{SP.POP.TOTL} — total
#'     population.  Retrieved via \pkg{wbstats}.}
#' }
#'
#' @references
#' World Bank (2024). *World Development Indicators*.
#' \url{https://databank.worldbank.org/source/world-development-indicators}
#'
#' World Bank (2024). *Worldwide Governance Indicators*.
#' \url{https://info.worldbank.org/governance/wgi/}
#'
#' Coppedge, M. et al. (2024). *V-Dem Dataset v14*.
#' Varieties of Democracy (V-Dem) Project.
#' \url{https://www.v-dem.net/}
#'
#' @examples
#' \dontrun{
#' # Single year, three countries
#' d <- get_country_indicators(countries = c("US", "DE", "BR"), year = 2022)
#' str(d)
#' attr(d, "sources")
#'
#' # Last-5-years panel for the default CDR country set
#' panel <- get_country_indicators()
#' table(panel$year)
#' }
#'
#' @export
get_country_indicators <- function(
    countries  = NULL,
    year       = NULL,
    indicators = c("gdp", "gdp_per_capita", "capitalization",
                   "democracy", "corruption", "population")
) {
  indicators <- match.arg(
    indicators,
    choices  = c("gdp", "gdp_per_capita", "capitalization",
                 "democracy", "corruption", "population"),
    several.ok = TRUE
  )

  if (is.null(countries)) {
    countries <- CDR$code
    # Namibia's ISO2 code "NA" is stored as R's NA in the CDR dataset
    countries[is.na(countries)] <- "NA"
  }
  countries <- unique(as.character(countries))

  # Determine year window
  current_yr  <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(year)) {
    start_yr <- current_yr - 6L   # extra buffer for lag-prone indicators
    end_yr   <- current_yr
    single   <- FALSE
  } else {
    year   <- as.integer(year)
    start_yr <- year - 5L          # lookback window for gap-filling
    end_yr   <- year
    single   <- TRUE
  }

  # --- World Bank indicators ------------------------------------------------
  wb_map <- c(
    gdp              = "NY.GDP.MKTP.CD",
    gdp_per_capita   = "NY.GDP.PCAP.CD",
    capitalization   = "CM.MKT.LCAP.GD.ZS",
    corruption       = "GOV_WGI_CC.EST",
    population       = "SP.POP.TOTL"
  )
  wb_needed <- wb_map[intersect(names(wb_map), indicators)]

  wb_df <- NULL
  if (length(wb_needed) > 0L) {
    wb_df <- .fetch_wb_indicators(
      countries  = countries,
      codes      = wb_needed,
      start_yr   = start_yr,
      end_yr     = end_yr
    )
  }

  # --- GFDD capitalization fallback -----------------------------------------
  # WDI CM.MKT.LCAP.GD.ZS has gaps for many frontier/non-reporting markets;
  # GFDD.DM.01 is sourced directly from exchanges and fills most of them.
  # GFDD data can lag WDI by several years, so fetch with a 5-yr wider window
  # and use nearest-year fill rather than exact-year merge.
  if ("capitalization" %in% indicators && !is.null(wb_df) &&
      "capitalization" %in% names(wb_df) && any(is.na(wb_df$capitalization))) {
    gfdd_df <- .fetch_gfdd_capitalization(countries, start_yr - 5L, end_yr)
    if (!is.null(gfdd_df)) {
      for (i in seq_len(nrow(wb_df))) {
        if (!is.na(wb_df$capitalization[i])) next
        iso  <- wb_df$iso2c[i]
        yr   <- wb_df$year[i]
        pool <- gfdd_df[gfdd_df$iso2c == iso &
                        gfdd_df$year >= (yr - 5L) &
                        gfdd_df$year <= yr &
                        !is.na(gfdd_df$cap_gfdd), ]
        if (nrow(pool) == 0L) next
        wb_df$capitalization[i] <- pool$cap_gfdd[which.max(pool$year)]
      }
    }
  }

  # --- V-Dem democracy ------------------------------------------------------
  vdem_df <- NULL
  if ("democracy" %in% indicators) {
    vdem_df <- .fetch_vdem_democracy(
      countries = countries,
      start_yr  = start_yr,
      end_yr    = end_yr
    )
  }

  # --- Merge ----------------------------------------------------------------
  if (!is.null(wb_df) && !is.null(vdem_df)) {
    result <- merge(wb_df, vdem_df, by = c("iso2c", "year"), all = TRUE)
  } else if (!is.null(wb_df)) {
    result <- wb_df
  } else {
    result <- vdem_df
  }

  # Filter to the 79 CDR countries (iso2c match)
  result <- result[result$iso2c %in% countries, , drop = FALSE]

  # --- Gap-fill lagged indicators ------------------------------------------
  lagged <- intersect(c("capitalization"), indicators)
  for (col in lagged) {
    if (col %in% names(result)) {
      filled <- .fill_latest(result, col, id_col = "iso2c", yr_col = "year")
      result[[col]]                    <- filled$value
      result[[paste0(col, "_year")]]   <- filled$actual_year
    }
  }

  # --- Single-year slice ----------------------------------------------------
  if (single) {
    result <- result[result$year == year, , drop = FALSE]
  } else {
    # Keep last 5 complete years
    available_yrs <- sort(unique(result$year), decreasing = TRUE)
    keep_yrs      <- available_yrs[seq_len(min(5L, length(available_yrs)))]
    result        <- result[result$year %in% keep_yrs, , drop = FALSE]
  }

  result <- result[order(result$iso2c, result$year), , drop = FALSE]
  rownames(result) <- NULL

  # Reorder columns: id cols first, then requested indicators
  id_cols  <- c("iso2c", "year")
  ind_cols <- unlist(lapply(indicators, function(i) {
    extra <- paste0(i, "_year")
    c(i, if (extra %in% names(result)) extra)
  }))
  col_order <- c(id_cols, ind_cols)
  col_order <- col_order[col_order %in% names(result)]
  result    <- result[, col_order, drop = FALSE]

  attr(result, "sources") <- .indicator_sources()[indicators]
  result
}

# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

.fetch_wb_indicators <- function(countries, codes, start_yr, end_yr,
                                 batch_size = 40L) {
  # Split into batches to avoid URL-length timeouts on large country lists
  batches <- split(countries, ceiling(seq_along(countries) / batch_size))
  raw <- do.call(rbind, lapply(batches, function(batch) {
    wbstats::wb_data(
      indicator   = codes,
      country     = batch,
      start_date  = start_yr,
      end_date    = end_yr,
      return_wide = TRUE
    )
  }))

  # wb_data returns iso3c + iso2c + country + date + indicator columns
  raw$year  <- as.integer(raw$date)
  raw$date  <- NULL

  # wbstats returns Namibia's iso2c as logical NA rather than the string "NA"
  if ("iso3c" %in% names(raw))
    raw$iso2c[is.na(raw$iso2c) & raw$iso3c == "NAM"] <- "NA"

  # Rename WB indicator codes to friendly names
  for (nm in names(codes)) {
    code <- codes[[nm]]
    if (code %in% names(raw) && nm != code) {
      names(raw)[names(raw) == code] <- nm
    }
  }

  # Drop WB metadata columns we don't need
  drop <- intersect(c("iso3c", "unit", "obs_status", "footnote",
                      "last_updated"), names(raw))
  raw[, setdiff(names(raw), drop), drop = FALSE]
}

.fetch_gfdd_capitalization <- function(countries, start_yr, end_yr,
                                       batch_size = 40L) {
  tryCatch({
    batches <- split(countries, ceiling(seq_along(countries) / batch_size))
    raw <- do.call(rbind, lapply(batches, function(batch) {
      wbstats::wb_data(
        indicator   = c(cap_gfdd = "GFDD.DM.01"),
        country     = batch,
        start_date  = start_yr,
        end_date    = end_yr,
        return_wide = TRUE
      )
    }))
    raw$year <- as.integer(raw$date)
    raw$date <- NULL
    if ("iso3c" %in% names(raw))
      raw$iso2c[is.na(raw$iso2c) & raw$iso3c == "NAM"] <- "NA"
    drop <- intersect(c("iso3c", "country", "unit", "obs_status",
                        "footnote", "last_updated"), names(raw))
    raw[, setdiff(names(raw), drop), drop = FALSE]
  }, error = function(e) {
    message("GFDD capitalization fallback failed: ", conditionMessage(e))
    NULL
  })
}

.fetch_vdem_democracy <- function(countries, start_yr, end_yr) {
  if (!requireNamespace("vdemdata", quietly = TRUE)) {
    message(
      "Package 'vdemdata' is needed for democracy data but is not installed.\n",
      "Install with: remotes::install_github(\"vdeminstitute/vdemdata\")\n",
      "Returning data without the 'democracy' column."
    )
    return(NULL)
  }

  vdem <- vdemdata::vdem  # the full V-Dem data object

  # V-Dem uses ISO country_text_id (3-letter) and country_id; we need iso2c.
  # The dataset contains a 'country_text_id' column (ISO3) which we map via
  # an internal lookup derived from the World Bank country list.
  wb_meta <- wbstats::wb_countries()
  iso_map  <- stats::setNames(wb_meta$iso2c, wb_meta$iso3c)

  sub <- vdem[
    vdem$year >= start_yr & vdem$year <= end_yr,
    c("country_text_id", "year", "v2x_polyarchy"),
    drop = FALSE
  ]

  sub$iso2c     <- iso_map[sub$country_text_id]
  sub$democracy <- sub$v2x_polyarchy
  sub           <- sub[!is.na(sub$iso2c) & sub$iso2c %in% countries, ]
  sub[, c("iso2c", "year", "democracy"), drop = FALSE]
}

# For each country, replace NA in `col` with the most-recent non-NA value
# within `max_lag` years of each row's year.  Returns a list with:
#   $value       — filled numeric vector
#   $actual_year — year of the observation used (NA when no fill available)
.fill_latest <- function(df, col, id_col = "iso2c", yr_col = "year",
                         max_lag = 5L) {
  value       <- df[[col]]
  actual_year <- ifelse(is.na(value), NA_integer_, df[[yr_col]])

  for (i in seq_len(nrow(df))) {
    if (!is.na(value[i])) next
    iso  <- df[[id_col]][i]
    yr   <- df[[yr_col]][i]
    pool <- df[df[[id_col]] == iso &
               df[[yr_col]] >= (yr - max_lag) &
               df[[yr_col]] <  yr &
               !is.na(df[[col]]), ]
    if (nrow(pool) == 0L) next
    best <- pool[which.max(pool[[yr_col]]), ]
    value[i]       <- best[[col]]
    actual_year[i] <- best[[yr_col]]
  }

  list(value = value, actual_year = actual_year)
}

.indicator_sources <- function() {
  c(
    gdp =
      "World Bank WDI NY.GDP.MKTP.CD (GDP, current USD) via wbstats",
    gdp_per_capita =
      "World Bank WDI NY.GDP.PCAP.CD (GDP per capita, current USD) via wbstats",
    capitalization =
      "World Bank WDI CM.MKT.LCAP.GD.ZS (market cap % of GDP) via wbstats; GFDD.DM.01 used as fallback where WDI is missing; back-filled from nearest prior year if still missing",
    democracy =
      "V-Dem v2x_polyarchy (electoral democracy index, 0-1) via vdemdata package",
    corruption =
      "World Bank WGI GOV_WGI_CC.EST (Control of Corruption, -2.5 to +2.5) via wbstats",
    population =
      "World Bank WDI SP.POP.TOTL (total population) via wbstats"
  )
}
