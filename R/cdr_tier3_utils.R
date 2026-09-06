# ---- Tier 3 internal helpers ----------------------------------------------

# Published pre-rounding CDR coefficients from Ridley & Llaugel (2018),
# "The Four-Dimensional Scientific CDR Economic Growth Model" (year 2014,
# n = 79).  Used as a fallback / reference when a model cannot be estimated
# reliably from the bundled short panel.
.cdr_published <- function() {
  list(
    # OLS (CDRN) second-stage-comparable capital coefficient
    ols_c   = 1.534346,
    # 2SLS second stage
    tsls    = c("(Intercept)" = -0.00051,
                C_std = 1.295617, D_std = 0.116963, R_std = 0.275395,
                CDR   = -0.98133, N_std = 0.388146),
    sigma_g = 0.208513,
    n       = 79L,
    entrepreneur_share = 0.85
  )
}

# The regression terms shared by every Tier 2/3 model.
.cdr_terms <- function() c("C_std", "D_std", "R_std", "CDR", "N_std")

# Cross-section for a single calendar year, carrying the within-country
# growth rate `g` (needs the prior year to exist in `panel`).
.year_cross_section <- function(panel, yr) {
  g_panel <- .gdp_growth(panel)
  g_panel[!is.na(g_panel$g) & g_panel$year == yr, , drop = FALSE]
}

# Predict `g` from a plain coefficient vector and a data frame holding the
# standardized columns (rebuilds CDR from C_std * D_std * R_std).
.cdr_predict_g <- function(coef, df) {
  df$CDR <- df$C_std * df$D_std * df$R_std
  b0 <- if ("(Intercept)" %in% names(coef)) coef[["(Intercept)"]] else 0
  b0 +
    coef[["C_std"]] * df$C_std +
    coef[["D_std"]] * df$D_std +
    coef[["R_std"]] * df$R_std +
    coef[["CDR"]]   * df$CDR +
    coef[["N_std"]] * df$N_std
}

# Resolve a country identifier (ISO-2 code or name, case-insensitive) to a
# single row of `cs`.  Stops with an informative message otherwise.
.cdr_match_country <- function(cs, country) {
  hit <- which(toupper(cs$iso2c) == toupper(country) |
               tolower(cs$country) == tolower(country))
  if (length(hit) == 0L)
    stop("Country '", country, "' not found in the panel.", call. = FALSE)
  if (length(hit) > 1L)
    stop("Country '", country, "' matches multiple rows.", call. = FALSE)
  cs[hit, , drop = FALSE]
}
