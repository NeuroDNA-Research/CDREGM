# ---- internal helpers -------------------------------------------------------

# Compute annualised log GDP-per-capita growth within each country.
.gdp_growth <- function(panel) {
  panel <- panel[order(panel$iso2c, panel$year), ]
  panel$gdp_pc     <- panel$gdp / panel$population
  panel$log_gdp_pc <- log(panel$gdp_pc)
  panel$g <- stats::ave(
    panel$log_gdp_pc, panel$iso2c,
    FUN = function(x) c(NA_real_, diff(x))
  )
  panel
}

# Aggregate a panel to a cross-section by averaging CDR columns per country.
.cross_section <- function(panel) {
  panel <- .gdp_growth(panel)
  keep  <- !is.na(panel$g)
  stats::aggregate(
    cbind(g, C_std, D_std, R_std, N_std, L_std, CDR) ~
      iso2c + country,
    data = panel[keep, ],
    FUN  = function(x) mean(x, na.rm = TRUE)
  )
}

# Partial R² of `drop_var` in `fit` = (SSR_reduced - SSR_full) / SSR_reduced.
.partial_r2 <- function(fit, drop_var) {
  fml <- stats::update(
    stats::formula(fit),
    stats::as.formula(paste(". ~ . -", drop_var))
  )
  fit_r       <- stats::lm(fml, data = stats::model.frame(fit))
  ssr_full    <- sum(stats::residuals(fit)^2)
  ssr_reduced <- sum(stats::residuals(fit_r)^2)
  (ssr_reduced - ssr_full) / ssr_reduced
}


# ---- cdr_standardize --------------------------------------------------------

#' Min-Max Standardize a Numeric Vector
#'
#' Rescales `x` to \[0, 1\] using `(x - lo) / (hi - lo)`, clamped to the
#' closed interval.  Used internally by `cdr_build_panel()` and exported so
#' that callers can standardize external data against known bounds.
#'
#' @param x Numeric vector.
#' @param lo Lower bound. Defaults to `min(x, na.rm = TRUE)`.
#' @param hi Upper bound. Defaults to `max(x, na.rm = TRUE)`.
#'
#' @return Numeric vector of the same length as `x`, values in \[0, 1\].
#'   `NA` inputs produce `NA` outputs; a degenerate range (`hi == lo`)
#'   returns a vector of zeros.
#'
#' @examples
#' cdr_standardize(c(0, 5, 10))                     # 0.0  0.5  1.0
#' cdr_standardize(c(2, 4, 6), lo = 0, hi = 10)     # 0.2  0.4  0.6
#'
#' @export
cdr_standardize <- function(x, lo = NULL, hi = NULL) {
  if (is.null(lo)) lo <- min(x, na.rm = TRUE)
  if (is.null(hi)) hi <- max(x, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi == lo) {
    return(rep(NA_real_, length(x)))
  }
  pmax(0, pmin(1, (x - lo) / (hi - lo)))
}


# ---- cdr_ols ----------------------------------------------------------------

#' CDR OLS Growth Model (CDRN Specification)
#'
#' Estimates the cross-sectional CDRN OLS model
#' \deqn{\hat{g} = \beta_C C + \beta_D D + \beta_R R
#'                 + \beta_{CDR}(C \cdot D \cdot R) + \beta_N N + \varepsilon}
#' where *g* is the country-average annualised log GDP-per-capita growth
#' rate, and C, D, R, N are min-max standardised institutional variables.
#'
#' @param data A data frame from `cdr_build_panel()`, or `NULL` to call it
#'   automatically.
#'
#' @return A list of class `"cdr_ols"` with components:
#'   \describe{
#'     \item{fit}{The [stats::lm()] object.}
#'     \item{partial_r2}{Named vector of partial R² for C, D, R.}
#'     \item{cdr_index}{Numeric vector C_std·D_std·R_std for the cross-section.}
#'     \item{coef_table}{Data frame: estimate, std_error, t_stat, p_value.}
#'     \item{data}{The cross-section data frame used for fitting.}
#'   }
#'
#' @examples
#' m <- cdr_ols()
#' print(m)
#' m$partial_r2
#'
#' @export
cdr_ols <- function(data = NULL) {
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(
    cs[, c("g", "C_std", "D_std", "R_std", "N_std", "CDR")]
  ), ]

  if (nrow(cs) < 10L) {
    stop("Too few complete observations to fit the CDR OLS model (n = ",
         nrow(cs), ").")
  }

  fit <- stats::lm(g ~ C_std + D_std + R_std + CDR + N_std, data = cs)

  pr2 <- c(
    C = .partial_r2(fit, "C_std"),
    D = .partial_r2(fit, "D_std"),
    R = .partial_r2(fit, "R_std")
  )

  s    <- summary(fit)$coefficients
  ctab <- data.frame(
    term      = rownames(s),
    estimate  = s[, "Estimate"],
    std_error = s[, "Std. Error"],
    t_stat    = s[, "t value"],
    p_value   = s[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  structure(
    list(fit = fit, partial_r2 = pr2, cdr_index = cs$CDR,
         coef_table = ctab, data = cs),
    class = "cdr_ols"
  )
}

#' @export
print.cdr_ols <- function(x, ...) {
  s <- summary(x$fit)
  cat("CDR OLS Growth Model (CDRN)\n")
  cat(sprintf("R2 = %.3f   Adj-R2 = %.3f   n = %d\n\n",
              s$r.squared, s$adj.r.squared, nrow(x$data)))
  print(x$coef_table, digits = 4, row.names = FALSE)
  cat(sprintf(
    "\nPartial R2:  C = %.3f   D = %.3f   R = %.3f\n",
    x$partial_r2["C"], x$partial_r2["D"], x$partial_r2["R"]
  ))
  invisible(x)
}

#' @export
summary.cdr_ols <- function(object, ...) summary(object$fit)


# ---- cdr_2sls ---------------------------------------------------------------

#' CDR Two-Stage Least Squares Model
#'
#' Estimates the structural CDR model using absolute latitude as an
#' instrumental variable for capitalism (C), addressing the endogeneity
#' between institutions and growth.  The interaction C·D·R is instrumented
#' by L·D·R.  Requires the **AER** package.
#'
#' @param data A data frame from `cdr_build_panel()`, or `NULL` to call it
#'   automatically.
#'
#' @return A list of class `"cdr_2sls"` with components:
#'   \describe{
#'     \item{fit}{The `ivreg` object (second stage).}
#'     \item{first_stage}{[stats::lm()] for the first stage (C_std on instruments).}
#'     \item{entrepreneurship_fraction}{Approximate share of fitted growth
#'       attributable to the CDR institutional block (target ≈ 0.85).}
#'     \item{capital_fraction}{Approximate share attributable to N (target ≈ 0.15).}
#'     \item{data}{The cross-section data frame used for fitting.}
#'   }
#'
#' @examples
#' \dontrun{
#'   m <- cdr_2sls()
#'   print(m)
#' }
#'
#' @export
cdr_2sls <- function(data = NULL) {
  if (!requireNamespace("AER", quietly = TRUE)) {
    stop("Package 'AER' is required. Install with: install.packages('AER')")
  }

  if (is.null(data)) data <- cdr_build_panel()

  cs      <- .cross_section(data)
  cs$LDR  <- cs$L_std * cs$D_std * cs$R_std
  cs <- cs[stats::complete.cases(
    cs[, c("g", "C_std", "D_std", "R_std", "N_std", "CDR", "L_std", "LDR")]
  ), ]

  if (nrow(cs) < 10L) {
    stop("Too few complete observations to fit the CDR 2SLS model (n = ",
         nrow(cs), ").")
  }

  fit <- AER::ivreg(
    g ~ C_std + D_std + R_std + CDR + N_std |
        L_std + D_std + R_std + LDR + N_std,
    data = cs
  )

  first_stage <- stats::lm(
    C_std ~ L_std + D_std + R_std + LDR + N_std,
    data = cs
  )

  # Approximate CDR-block vs N contribution using squared fitted components.
  co           <- stats::coef(fit)
  yhat         <- stats::fitted(fit)
  cdr_contrib  <- with(cs,
    co["C_std"] * C_std + co["D_std"] * D_std +
    co["R_std"] * R_std + co["CDR"]   * CDR
  )
  n_contrib    <- co["N_std"] * cs$N_std
  ss_total     <- sum(yhat^2, na.rm = TRUE)

  structure(
    list(
      fit                     = fit,
      first_stage             = first_stage,
      entrepreneurship_fraction = sum(cdr_contrib^2, na.rm = TRUE) / ss_total,
      capital_fraction          = sum(n_contrib^2,   na.rm = TRUE) / ss_total,
      data                    = cs
    ),
    class = "cdr_2sls"
  )
}

#' @export
print.cdr_2sls <- function(x, ...) {
  cat("CDR Two-Stage Least Squares\n")
  cat(sprintf("n = %d\n\n", nrow(x$data)))
  print(summary(x$fit)$coefficients, digits = 4)
  cat(sprintf(
    "\nCDR (entrepreneurship) fraction: %.1f%%\n",
    100 * x$entrepreneurship_fraction
  ))
  cat(sprintf(
    "N  (capital stock)     fraction: %.1f%%\n",
    100 * x$capital_fraction
  ))
  invisible(x)
}


# ---- cdr_index --------------------------------------------------------------

#' Compute the CDR Composite Index
#'
#' Produces two index variants per country:
#'
#' * **CDRs** (additive) – unweighted mean of C_std, D_std, R_std.
#' * **CDRp** (friction-adjusted) – CDRs minus the democratic-friction
#'   interaction C·D·R, then re-standardized to \[0, 1\].
#'
#' When no `year` is supplied the function uses the most recent observation
#' available for each country.
#'
#' @param data A data frame from `cdr_build_panel()`, or `NULL` to call it
#'   automatically.
#' @param year Integer. Filter to this year before computing the index.
#'   Overridden by a non-`NULL` `data` argument unless `year` is also used
#'   to subset that data.
#'
#' @return A data frame with one row per country, sorted descending by
#'   `CDRp`, with columns `iso2c`, `country`, `year`, `C_std`, `D_std`,
#'   `R_std`, `CDRs`, `CDRp`.
#'
#' @examples
#' idx <- cdr_index()
#' head(idx)
#'
#' @export
cdr_index <- function(data = NULL, year = NULL) {
  if (is.null(data)) {
    data <- cdr_build_panel(year = year)
  } else if (!is.null(year)) {
    data <- data[data$year %in% year, ]
  }

  # Use the most recent complete (non-NA CDR) observation per country.
  if (is.null(year) || length(unique(data$year)) > 1L) {
    data <- data[order(data$iso2c, data$year), ]
    complete_cdr <- !is.na(data$C_std) & !is.na(data$D_std) & !is.na(data$R_std)
    data <- data[complete_cdr, ]
    data <- data[!duplicated(data$iso2c, fromLast = TRUE), ]
  }

  data$CDRs <- (data$C_std + data$D_std + data$R_std) / 3
  raw_cdrp  <- (data$C_std + data$D_std + data$R_std - data$CDR) / 3
  data$CDRp <- cdr_standardize(raw_cdrp)

  cols   <- c("iso2c", "country", "year", "C_std", "D_std", "R_std",
               "CDRs", "CDRp")
  result <- data[stats::complete.cases(data[, c("CDRs", "CDRp")]), cols]
  result[order(-result$CDRp), ]
}
