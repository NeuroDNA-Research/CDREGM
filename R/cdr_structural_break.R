#' Test the CDR Coefficients for Structural Breaks Over Time
#'
#' Two complementary tests of the framework's time-invariance claim:
#' a joint F-test that the CDR coefficients are equal across all years
#' (an interaction model), and a Chow test at a single `breakpoint` year.
#' If **strucchange** is installed, a Bai-Perron breakpoint search on the
#' year-mean growth series is added.
#'
#' The bundled panel has only a handful of years, so these tests are
#' illustrative rather than definitive.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param breakpoint Year at which to split for the Chow test.  `NULL`
#'   (default) uses the median available year.
#'
#' @return An object of class `"cdr_structural_break"`: a list with
#'   `joint` (an `anova` comparing the pooled and year-interacted models),
#'   `chow` (list: `F`, `df1`, `df2`, `p.value`, `breakpoint`),
#'   `by_year` (per-year coefficient data frame), and optionally
#'   `bai_perron`.
#'
#' @references
#' Bai, J. & Perron, P. (1998). Estimating and testing linear models with
#' multiple structural changes. *Econometrica*, 66(1), 47-78.
#'
#' @examples
#' cdr_structural_break()
#'
#' @export
cdr_structural_break <- function(data = NULL, breakpoint = NULL) {
  if (is.null(data)) data <- cdr_build_panel()

  pnl <- .gdp_growth(data)
  pnl <- pnl[stats::complete.cases(pnl[, c("g", .cdr_terms(), "year")]), ]
  yrs <- sort(unique(pnl$year))
  if (length(yrs) < 2L)
    stop("Need at least two years with a growth rate.", call. = FALSE)

  terms_rhs <- paste(.cdr_terms(), collapse = " + ")
  pooled <- stats::lm(stats::as.formula(paste("g ~", terms_rhs)), data = pnl)

  pnl$yr <- factor(pnl$year)
  inter  <- stats::lm(
    stats::as.formula(paste("g ~ (", terms_rhs, ") * yr")), data = pnl)
  joint  <- stats::anova(pooled, inter)

  if (is.null(breakpoint)) breakpoint <- stats::median(yrs)
  early <- pnl[pnl$year <  breakpoint, ]
  late  <- pnl[pnl$year >= breakpoint, ]
  chow <- NULL
  if (nrow(early) > length(.cdr_terms()) + 1L &&
      nrow(late)  > length(.cdr_terms()) + 1L) {
    f0 <- stats::as.formula(paste("g ~", terms_rhs))
    rss_p <- sum(stats::resid(pooled)^2)
    rss_e <- sum(stats::resid(stats::lm(f0, data = early))^2)
    rss_l <- sum(stats::resid(stats::lm(f0, data = late))^2)
    k  <- length(stats::coef(pooled))
    n  <- nrow(pnl)
    Fs <- ((rss_p - (rss_e + rss_l)) / k) / ((rss_e + rss_l) / (n - 2 * k))
    chow <- list(F = Fs, df1 = k, df2 = n - 2 * k,
                 p.value = stats::pf(Fs, k, n - 2 * k, lower.tail = FALSE),
                 breakpoint = breakpoint)
  }

  by_year <- do.call(rbind, lapply(yrs, function(yr) {
    d <- pnl[pnl$year == yr, ]
    if (nrow(d) < length(.cdr_terms()) + 2L) return(NULL)
    co <- stats::coef(stats::lm(stats::as.formula(paste("g ~", terms_rhs)),
                                data = d))
    data.frame(year = yr, t(co), check.names = FALSE)
  }))

  bai_perron <- NULL
  if (requireNamespace("strucchange", quietly = TRUE)) {
    ts_g <- tapply(pnl$g, pnl$year, mean)
    bai_perron <- tryCatch(
      strucchange::breakpoints(ts_g ~ 1), error = function(e) NULL)
  }

  structure(list(joint = joint, chow = chow, by_year = by_year,
                 bai_perron = bai_perron),
            class = "cdr_structural_break")
}

#' @rdname cdr_structural_break
#' @param x A `cdr_structural_break` object.
#' @param ... Ignored.
#' @export
print.cdr_structural_break <- function(x, ...) {
  cat("CDR Structural-Break Tests\n\n")
  pj <- x$joint[["Pr(>F)"]][2]
  cat(sprintf("  Joint test (coefficients constant across years): p = %.3f\n",
              pj))
  if (!is.null(x$chow))
    cat(sprintf("  Chow test at %s: F = %.2f, p = %.3f\n",
                x$chow$breakpoint, x$chow$F, x$chow$p.value))
  cat("\n  Per-year coefficients:\n")
  print(x$by_year, digits = 3, row.names = FALSE)
  invisible(x)
}
