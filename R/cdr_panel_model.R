#' Fixed- and Random-Effects Panel Estimation of the CDR Model
#'
#' Estimates the CDR growth model on the full country-year panel (rather
#' than a single cross-section) with the **plm** package: a one-way
#' fixed-effects (within) model and a random-effects model, a Hausman test
#' between them, and cluster-robust (by country) standard errors for the
#' preferred specification.
#'
#' Note: the bundled [indicators] panel spans only a few recent years, so
#' within-country variation in the slow-moving institutional variables is
#' limited and the fixed-effects estimates are correspondingly imprecise.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param index Length-2 character vector naming the individual and time
#'   columns.  Default `c("iso2c", "year")`.
#'
#' @return An object of class `"cdr_panel"`: a list with `within`,
#'   `random` (both `plm` objects), `hausman` (an `htest`), `preferred`
#'   (`"within"` or `"random"`), and `coef_table` (robust SEs for the
#'   preferred model).
#'
#' @references
#' Croissant, Y. & Millo, G. (2008). Panel data econometrics in R: the plm
#' package. *Journal of Statistical Software*, 27(2).
#'
#' @examples
#' \dontrun{
#'   m <- cdr_panel()
#'   m$hausman
#'   m$coef_table
#' }
#'
#' @export
cdr_panel <- function(data = NULL, index = c("iso2c", "year")) {
  if (!requireNamespace("plm", quietly = TRUE))
    stop("Package 'plm' is required. Install with: install.packages('plm')",
         call. = FALSE)
  stopifnot(length(index) == 2L)
  if (is.null(data)) data <- cdr_build_panel()

  pnl <- .gdp_growth(data)
  pnl <- pnl[stats::complete.cases(pnl[, c("g", .cdr_terms(), index)]), ]
  if (length(unique(pnl[[index[2]]])) < 2L)
    stop("Panel estimation needs at least two time periods with data.",
         call. = FALSE)

  fml <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))
  pd  <- plm::pdata.frame(pnl, index = index)

  within <- plm::plm(fml, data = pd, model = "within")
  random <- tryCatch(plm::plm(fml, data = pd, model = "random"),
                     error = function(e) NULL)

  hausman <- if (!is.null(random))
    tryCatch(plm::phtest(within, random), error = function(e) NULL) else NULL

  preferred_fit <- within
  preferred     <- "within"
  if (!is.null(hausman) && is.finite(hausman$p.value) &&
      hausman$p.value > 0.05 && !is.null(random)) {
    preferred_fit <- random
    preferred     <- "random"
  }

  rob <- .cdr_coeftest(preferred_fit,
                         plm::vcovHC(preferred_fit, method = "arellano",
                                     type = "HC1", cluster = "group"))

  structure(
    list(within = within, random = random, hausman = hausman,
         preferred = preferred, coef_table = rob),
    class = "cdr_panel"
  )
}

# coeftest without importing lmtest: fall back to a plain matrix if lmtest
# is not available.
.cdr_coeftest <- function(fit, vcov_mat) {
  if (requireNamespace("lmtest", quietly = TRUE))
    return(lmtest::coeftest(fit, vcov. = vcov_mat))
  b  <- stats::coef(fit)
  se <- sqrt(diag(vcov_mat))
  cbind(Estimate = b, `Std. Error` = se,
        `t value` = b / se,
        `Pr(>|t|)` = 2 * stats::pnorm(-abs(b / se)))
}

#' @rdname cdr_panel
#' @param x A `cdr_panel` object.
#' @param ... Ignored.
#' @export
print.cdr_panel <- function(x, ...) {
  cat("CDR Panel Model\n")
  if (!is.null(x$hausman))
    cat(sprintf("  Hausman chi-sq = %.2f, p = %.3f  ->  preferred: %s\n\n",
                x$hausman$statistic, x$hausman$p.value, x$preferred))
  else
    cat(sprintf("  preferred: %s\n\n", x$preferred))
  cat("Cluster-robust coefficients (by country):\n")
  print(x$coef_table, digits = 4)
  invisible(x)
}
