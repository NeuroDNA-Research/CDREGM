#' Non-Parametric Bootstrap Confidence Intervals for the CDR Model
#'
#' Resamples countries (rows of the cross-section) with replacement, refits
#' the CDR OLS model on each replicate, and returns percentile confidence
#' intervals for every coefficient plus two derived quantities: the mean
#' fitted growth rate and the mean CDR interaction term.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param n_rep Number of bootstrap replicates.  Default `2000`.
#' @param conf Confidence level.  Default `0.95`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return An object of class `"cdr_bootstrap"`: a list with `replicates`
#'   (a `n_rep` x k matrix), `ci` (a data frame with `term`, `estimate`,
#'   `lower`, `upper`), `n_rep`, and `conf`.
#'
#' @examples
#' b <- cdr_bootstrap(n_rep = 200, seed = 1)
#' b$ci
#'
#' @export
cdr_bootstrap <- function(data = NULL, n_rep = 2000L, conf = 0.95,
                          seed = NULL) {
  stopifnot(n_rep >= 1L, conf > 0, conf < 1)
  if (!is.null(seed)) set.seed(seed)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("g", .cdr_terms())]), ]
  if (nrow(cs) < 10L)
    stop("Too few complete observations to bootstrap (n = ", nrow(cs), ").",
         call. = FALSE)

  fml  <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))
  base <- stats::lm(fml, data = cs)
  stat <- function(d) {
    fit <- stats::lm(fml, data = d)
    c(stats::coef(fit),
      mean_g   = mean(stats::fitted(fit)),
      mean_cdr = mean(d$CDR))
  }

  n    <- nrow(cs)
  reps <- vapply(seq_len(n_rep), function(i) {
    stat(cs[sample.int(n, n, replace = TRUE), , drop = FALSE])
  }, numeric(length(stat(cs))))
  reps <- t(reps)

  a  <- (1 - conf) / 2
  ci <- data.frame(
    term     = colnames(reps),
    estimate = stat(cs),
    lower    = apply(reps, 2, stats::quantile, probs = a,     names = FALSE),
    upper    = apply(reps, 2, stats::quantile, probs = 1 - a, names = FALSE),
    row.names = NULL, stringsAsFactors = FALSE
  )

  structure(list(replicates = reps, ci = ci, n_rep = n_rep, conf = conf),
            class = "cdr_bootstrap")
}

#' @rdname cdr_bootstrap
#' @param x A `cdr_bootstrap` object.
#' @param ... Ignored.
#' @export
print.cdr_bootstrap <- function(x, ...) {
  cat(sprintf("CDR Bootstrap (%d replicates, %.0f%% percentile CIs)\n\n",
              x$n_rep, 100 * x$conf))
  print(x$ci, digits = 4, row.names = FALSE)
  invisible(x)
}
