#' Conjugate Bayesian CDR Regression
#'
#' Fits the CDR growth model with a Normal-Inverse-Gamma conjugate prior
#' centred on the published CDR coefficients, giving an analytic posterior
#' (no MCMC, no compiled dependency).  The prior pulls estimates toward the
#' published sign structure; `prior_scale` sets how strongly.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param prior_mean Named coefficient vector for the prior mean, or
#'   `"published"` (default) for the CDRN OLS estimates.  Missing design
#'   columns default to a prior mean of 0.
#' @param prior_scale Prior standard deviation on each coefficient (the
#'   prior covariance is `prior_scale^2 * I`, in units of the residual
#'   variance).  Larger = weaker prior.  Default `0.5`.
#' @param prior_shape,prior_rate Inverse-Gamma hyperparameters for the
#'   residual variance.  Defaults `0.001` (near-flat).
#' @param n_draws Posterior draws to return.  Default `4000`.
#' @param seed Optional integer seed.
#'
#' @return An object of class `"cdr_bayes"`: a list with `posterior` (a
#'   data frame: `term`, `mean`, `sd`, `q2.5`, `q97.5`), `draws` (an
#'   `n_draws` x k matrix), `sigma` (posterior summary of the residual
#'   SD), and `prior`.
#'
#' @examples
#' b <- cdr_bayes(seed = 1)
#' b$posterior
#'
#' @export
cdr_bayes <- function(data = NULL, prior_mean = "published",
                      prior_scale = 0.5, prior_shape = 1e-3,
                      prior_rate = 1e-3, n_draws = 4000L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("g", .cdr_terms())]), ]
  if (nrow(cs) < 10L)
    stop("Too few complete observations (n = ", nrow(cs), ").", call. = FALSE)

  X <- stats::model.matrix(
    stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + "))),
    data = cs)
  y  <- cs$g
  nm <- colnames(X)

  pm <- if (identical(prior_mean, "published")) .cdr_coef("published") else prior_mean
  if (!is.numeric(pm) || is.null(names(pm)))
    stop("`prior_mean` must be \"published\" or a named numeric vector.",
         call. = FALSE)
  m0 <- stats::setNames(rep(0, length(nm)), nm)
  m0[intersect(nm, names(pm))] <- pm[intersect(nm, names(pm))]

  V0i <- diag(1 / prior_scale^2, length(nm))
  Vn  <- solve(V0i + crossprod(X))
  mn  <- as.vector(Vn %*% (V0i %*% m0 + crossprod(X, y)))
  names(mn) <- nm

  n  <- nrow(X)
  an <- prior_shape + n / 2
  bn <- as.numeric(prior_rate + 0.5 *
        (crossprod(y) + t(m0) %*% V0i %*% m0 - t(mn) %*% solve(Vn) %*% mn))

  sig2 <- 1 / stats::rgamma(n_draws, shape = an, rate = bn)
  L    <- chol(Vn)
  draws <- t(vapply(sig2, function(s2) {
    mn + sqrt(s2) * as.vector(crossprod(L, stats::rnorm(length(nm))))
  }, numeric(length(nm))))
  colnames(draws) <- nm

  post <- data.frame(
    term = nm,
    mean = colMeans(draws),
    sd   = apply(draws, 2, stats::sd),
    q2.5  = apply(draws, 2, stats::quantile, 0.025, names = FALSE),
    q97.5 = apply(draws, 2, stats::quantile, 0.975, names = FALSE),
    row.names = NULL, stringsAsFactors = FALSE
  )

  structure(
    list(posterior = post, draws = draws,
         sigma = c(mean = mean(sqrt(sig2)),
                   q2.5 = stats::quantile(sqrt(sig2), 0.025, names = FALSE),
                   q97.5 = stats::quantile(sqrt(sig2), 0.975, names = FALSE)),
         prior = list(mean = m0, scale = prior_scale)),
    class = "cdr_bayes"
  )
}

#' @rdname cdr_bayes
#' @param x A `cdr_bayes` object.
#' @param ... Ignored.
#' @export
print.cdr_bayes <- function(x, ...) {
  cat("Conjugate Bayesian CDR Regression\n")
  cat(sprintf("Prior: N(published, %.2f^2)   draws: %d\n\n",
              x$prior$scale, nrow(x$draws)))
  print(x$posterior, digits = 4, row.names = FALSE)
  cat(sprintf("\nResidual SD (posterior mean): %.4f\n", x$sigma[["mean"]]))
  invisible(x)
}
