#' Counterfactual CDR Policy Simulation
#'
#' Applies one or more changes to a country's standardized CDR variables
#' and reports the implied change in fitted growth, with a bootstrap
#' confidence interval from resampling the estimation sample.
#'
#' @param country ISO-2 code or country name (case-insensitive).
#' @param scenario A named numeric vector of additive changes to
#'   standardized variables (`C_std`, `D_std`, `R_std`, `N_std`), or a list
#'   of such vectors for multiple scenarios.  Resulting values are clamped
#'   to \[0, 1] and `CDR` is recomputed as `C_std * D_std * R_std`.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()] object, or `NULL` to fit one.
#' @param n_rep Bootstrap replicates for the CI.  Default `1000`.
#' @param conf Confidence level.  Default `0.95`.
#' @param seed Optional integer seed.
#'
#' @return An object of class `"cdr_simulate"`: a data frame with one row
#'   per scenario (`scenario`, `baseline_g`, `new_g`, `delta_g`, `lower`,
#'   `upper`).
#'
#' @examples
#' cdr_simulate("US", c(D_std = 0.1), n_rep = 200, seed = 1)
#'
#' @export
cdr_simulate <- function(country, scenario, data = NULL, model = NULL,
                         n_rep = 1000L, conf = 0.95, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(data)) data <- cdr_build_panel()
  if (is.null(model)) model <- cdr_ols(data)
  if (!inherits(model, "cdr_ols"))
    stop("`model` must be a cdr_ols object.", call. = FALSE)

  if (is.numeric(scenario)) scenario <- list(scenario)
  if (is.null(names(scenario)))
    names(scenario) <- paste0("scenario_", seq_along(scenario))

  cs  <- model$data
  row <- .cdr_match_country(cs, country)

  vars  <- c("C_std", "D_std", "R_std", "N_std")
  apply_scenario <- function(delta) {
    bad <- setdiff(names(delta), vars)
    if (length(bad))
      stop("Unknown scenario variable(s): ", paste(bad, collapse = ", "),
           call. = FALSE)
    new <- row
    for (v in names(delta))
      new[[v]] <- pmin(1, pmax(0, new[[v]] + delta[[v]]))
    new
  }

  fml   <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))
  b     <- stats::coef(model$fit)
  n     <- nrow(cs)

  boot_delta <- function(new_row) {
    vapply(seq_len(n_rep), function(i) {
      d  <- cs[sample.int(n, n, replace = TRUE), , drop = FALSE]
      bb <- stats::coef(stats::lm(fml, data = d))
      .cdr_predict_g(bb, new_row) - .cdr_predict_g(bb, row)
    }, numeric(1))
  }

  a <- (1 - conf) / 2
  rows <- lapply(names(scenario), function(nm) {
    new_row  <- apply_scenario(scenario[[nm]])
    base_g   <- .cdr_predict_g(b, row)
    new_g    <- .cdr_predict_g(b, new_row)
    reps     <- boot_delta(new_row)
    data.frame(
      scenario   = nm,
      baseline_g  = base_g,
      new_g       = new_g,
      delta_g     = new_g - base_g,
      lower       = stats::quantile(reps, a,     names = FALSE),
      upper       = stats::quantile(reps, 1 - a, names = FALSE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "country") <- paste0(row$country, " (", row$iso2c, ")")
  class(out) <- c("cdr_simulate", "data.frame")
  out
}

#' @rdname cdr_simulate
#' @param x A `cdr_simulate` object.
#' @param ... Ignored.
#' @export
print.cdr_simulate <- function(x, ...) {
  cat("CDR Counterfactual Simulation:", attr(x, "country"), "\n\n")
  print(as.data.frame(x), digits = 4, row.names = FALSE)
  invisible(x)
}
