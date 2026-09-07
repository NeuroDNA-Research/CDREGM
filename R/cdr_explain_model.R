#' Plain-English Interpretation of a CDR Model
#'
#' Turns a fitted [cdr_ols()] into a short prose summary: the direction and
#' size of each coefficient, the meaning of the negative `C * D * R`
#' friction term, the partial-\eqn{R^2} ranking of the pillars, and the
#' overall fit.
#'
#' @param model A fitted [cdr_ols()], or `NULL` to fit one on `data`.
#' @param data A panel from [cdr_build_panel()], used only when
#'   `model` is `NULL`.
#'
#' @return A character vector of sentences (class `"cdr_explanation"`),
#'   printed one per line.
#'
#' @examples
#' cdr_explain_model()
#'
#' @export
cdr_explain_model <- function(model = NULL, data = NULL) {
  if (is.null(model)) model <- cdr_ols(data %||% cdr_build_panel())
  if (!inherits(model, "cdr_ols"))
    stop("`model` must be a cdr_ols object.", call. = FALSE)

  b  <- stats::coef(model$fit)
  s  <- summary(model$fit)
  pr <- sort(model$partial_r2, decreasing = TRUE)
  dir <- function(x) if (x >= 0) "raises" else "lowers"
  lab <- c(C_std = "capitalism", D_std = "democracy", R_std = "rule of law",
           N_std = "natural resources")

  lines <- c(
    sprintf(
      "The model explains %.0f%% of the cross-country variation in growth (adjusted R-squared %.2f, n = %d).",
      100 * s$r.squared, s$adj.r.squared, length(stats::residuals(model$fit))),
    sprintf(
      "Moving %s from its sample minimum to its maximum %s growth by about %.3f, all else equal.",
      lab["C_std"], dir(b[["C_std"]]), abs(b[["C_std"]])),
    sprintf(
      "The corresponding figures are %+.3f for %s and %+.3f for %s.",
      b[["D_std"]], lab["D_std"], b[["R_std"]], lab["R_std"]),
    sprintf(
      "The C*D*R interaction enters with coefficient %+.3f: this is democratic friction, the growth cost of decision-making disagreement when all three institutions are strong at once -- not evidence against democracy, since the D and R terms themselves are %s.",
      b[["CDR"]],
      if (b[["D_std"]] >= 0 && b[["R_std"]] >= 0) "positive" else "mixed"),
    sprintf(
      "By partial R-squared the pillars rank %s.",
      paste(names(pr), sprintf("(%.2f)", pr), collapse = " > ")),
    sprintf(
      "Natural resources %s growth by about %.3f.",
      dir(b[["N_std"]]), abs(b[["N_std"]]))
  )
  structure(lines, class = "cdr_explanation")
}

#' @rdname cdr_explain_model
#' @param x A `cdr_explanation` object.
#' @param ... Ignored.
#' @export
print.cdr_explanation <- function(x, ...) {
  cat(paste(strwrap(x, width = 78, exdent = 2), collapse = "\n"), "\n")
  invisible(x)
}
