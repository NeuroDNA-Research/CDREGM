#' Shapley Decomposition of a Country's Growth Change
#'
#' Splits the change in fitted growth for one country between two years
#' into additive contributions from the moves in `C`, `D`, `R`, and `N`.
#' Because the model contains the `C * D * R` interaction the split is not
#' unique by ordering, so Shapley values (the average marginal
#' contribution over all 24 orderings) are used; they sum exactly to the
#' total change.
#'
#' @param country ISO-2 code or country name.
#' @param from_year,to_year The two years to compare.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], `"published"`, or `NULL`.
#'
#' @return An object of class `"cdr_growth_accounting"`: a list with
#'   `contributions` (named vector for `C`, `D`, `R`, `N` that sums to
#'   `total`), `total`, `g_from`, `g_to`, and the two years.
#'
#' @examples
#' \dontrun{
#'   cdr_growth_accounting("Poland", 2022, 2024)
#' }
#'
#' @export
cdr_growth_accounting <- function(country, from_year, to_year,
                                  data = NULL, model = NULL) {
  if (is.null(data)) data <- cdr_build_panel()
  b <- .cdr_coef(model, data)

  pick <- function(yr) {
    r <- data[data$year == yr &
              (toupper(data$iso2c) == toupper(country) |
               tolower(data$country) == tolower(country)), ]
    if (nrow(r) == 0L)
      stop("No row for '", country, "' in ", yr, ".", call. = FALSE)
    stats::setNames(as.numeric(r[1, c("C_std", "D_std", "R_std", "N_std")]),
                    c("C_std", "D_std", "R_std", "N_std"))
  }
  v1 <- pick(from_year)
  v2 <- pick(to_year)
  if (any(is.na(c(v1, v2))))
    stop("Missing standardized values for '", country,
         "' in one of the years.", call. = FALSE)

  g_of <- function(v) {
    b0 <- if ("(Intercept)" %in% names(b)) b[["(Intercept)"]] else 0
    b0 + b[["C_std"]] * v[["C_std"]] + b[["D_std"]] * v[["D_std"]] +
      b[["R_std"]] * v[["R_std"]] +
      b[["CDR"]] * v[["C_std"]] * v[["D_std"]] * v[["R_std"]] +
      b[["N_std"]] * v[["N_std"]]
  }

  vars  <- c("C_std", "D_std", "R_std", "N_std")
  perms <- .cdr_permutations(vars)
  contr <- stats::setNames(numeric(length(vars)), vars)
  for (p in perms) {
    cur <- v1
    for (nm in p) {
      before <- g_of(cur)
      cur[[nm]] <- v2[[nm]]
      contr[[nm]] <- contr[[nm]] + (g_of(cur) - before)
    }
  }
  contr <- contr / length(perms)
  names(contr) <- c("C", "D", "R", "N")

  structure(
    list(contributions = contr, total = sum(contr),
         g_from = g_of(v1), g_to = g_of(v2),
         from_year = from_year, to_year = to_year,
         country = country),
    class = "cdr_growth_accounting")
}

# All permutations of a character vector (small n only).
.cdr_permutations <- function(x) {
  if (length(x) <= 1L) return(list(x))
  do.call(c, lapply(seq_along(x), function(i)
    lapply(.cdr_permutations(x[-i]), function(p) c(x[i], p))))
}

#' @rdname cdr_growth_accounting
#' @param x A `cdr_growth_accounting` object.
#' @param ... Ignored.
#' @export
print.cdr_growth_accounting <- function(x, ...) {
  cat(sprintf("CDR Growth Accounting: %s, %s -> %s\n\n",
              x$country, x$from_year, x$to_year))
  cat(sprintf("  fitted g: %.4f -> %.4f   (change %+.4f)\n\n",
              x$g_from, x$g_to, x$total))
  print(round(x$contributions, 4))
  invisible(x)
}
