#' Spatial Dependence in CDR Growth
#'
#' Tests for and models spillovers in the CDR framework.  Neighbours are
#' defined either in standardized CDR space (`weights = "cdr"`, the
#' institutional-contagion interpretation) or by absolute latitude
#' (`weights = "latitude"`, a crude geographic proxy — the bundled data has
#' no longitude).
#'
#' Reports Moran's I for the growth rate and the CDR index (analytic and
#' permutation p-values, computed without external packages), and estimates
#' a spatial-lag model `g = rho * W g + X beta + e` by spatial two-stage
#' least squares (instruments `W X`, `W^2 X`).  If **spatialreg** is
#' installed, maximum-likelihood SAR and SEM fits are added.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param weights `"cdr"` (default) or `"latitude"`.
#' @param k Number of nearest neighbours.  Default `5`.
#' @param n_perm Permutations for the Moran's I test.  Default `999`.
#'
#' @return An object of class `"cdr_spatial"`: a list with `moran_g`,
#'   `moran_index` (each a list from the internal Moran routine),
#'   `slx_2sls` (the spatial-lag 2SLS fit: `coef`, `se`, `t`, with `rho`
#'   as the `Wg` term), optionally `sar` / `sem` (spatialreg fits), and
#'   `W`.
#'
#' @references
#' Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer.
#'
#' @examples
#' m <- cdr_spatial(k = 4, n_perm = 99)
#' m$moran_g[c("I", "p_perm")]
#'
#' @export
cdr_spatial <- function(data = NULL, weights = c("cdr", "latitude"),
                        k = 5L, n_perm = 999L) {
  weights <- match.arg(weights)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("g", .cdr_terms())]), ]

  if (weights == "cdr") {
    coords <- as.matrix(cs[, c("C_std", "D_std", "R_std")])
  } else {
    lat <- stats::aggregate(cbind(L_std = L_std) ~ iso2c, data = data,
                            FUN = function(v) mean(v, na.rm = TRUE))
    cs  <- merge(cs, lat, by = "iso2c", suffixes = c("", ".dup"))
    coords <- as.matrix(cs[, "L_std", drop = FALSE])
  }
  W <- .cdr_knn_weights(coords, k = k)

  idx <- cdr_index(data)
  cs  <- merge(cs, idx[, c("iso2c", "CDRp")], by = "iso2c", all.x = TRUE)

  moran_g   <- .cdr_moran(cs$g, W, n_perm = n_perm)
  moran_idx <- .cdr_moran(cs$CDRp, W, n_perm = n_perm)

  # Spatial-lag model by 2SLS
  Xm <- stats::model.matrix(
    stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + "))),
    data = cs)
  Wg  <- as.vector(W %*% cs$g)
  WX  <- W %*% Xm[, -1, drop = FALSE]
  W2X <- W %*% WX
  reg <- cbind(Xm, Wg = Wg)
  inst <- cbind(Xm, WX, W2X)
  slx <- .cdr_tsls(cs$g, reg, inst)

  out <- list(moran_g = moran_g, moran_index = moran_idx,
              slx_2sls = slx, W = W, weights = weights, data = cs)

  if (requireNamespace("spatialreg", quietly = TRUE) &&
      requireNamespace("spdep", quietly = TRUE)) {
    lw  <- spdep::mat2listw(W, style = "W")
    fml <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))
    out$sar <- tryCatch(spatialreg::lagsarlm(fml, data = cs, listw = lw),
                        error = function(e) NULL)
    out$sem <- tryCatch(spatialreg::errorsarlm(fml, data = cs, listw = lw),
                        error = function(e) NULL)
  }

  structure(out, class = "cdr_spatial")
}

#' @rdname cdr_spatial
#' @param x A `cdr_spatial` object.
#' @param ... Ignored.
#' @export
print.cdr_spatial <- function(x, ...) {
  cat(sprintf("CDR Spatial Analysis  (%s neighbours)\n\n", x$weights))
  f <- function(m, lab)
    cat(sprintf("  Moran's I, %-10s = %+.3f   E[I] = %+.3f   p(perm) = %.3f\n",
                lab, m$I, m$expectation, m$p_perm))
  f(x$moran_g, "growth")
  f(x$moran_index, "CDR index")
  cat(sprintf("\n  Spatial-lag rho (2SLS) = %+.3f  (t = %.2f)\n",
              x$slx_2sls$coef[["Wg"]],
              x$slx_2sls$t[["Wg"]]))
  if (!is.null(x$sar))
    cat(sprintf("  Spatial-lag rho (ML)   = %+.3f\n", x$sar$rho))
  invisible(x)
}
