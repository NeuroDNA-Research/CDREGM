#' Entrepreneurship-Capital Elasticity of Growth
#'
#' Computes the point elasticity of growth \eqn{g} with respect to
#' exogenous entrepreneurial capital \eqn{\hat C} for each country, from the
#' 2SLS structural coefficients (Ridley & Llaugel 2018, sec. 1.5).  A
#' fraction `f` of growth is assumed reinvested in capital stock, so
#' \deqn{\hat g_i = \frac{b_C \hat C_i + b_D D_i + b_R R_i
#'        + b_{CDR}\hat C_i D_i R_i}{1 - b_C f + (-b_{CDR}) f D_i R_i},}
#' \deqn{\frac{\partial E[\hat g_i]}{\partial \hat C_i}
#'        = \frac{b_C + b_{CDR} D_i R_i}
#'               {1 - b_C f + (-b_{CDR}) f D_i R_i},}
#' and the elasticity is \eqn{(\hat C_i / \hat g_i)\,
#' \partial E[\hat g_i]/\partial \hat C_i}.
#'
#' With `f = 0` growth is always inelastic (elasticity < 1); unitary
#' elasticity becomes reachable for moderate `D`, `R` once `f` rises toward
#' the global average gross fixed capital formation rate (\eqn{\approx}
#' 0.21).
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param f Reinvestment fraction, scalar in \[0, 1).  Default `0`.
#' @param coefficients Named numeric vector with `C_std`, `D_std`, `R_std`,
#'   `CDR`.  Defaults to the published 2SLS second-stage estimates.
#' @param c_hat Optional per-country exogenous capital estimate.  `NULL`
#'   (default) uses `C_std` from the panel.
#'
#' @return A data frame, one row per country, with columns `iso2c`,
#'   `country`, `D_std`, `R_std`, `c_hat`, `g_hat`, `marginal_return`,
#'   `elasticity`, sorted by `elasticity` descending.
#'
#' @references
#' Ridley, D. & Llaugel, F. (2018). Advances in the CDR economic theory of
#' entrepreneurship and GDP.
#'
#' @examples
#' head(cdr_elasticity(f = 0.21))
#'
#' @export
cdr_elasticity <- function(data = NULL,
                           f = 0,
                           coefficients = c(C_std = 1.295617, D_std = 0.116963,
                                            R_std = 0.275395, CDR = -0.98133),
                           c_hat = NULL) {
  stopifnot(length(f) == 1L, f >= 0, f < 1)
  need <- c("C_std", "D_std", "R_std", "CDR")
  if (!all(need %in% names(coefficients)))
    stop("`coefficients` needs elements C_std, D_std, R_std, CDR.",
         call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std")]), ]

  bc  <- coefficients[["C_std"]]
  bd  <- coefficients[["D_std"]]
  br  <- coefficients[["R_std"]]
  bcd <- coefficients[["CDR"]]

  ch <- if (is.null(c_hat)) cs$C_std else rep_len(c_hat, nrow(cs))
  dr <- cs$D_std * cs$R_std

  denom <- 1 - bc * f - bcd * f * dr
  g_hat <- (bc * ch + bd * cs$D_std + br * cs$R_std + bcd * ch * dr) / denom
  marg  <- (bc + bcd * dr) / denom
  elast <- (ch / g_hat) * marg

  out <- data.frame(
    iso2c = cs$iso2c, country = cs$country,
    D_std = cs$D_std, R_std = cs$R_std,
    c_hat = ch, g_hat = g_hat,
    marginal_return = marg, elasticity = elast,
    stringsAsFactors = FALSE
  )
  out[order(-out$elasticity), ]
}
