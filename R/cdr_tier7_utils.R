# ---- Tier 7 internal helpers -------------------------------------------

# Published 2SLS second-stage coefficients (Ridley & Llaugel 2018), used
# by the reinvestment / elasticity tools.
.cdr_tsls_published <- function() {
  c(C_std = 1.295617, D_std = 0.116963, R_std = 0.275395, CDR = -0.98133)
}

# Entrepreneurship-capital elasticity of growth at reinvestment fraction
# `f`, given 2SLS coefficients `b`, capital `c_hat`, and D, R.
.cdr_elasticity_at <- function(b, c_hat, d, r, f) {
  bc <- b[["C_std"]]; bd <- b[["D_std"]]; br <- b[["R_std"]]; bcd <- b[["CDR"]]
  dr    <- d * r
  denom <- 1 - bc * f - bcd * f * dr
  g_hat <- (bc * c_hat + bd * d + br * r + bcd * c_hat * dr) / denom
  marg  <- (bc + bcd * dr) / denom
  (c_hat / g_hat) * marg
}

# World Bank FY income-group thresholds (GNI per capita, current USD;
# 2023 revision).  Used as a rough classifier on GDP per capita.
.cdr_income_group <- function(gdp_pc) {
  cut(gdp_pc,
      breaks = c(-Inf, 1145, 4515, 14005, Inf),
      labels = c("LIC", "LMIC", "UMIC", "HIC"))
}
