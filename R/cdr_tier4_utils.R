# ---- Tier 4 internal helpers ---------------------------------------------

# Resolve the `model` argument shared by the policy functions to a plain
# named coefficient vector (C_std, D_std, R_std, CDR, N_std, intercept).
# Accepts NULL (fit cdr_ols), a cdr_ols object, the string "published"
# (the CDRN OLS estimates from Ridley & Khan 2018), or a named vector.
.cdr_coef <- function(model = NULL, data = NULL) {
  if (is.character(model) && length(model) == 1L && model == "published")
    return(c("(Intercept)" = 0, C_std = 1.53, D_std = 0.14,
             R_std = 0.23, CDR = -1.21, N_std = 0.38))
  if (is.null(model)) {
    if (is.null(data)) data <- cdr_build_panel()
    model <- cdr_ols(data)
  }
  if (inherits(model, "cdr_ols")) return(stats::coef(model$fit))
  if (is.numeric(model) && !is.null(names(model))) return(model)
  stop("`model` must be NULL, \"published\", a cdr_ols object, or a named ",
       "coefficient vector.", call. = FALSE)
}

# Fitted growth as a function of a length-3 vector x = (C_std, D_std, R_std)
# with N_std held fixed.
.cdr_g_of_cdr <- function(b, x, n_std) {
  b0 <- if ("(Intercept)" %in% names(b)) b[["(Intercept)"]] else 0
  b0 + b[["C_std"]] * x[1] + b[["D_std"]] * x[2] + b[["R_std"]] * x[3] +
    b[["CDR"]] * x[1] * x[2] * x[3] + b[["N_std"]] * n_std
}

# Gradient of .cdr_g_of_cdr with respect to x.
.cdr_grad_cdr <- function(b, x) {
  c(
    b[["C_std"]] + b[["CDR"]] * x[2] * x[3],
    b[["D_std"]] + b[["CDR"]] * x[1] * x[3],
    b[["R_std"]] + b[["CDR"]] * x[1] * x[2]
  )
}

# Map ISO-2 codes to base-R world-map region names via maps::iso3166.
.cdr_map_regions <- function(iso2) {
  key <- maps::iso3166
  key$mapname <- sub(":.*$", "", key$mapname)
  key$mapname[key$a2 == "GB"] <- "UK"
  key$mapname[key$a2 == "US"] <- "USA"
  stats::setNames(key$mapname, key$a2)[iso2]
}
