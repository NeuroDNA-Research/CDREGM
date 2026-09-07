# ---- Tier 6 (data infrastructure) internal helpers ---------------------

# Standardize an indicator panel + country metadata into the modelling
# panel, exactly as cdr_build_panel() does, but from explicit data frames
# (cdr_build_panel() only reads the bundled datasets).
.cdr_standardize_panel <- function(indicators, countries) {
  ind <- indicators
  cty <- countries
  ind$iso2c[is.na(ind$iso2c)] <- "NA"
  cty$code[is.na(cty$code)]   <- "NA"

  p <- merge(ind, cty[, c("code", "country", "latitude",
                          "natural_resources")],
             by.x = "iso2c", by.y = "code", all.x = TRUE)
  p$C_std <- cdr_standardize(p$capitalization)
  p$D_std <- cdr_standardize(p$democracy)
  p$R_std <- cdr_standardize(p$corruption)
  p$N_std <- cdr_standardize(p$natural_resources)
  p$L_std <- cdr_standardize(abs(p$latitude))
  p$CDR   <- p$C_std * p$D_std * p$R_std
  p[order(p$iso2c, p$year), , drop = FALSE]
}
