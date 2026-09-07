# Column names referenced via non-standard evaluation (aggregate formulas,
# ggplot2 aesthetics).  Declared here to keep R CMD check quiet.
utils::globalVariables(c(
  ".data",
  "gdp", "population", "natural_resources",
  "g", "C_std", "D_std", "R_std", "N_std", "L_std", "CDR",
  "long", "lat", "group", "value", "year",
  "gdp_pc", "log_gdp_pc", "component", "xval"
))
