# Column names referenced via non-standard evaluation (aggregate formulas,
# ggplot2 aesthetics).  Declared here to keep R CMD check quiet.
utils::globalVariables(c(
  ".data",
  "gdp", "population", "natural_resources",
  "g", "C_std", "D_std", "R_std", "N_std", "CDR"
))
