# Tier 2 Demo: cdr_standardize, cdr_build_panel, cdr_ols, cdr_2sls, cdr_index
.libPaths(c("C:/Users/gypsa/AppData/Local/R/win-library/4.6", .libPaths()))
devtools::load_all("c:/Users/gypsa/CDREGM", quiet = TRUE)

# ---- 1. cdr_standardize --------------------------------------------------
# Rescales any numeric vector to [0, 1] using min-max normalization.

cat("-- cdr_standardize: basic\n")
print(cdr_standardize(c(0, 5, 10)))          # 0.0  0.5  1.0

cat("\n-- cdr_standardize: custom bounds\n")
print(cdr_standardize(c(2, 6, 8), lo = 0, hi = 10))   # 0.2  0.6  0.8

cat("\n-- cdr_standardize: NAs pass through\n")
print(cdr_standardize(c(1, NA, 3, 5)))       # 0.00  NA  0.50  1.00


# ---- 2. cdr_build_panel --------------------------------------------------
# Merges indicators + countries and adds standardised CDR columns.

panel <- cdr_build_panel()
cat("\n-- cdr_build_panel: dimensions\n")
cat(sprintf("  %d rows x %d cols\n", nrow(panel), ncol(panel)))

cat("\n-- cdr_build_panel: new columns added\n")
print(names(panel))

cat("\n-- cdr_build_panel: sample rows (Armenia)\n")
print(panel[panel$iso2c == "AM", c("iso2c","year","C_std","D_std","R_std","N_std","L_std","CDR")])

cat("\n-- cdr_build_panel: filter to a single year\n")
panel_2024 <- cdr_build_panel(year = 2024)
cat(sprintf("  Rows for 2024: %d\n", nrow(panel_2024)))


# ---- 3. cdr_ols ----------------------------------------------------------
# Fits CDRN OLS: g ~ C + D + R + C*D*R + N, returning partial R2 per pillar.

m_ols <- cdr_ols(panel)
cat("\n-- cdr_ols: model summary\n")
print(m_ols)

cat("\n-- cdr_ols: raw lm summary (F-stat, residuals)\n")
print(summary(m_ols))

cat("\n-- cdr_ols: CDR interaction values (first 6 countries)\n")
print(head(data.frame(country = m_ols$data$country, CDR_index = round(m_ols$cdr_index, 4))))


# ---- 4. cdr_2sls ---------------------------------------------------------
# 2SLS using |latitude| as IV for C; separates entrepreneurship vs capital.

m_2sls <- cdr_2sls(panel)
cat("\n-- cdr_2sls: model summary\n")
print(m_2sls)

cat("\n-- cdr_2sls: first-stage (C_std ~ latitude instruments)\n")
print(summary(m_2sls$first_stage)$coefficients)


# ---- 5. cdr_index --------------------------------------------------------
# Ranks all 79 countries by CDRp (friction-adjusted composite index).

idx <- cdr_index(panel)
cat("\n-- cdr_index: top 10 countries\n")
print(head(idx[, c("country","year","C_std","D_std","R_std","CDRs","CDRp")], 10))

cat("\n-- cdr_index: bottom 10 countries\n")
print(tail(idx[, c("country","year","C_std","D_std","R_std","CDRs","CDRp")], 10))

cat("\n-- cdr_index: specific year\n")
idx_2023 <- cdr_index(year = 2023)
cat(sprintf("  Countries with complete 2023 data: %d\n", nrow(idx_2023)))
print(head(idx_2023[, c("country","CDRs","CDRp")], 5))
