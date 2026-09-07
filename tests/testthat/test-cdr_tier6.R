# Tier 6: data infrastructure -- quality, lookup, merge, add-country,
# historical panel, dataset refresh.

# ---- cdr_data_quality ------------------------------------------------

test_that("cdr_data_quality reports missingness by year and flags cells", {
  q <- cdr_data_quality()
  expect_s3_class(q, "cdr_data_quality")
  expect_true(all(q$overall >= 0 & q$overall <= 1))
  expect_true("capitalization" %in% rownames(q$by_year))
  expect_true(all(c("iso2c", "year") %in% names(q$flagged)))
  expect_equal(nrow(q$flagged), sum(is.na(indicators$capitalization)))
})

test_that("cdr_data_quality handles a missing flag column", {
  q <- cdr_data_quality(flag = "not_a_column")
  expect_equal(nrow(q$flagged), 0L)
})

# ---- cdr_iso_lookup ------------------------------------------------

test_that("cdr_iso_lookup resolves mixed identifier formats", {
  skip_if_not_installed("countrycode")
  out <- cdr_iso_lookup(c("USA", "840", "BR", "Japan"))
  expect_equal(out, c("US", "US", "BR", "JP"))
})

test_that("cdr_iso_lookup warns on unmatched input and returns NA", {
  skip_if_not_installed("countrycode")
  expect_warning(res <- cdr_iso_lookup(c("US", "Xanadu")), "Unmatched")
  expect_equal(res, c("US", NA))
})

test_that("cdr_iso_lookup honours an explicit origin", {
  skip_if_not_installed("countrycode")
  expect_equal(cdr_iso_lookup(c("USA", "DEU"), origin = "iso3c"),
               c("US", "DE"))
})

# ---- cdr_merge_external ------------------------------------------

test_that("cdr_merge_external joins by country name", {
  skip_if_not_installed("countrycode")
  ext <- data.frame(country = c("United States", "Brazil", "Narnia"),
                    trade = c(0.25, 0.28, 0.9))
  m <- cdr_merge_external(ext, name = "country", year = 2021)
  expect_true("trade" %in% names(m))
  expect_equal(m$trade[m$iso2c == "US"][1], 0.25)
  expect_equal(nrow(attr(m, "unmatched")), 1L)
})

test_that("cdr_merge_external joins by an ISO-2 key", {
  ext <- data.frame(cc = c("us", "jp"), score = c(1, 2))
  m <- cdr_merge_external(ext, key = "cc", year = 2021)
  expect_equal(m$score[m$iso2c == "US"][1], 1)
  expect_true(all(is.na(m$score[!m$iso2c %in% c("US", "JP")])))
})

test_that("cdr_merge_external needs a key or a name", {
  expect_error(cdr_merge_external(data.frame(a = 1)), "either")
})

# ---- cdr_add_country ------------------------------------------

new_rows <- data.frame(
  iso2c = "ZZ", year = 2021:2022,
  gdp = c(1e9, 1.1e9), capitalization = c(5, 6),
  democracy = c(0.5, 0.5), corruption = c(0.1, 0.1),
  population = c(1e6, 1e6))
zz_meta <- data.frame(code = "ZZ", country = "Zedland",
                      latitude = 10, natural_resources = 2)

test_that("cdr_add_country extends both datasets and rebuilds the panel", {
  out <- cdr_add_country("ZZ", new_rows, country_row = zz_meta)
  expect_true("ZZ" %in% out$indicators$iso2c)
  expect_true("ZZ" %in% out$countries$code)
  expect_equal(nrow(out$panel), nrow(indicators) + 2L)
  expect_true(all(c("C_std", "D_std", "R_std", "CDR") %in% names(out$panel)))
  expect_false(any(out$extrapolation))
})

test_that("cdr_add_country warns on out-of-sample inputs", {
  wild <- new_rows
  wild$capitalization <- 5000
  expect_warning(cdr_add_country("ZZ", wild, country_row = zz_meta),
                 "extrapolation")
})

test_that("cdr_add_country needs country_row for a genuinely new code", {
  expect_error(cdr_add_country("ZZ", new_rows), "country_row")
})

# ---- cdr_historical_panel / cdr_update_data --------------

test_that("cdr_historical_panel reads an existing cache without fetching", {
  d <- file.path(tempdir(), paste0("t6_", as.integer(runif(1, 1, 1e9)))); dir.create(d)
  fake <- indicators[1:12, ]
  saveRDS(fake, file.path(d, "cdr_historical_1990_2000.rds"))
  hp <- cdr_historical_panel(1990, 2000, cache_dir = d)
  expect_equal(nrow(hp), 12L)
  expect_equal(normalizePath(attr(hp, "cache")),
               normalizePath(file.path(d, "cdr_historical_1990_2000.rds")))
})

test_that("cdr_historical_panel downloads when asked (network)", {
  skip_on_cran()
  skip_if_offline()
  d <- file.path(tempdir(), paste0("t6_", as.integer(runif(1, 1, 1e9)))); dir.create(d)
  hp <- cdr_historical_panel(2015, 2017, codes = c("US", "DE"), cache_dir = d)
  expect_true(all(c("iso2c", "year", "gdp") %in% names(hp)))
  expect_true(all(hp$year >= 2015 & hp$year <= 2017))
})

test_that("cdr_update_data returns both datasets (network)", {
  skip_on_cran()
  skip_if_offline()
  fresh <- cdr_update_data(codes = c("US", "DE", "BR"))
  expect_true(all(c("indicators", "countries") %in% names(fresh)))
  expect_s3_class(fresh$indicators, "data.frame")
})
