# Tier 4: democratic friction, peer group, reform path, maps, animation,
# country report, and the Shiny explorer.

panel <- cdr_build_panel()

# ---- cdr_democratic_friction ------------------------------------------

test_that("cdr_democratic_friction decomposes the interaction term", {
  fr <- cdr_democratic_friction(panel)
  expect_s3_class(fr, "cdr_democratic_friction")
  expect_true(all(c("cdr", "friction_growth", "friction_usd_pc",
                    "d_contribution", "r_contribution",
                    "net_institutional") %in% names(fr)))
  expect_true(all(fr$friction_growth <= 0))                 # negative cost
  expect_equal(fr$net_institutional,
               fr$d_contribution + fr$r_contribution + fr$friction_growth,
               tolerance = 1e-9)
  expect_true(all(diff(abs(fr$friction_growth)) <= 1e-9))   # sorted by |.|
})

test_that("cdr_democratic_friction accepts published coefficients", {
  fr <- cdr_democratic_friction(panel, model = "published")
  expect_equal(attr(fr, "b_cdr"), -1.21)
})

# ---- cdr_peer_group --------------------------------------------------

test_that("cdr_peer_group returns k ordered peers, excluding self", {
  p <- cdr_peer_group("US", k = 4, data = panel)
  expect_s3_class(p, "cdr_peer_group")
  expect_equal(nrow(p), 4L)
  expect_false("US" %in% p$iso2c)
  expect_true(all(diff(p$distance) >= -1e-9))
})

test_that("cdr_peer_group works by name and with mahalanobis", {
  p <- cdr_peer_group("Brazil", k = 3, data = panel, metric = "mahalanobis")
  expect_equal(nrow(p), 3L)
  expect_equal(attr(p, "metric"), "mahalanobis")
})

test_that("cdr_peer_group errors on an unknown country", {
  expect_error(cdr_peer_group("Narnia", data = panel), "not found")
})

# ---- cdr_reform_path -----------------------------------------------

test_that("cdr_reform_path reaches an attainable target", {
  r <- cdr_reform_path("Brazil", target_g = 0.08, data = panel)
  expect_s3_class(r, "cdr_reform_path")
  expect_true(r$reached)
  expect_gte(r$path$g[nrow(r$path)], 0.08 - 1e-6)
  expect_named(r$total_change, c("C_std", "D_std", "R_std"))
  expect_true(all(r$path$C_std >= -1e-9 & r$path$C_std <= 1 + 1e-9))
})

test_that("cdr_reform_path reports failure to reach an impossible target", {
  r <- cdr_reform_path("Brazil", target_g = 5, data = panel, max_iter = 50)
  expect_false(r$reached)
  expect_lte(r$iterations, 50L)
})

test_that("cdr_reform_path rejects non-positive costs", {
  expect_error(
    cdr_reform_path("US", 0.1, data = panel, cost = c(C_std = 0, D_std = 1,
                                                      R_std = 1)),
    "positive")
})

# ---- plots / report / explorer ------------------------------

test_that("cdr_plot_map builds a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("maps")
  p <- cdr_plot_map(panel, fill = "R_std")
  expect_s3_class(p, "ggplot")
})

test_that("cdr_plot_animated builds a gganim object", {
  skip_if_not_installed("gganimate")
  a <- cdr_plot_animated(panel, min_countries = 10)
  expect_s3_class(a, "gganim")
})

test_that("cdr_country_report renders a file", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available())
  out <- cdr_country_report("US", data = panel,
                            output_file = tempfile(fileext = ".html"))
  expect_true(file.exists(out))
})

test_that("cdr_explorer errors without shiny, finds the app otherwise", {
  if (requireNamespace("shiny", quietly = TRUE)) {
    expect_true(nzchar(system.file("shiny", "explorer", package = "CDREGM")))
  } else {
    expect_error(cdr_explorer(), "shiny")
  }
})
