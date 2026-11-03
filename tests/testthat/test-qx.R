# number of life tables (for each sex) each calibration was estimated from
n.lts <- function(m) m$female$v1$df.residual + m$female$v1$rank

test_that("every calibration version is available and distinct", {
  versions <- c(2018, 2021, 2022, 2024, 2025, 2026)
  out <- lapply(versions, function(v) qx("female", 0.05, modsv = v)[, 1])
  for (i in 1:(length(versions) - 1)) {
    for (j in (i + 1):length(versions)) {
      expect_false(isTRUE(all.equal(out[[i]], out[[j]])),
                   info = paste(versions[i], "vs", versions[j]))
    }
  }
  # the default is the newest calibration
  expect_identical(qx("female", 0.05), qx("female", 0.05, modsv = 2026))
})

test_that("each calibration has the documented number of life tables", {
  expect_equal(n.lts(mods2018), 4610)  # the Demography paper
  expect_equal(n.lts(mods2021), 4765)  # 'mods2018' in version 0.1.0
  expect_equal(n.lts(mods2022), 4857)
  expect_equal(n.lts(mods2024), 4902)
  expect_equal(n.lts(mods2025), 4946)
  expect_equal(n.lts(mods2026), 4987)
})

test_that("with match.inputs = FALSE, results for 2022 and 2024 are unchanged from 0.1.0", {
  fx <- readRDS(test_path("fixtures", "qx-0.1.0.rds"))
  for (k in seq_len(nrow(fx$grid))) {
    g <- fx$grid[k, ]
    am <- if (is.na(g$am)) NULL else g$am
    # (some of these input combinations now also give an agreement warning)
    new <- suppressWarnings(qx(g$sex, g$cm, smooth = g$smooth, outlogit = g$outlogit,
                               out5 = g$out5, am = am, modsv = g$modsv,
                               match.inputs = FALSE))
    expect_equal(new, fx$ref[[k]], tolerance = 1e-12, info = paste("row", k))
  }
})

test_that("qx() returns the expected shapes", {
  expect_equal(dim(qx("female", 0.05)), c(24, 1))
  expect_equal(dim(qx("female", 0.05, out5 = FALSE)), c(110, 1))
  expect_equal(dim(qx("male", c(0.01, 0.05, 0.1))), c(24, 3))
  expect_equal(dim(qx("male", 0.05, outlogit = TRUE)), c(23, 1))
  expect_equal(qx("female", 0.05)["110+", 1], 1)
  q <- qx("male", c(0.004, 0.3), out5 = FALSE)
  expect_true(all(q > 0 & q < 1))
  expect_equal(nrow(qx("female", 0.05, outlogit = TRUE, out5 = FALSE)), 110)
})

test_that("sex is not case sensitive", {
  expect_identical(qx("Female", 0.05), qx("female", 0.05))
  expect_identical(qx("MALE", 0.05), qx("male", 0.05))
})

test_that("bad inputs give clear errors", {
  expect_error(qx("female"), "must both be given")
  expect_error(qx("women", 0.05), "female")
  expect_error(qx(c("female", "male"), 0.05), "female")
  expect_error(qx("female", NA_real_), "missing")
  expect_error(qx("female", NA), "number")
  expect_error(qx("female", "0.05"), "number")
  expect_error(qx("female", 0.05, am = 1.5), "between")
  expect_error(qx("female", 0.05, am = NA_real_), "missing")
  expect_error(qx("female", c(0.05, 0.06), am = 0.2), "same length")
  expect_error(qx("female", 0.05, modsv = 2019), "must be one of")
  expect_error(qx("female", 0.05, modsv = NA), "must be one of")
  expect_error(qx("female", 0.05, match.inputs = NA), "TRUE or FALSE")
  expect_error(qx("female", 0.05, match.inputs = "yes"), "TRUE or FALSE")
})

test_that("am = NULL given explicitly means am is predicted", {
  expect_identical(qx("female", 0.05, am = NULL), qx("female", 0.05))
})

test_that("q1to5() accepts a vector, a matrix, or a data frame", {
  q1 <- qx("female", c(0.02, 0.08), out5 = FALSE)
  from.df <- q1to5(q1)
  from.vector <- q1to5(q1[, 1])
  from.matrix <- q1to5(as.matrix(q1))
  expect_equal(unname(as.matrix(from.vector)[, 1]), unname(as.matrix(from.df)[, 1]))
  expect_equal(from.matrix, from.df)
  expect_equal(nrow(from.df), 24)
  expect_error(q1to5(q1[1:100, ]), "at least 110")
})

test_that("inputs are limited to the range of the calibration life tables", {
  r <- SVDMx:::calibrationRanges
  expect_equal(nrow(r), 12)
  expect_true(all(r$cm.min < r$cm.max & r$am.min < r$am.max))
  # each version and sex accepts its own limits and rejects values just outside them
  for (k in seq_len(nrow(r))) {
    expect_error(suppressWarnings(qx(r$sex[k], r$cm.min[k], modsv = r$modsv[k])), NA)
    expect_error(suppressWarnings(qx(r$sex[k], r$cm.max[k], modsv = r$modsv[k])), NA)
    expect_error(qx(r$sex[k], r$cm.min[k] * 0.99, modsv = r$modsv[k]), "must be between")
    expect_error(qx(r$sex[k], r$cm.max[k] * 1.01, modsv = r$modsv[k]), "must be between")
    expect_error(qx(r$sex[k], 0.05, am = r$am.max[k] * 1.01, modsv = r$modsv[k]), "must be between")
  }
  # current low-mortality levels are now covered (0.1.0 and 0.2.0 stopped below 5q0 = 0.003)
  expect_equal(dim(suppressWarnings(qx("female", 0.0025))), c(24, 1))
  expect_error(qx("female", 0.0001), "must be between")
  expect_error(qx("male", 0.001), "must be between")
  expect_error(qx("female", 1.2), "must be between")
})

test_that("without matching, a warning is given when a schedule departs from its input", {
  expect_no_warning(qx("male", c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3), match.inputs = FALSE))
  expect_no_warning(qx("female", c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3), smooth = FALSE,
                       match.inputs = FALSE))
  expect_warning(qx("male", 0.002, match.inputs = FALSE), "differs from the input")
  expect_warning(qx("male", 0.55, match.inputs = FALSE), "differs from the input")
  expect_no_warning(qx("male", c(0.002, 0.55)))
})

test_that("column labels are distinct and keep the 0.1.0 format", {
  expect_match(colnames(qx("female", 0.05)), "^cm-0\\.050\\.am-0\\.[0-9]{3}$")
  labels <- colnames(qx("female", c(0.0301, 0.0304)))
  expect_false(anyDuplicated(labels) > 0)
  expect_match(labels[1], "^cm-0\\.0301\\.")
  expect_match(colnames(suppressWarnings(qx("female", 0.0005))), "^cm-0\\.00050\\.")
})

implied <- function(q, ages) 1 - apply(1 - as.matrix(q)[ages + 1, , drop = FALSE], 2, prod)

test_that("by default the generated schedules reproduce their inputs exactly", {
  cm <- c(0.0005, 0.002, 0.01, 0.05, 0.2, 0.5)
  for (sm in c(TRUE, FALSE)) {
    q <- qx("female", cm, smooth = sm, out5 = FALSE)
    expect_equal(unname(implied(q, 0:4)), cm, tolerance = 1e-9)
    am <- c(0.06, 0.08, 0.12, 0.2, 0.4, 0.7)
    q <- qx("male", c(0.002, 0.005, 0.01, 0.05, 0.2, 0.5), am = am, smooth = sm, out5 = FALSE)
    expect_equal(unname(implied(q, 0:4)), c(0.002, 0.005, 0.01, 0.05, 0.2, 0.5), tolerance = 1e-9)
    expect_equal(unname(implied(q, 15:59)), am, tolerance = 1e-9)
  }
  # in the five-year output, 1q0 and 4q1 combine to the input 5q0
  q5 <- as.matrix(qx("female", c(0.03, 0.1)))
  expect_equal(unname(1 - (1 - q5[1, ]) * (1 - q5[2, ])), c(0.03, 0.1), tolerance = 1e-9)
})

test_that("matching converges at the corners of the input ranges", {
  r <- SVDMx:::calibrationRanges
  for (k in which(r$modsv == 2026)) {
    cm <- rep(c(r$cm.min[k], r$cm.max[k]), 2)
    am <- rep(c(r$am.min[k], r$am.max[k]), each = 2)
    expect_no_warning(q <- qx(r$sex[k], cm, am = am, out5 = FALSE))
    expect_equal(unname(implied(q, 0:4)), cm, tolerance = 1e-9)
    expect_equal(unname(implied(q, 15:59)), am, tolerance = 1e-9)
  }
})

test_that("matching changes the schedule only within the span of the components", {
  m <- mods2026$female
  for (sm in c(FALSE, TRUE)) {
    cs <- if (sm) m$components.sm else m$components
    d <- logit(as.matrix(qx("female", c(0.002, 0.05), smooth = sm, out5 = FALSE))) -
      logit(as.matrix(suppressWarnings(qx("female", c(0.002, 0.05), smooth = sm, out5 = FALSE,
                                          match.inputs = FALSE))))
    resid <- d - t(cs[1:4, ]) %*% qr.solve(t(cs[1:4, ]), d)
    expect_lt(max(abs(resid)), 1e-8)
    expect_gt(max(abs(d)), 1e-3)
  }
})
