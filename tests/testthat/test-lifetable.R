test_that("lifetable() returns abridged and single-year tables with the right structure", {
  lt <- lifetable("female", c(0.01, 0.1))
  expect_s3_class(lt, "lifetables")
  expect_equal(length(lt$tables), 2)
  expect_equal(nrow(lt$summary), 2)
  t5 <- lt$tables[[1]]
  expect_equal(names(t5), c("age", "x", "n", "nmx", "nqx", "nax", "lx", "ndx", "nLx", "Tx", "ex"))
  expect_equal(nrow(t5), 24)
  expect_equal(t5$age[c(1, 2, 3, 24)], c("0", "1-4", "5-9", "110+"))
  expect_equal(t5$n, c(1, 4, rep(5, 21), NA))
  t1 <- lifetable("male", 0.03, out5 = FALSE)$tables[[1]]
  expect_equal(nrow(t1), 111)
  expect_equal(t1$x, 0:110)
})

test_that("life table columns are consistent with each other and with qx()", {
  for (out5 in c(TRUE, FALSE)) {
    lt <- lifetable("female", 0.05, am = 0.2, out5 = out5, radix = 1000)
    t <- lt$tables[[1]]
    k <- nrow(t)
    expect_equal(t$lx[1], 1000)
    expect_equal(sum(t$ndx), 1000)
    expect_equal(t$lx[-1], t$lx[-k] - t$ndx[-k])
    expect_equal(t$Tx[1], sum(t$nLx))
    expect_equal(t$ex, t$Tx / t$lx)
    expect_equal(t$nmx, t$ndx / t$nLx)
    expect_equal(t$nLx[-k], t$n[-k] * t$lx[-1] + t$nax[-k] * t$ndx[-k])
    q <- as.matrix(qx("female", 0.05, am = 0.2, out5 = out5))[, 1]
    expect_equal(t$nqx[1:length(q)], unname(q))
    expect_equal(lt$summary$cm.lt, 0.05)
    expect_equal(lt$summary$am.lt, 0.2)
  }
})

test_that("nax follows Coale-Demeny at ages 0 and 1-4, then 2.5 or 0.5", {
  t <- lifetable("female", 0.05)$tables[[1]]
  q0 <- t$nqx[1]
  expect_equal(t$nax[1], 0.05 + 3.0 * q0)
  expect_equal(t$nax[2], 1.524 - 1.627 * q0)
  expect_equal(t$nax[3:23], rep(2.5, 21))
  t <- lifetable("male", 0.03, out5 = FALSE)$tables[[1]]
  expect_equal(t$nax[1], 0.0425 + 2.875 * t$nqx[1])
  expect_equal(t$nax[2:110], rep(0.5, 109))
  # high infant mortality: the constants
  t <- lifetable("male", 0.4)$tables[[1]]
  expect_gt(t$nqx[1], 0.1)
  expect_equal(t$nax[1:2], c(0.33, 1.352))
  t <- lifetable("female", 0.4)$tables[[1]]
  expect_equal(t$nax[1:2], c(0.35, 1.361))
})

test_that("the open age group continues the death rate at age 109", {
  q1 <- as.matrix(qx("male", 0.03, out5 = FALSE))[, 1]
  m109 <- q1[110] / (1 - 0.5 * q1[110])
  for (out5 in c(TRUE, FALSE)) {
    t <- lifetable("male", 0.03, out5 = out5)$tables[[1]]
    k <- nrow(t)
    expect_equal(unname(t$nmx[k]), unname(m109))
    expect_equal(unname(t$ex[k]), unname(1 / m109))
  }
})

test_that("abridged and single-year tables give nearly the same e0, and radix scales", {
  for (cm in c(0.005, 0.05, 0.2)) {
    e5 <- lifetable("female", cm)$summary$e0
    e1 <- lifetable("female", cm, out5 = FALSE)$summary$e0
    expect_lt(abs(e5 - e1), 0.05)
  }
  a <- lifetable("male", 0.02)$tables[[1]]
  b <- lifetable("male", 0.02, radix = 1)$tables[[1]]
  expect_equal(b$lx * 1e5, a$lx)
  expect_equal(b$ex, a$ex)
})

test_that("lifetable() checks its inputs", {
  expect_error(lifetable("female", 0.05, out5 = NA), "TRUE or FALSE")
  expect_error(lifetable("female", 0.05, radix = 0), "positive")
  expect_error(lifetable("female", 0.0001), "must be between")
  expect_error(lifetable("female", 0.05, file = "lt.pdf"), "xlsx, .csv, .txt or .json")
})

test_that("life tables print in fixed columns", {
  lt <- lifetable("female", 0.05)
  expect_output(print(lt), "Life table cm-0.050")
  expect_output(print(lt), "110\\+")
  lines <- SVDMx:::formatLifetable(lt, 1)
  rows <- lines[(which(lines == "") + 1):length(lines)]
  expect_equal(length(rows), 25)
  expect_equal(length(unique(nchar(rows))), 1)
})

test_that("writeLifetables() writes CSV, text, JSON and Excel files", {
  lt <- lifetable("male", c(0.02, 0.08), am = c(0.15, 0.3))
  d <- tempfile("lt")
  dir.create(d)
  f <- file.path(d, c("lt.csv", "lt.txt"))
  writeLifetables(lt, f)
  csv <- utils::read.csv(f[1], stringsAsFactors = FALSE)
  expect_equal(nrow(csv), 48)
  expect_equal(csv$ex, as.data.frame(lt)$ex)
  txt <- readLines(f[2])
  expect_equal(sum(grepl("^Life table", txt)), 2)
  writeLifetables(lt, file.path(d, "lt.json"))
  j <- jsonlite::read_json(file.path(d, "lt.json"), simplifyVector = TRUE)
  expect_equal(j$summary$e0, lt$summary$e0)
  expect_equal(j$lifetable[[2]]$lx, lt$tables[[2]]$lx)
  writeLifetables(lt, file.path(d, "lt.xlsx"))
  expect_true(file.exists(file.path(d, "lt.xlsx")))
  skip_if_not_installed("readxl")
  expect_equal(readxl::excel_sheets(file.path(d, "lt.xlsx")), c("summary", names(lt$tables)))
  x <- as.data.frame(readxl::read_excel(file.path(d, "lt.xlsx"), sheet = 2))
  expect_equal(x$Tx, lt$tables[[1]]$Tx)
})

test_that("lifetableBatch() makes one life table per row, with settings and labels", {
  inp <- data.frame(country = c("A", "A", "B"), sex = c("female", "M", "f"),
                    "5q0" = c(0.02, 0.025, 0.1), "45q15" = c(0.08, NA, 0.3),
                    out5 = c(TRUE, FALSE, NA), check.names = FALSE)
  b <- lifetableBatch(inp)
  expect_equal(nrow(b$summary), 3)
  expect_equal(b$summary$id, c("LT1", "LT2", "LT3"))
  expect_equal(b$summary$country, c("A", "A", "B"))
  expect_equal(b$summary$sex, c("female", "male", "female"))
  expect_equal(b$summary$table, c("abridged", "single-year", "abridged"))
  expect_true(is.na(b$summary$am[2]))
  expect_equal(b$tables[[1]], lifetable("female", 0.02, am = 0.08)$tables[[1]])
  expect_equal(b$tables[[2]], lifetable("male", 0.025, out5 = FALSE)$tables[[1]])
  # the same from a CSV file and a tab-separated text file
  d <- tempfile("batch")
  dir.create(d)
  utils::write.csv(inp, file.path(d, "in.csv"), row.names = FALSE, na = "")
  expect_equal(lifetableBatch(file.path(d, "in.csv"))$tables, b$tables)
  utils::write.table(inp, file.path(d, "in.txt"), sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  expect_equal(lifetableBatch(file.path(d, "in.txt"))$tables, b$tables)
  # defaults for settings the input does not give
  expect_equal(lifetableBatch(inp[, 1:4], out5 = FALSE)$summary$table, rep("single-year", 3))
})

test_that("lifetableBatch() reports unusable rows and collects warnings", {
  expect_error(lifetableBatch(data.frame(sex = c("female", "male"), cm = c(0.05, 0.0005))),
               "1 of 2 rows")
  expect_error(lifetableBatch(data.frame(sex = "female", cm = "x")), "cannot be read")
  expect_error(lifetableBatch(data.frame(sex = "female")), "columns for sex and 5q0")
  expect_error(lifetableBatch(data.frame(id = c("a", "a"), sex = "female", cm = 0.05)), "distinct")
  expect_warning(lifetableBatch(data.frame(sex = "male", cm = 0.002, match.inputs = FALSE)),
                 "warning\\(s\\) for individual rows")
})

test_that("lifetableBatch() reads Excel input when readxl is installed", {
  skip_if_not_installed("readxl")
  inp <- data.frame(id = c("a", "b"), sex = c("female", "male"), cm = c(0.02, 0.05), am = c(0.1, NA))
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(inp, f)
  expect_equal(lifetableBatch(f)$tables, lifetableBatch(inp)$tables)
})
