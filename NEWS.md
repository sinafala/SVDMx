# SVDMx 0.3.0

* New argument `match.inputs` in `qx()`, `TRUE` by default: each generated schedule is changed
  as little as possible, within the span of the model's components, so that it reproduces its
  inputs exactly: its 5q0 equals `cm` and, when `am` is given, its 45q15 equals `am`. This is
  an addition to the model in Clark (2019). For the life tables in the October 2026 HMD it
  reduces the mean absolute error of e0, most of all when 45q15 is given (from 0.65 to 0.58
  years for females and from 0.66 to 0.54 years for males), and it removes most of the bias
  in e0. See `?qx`.
* **Results change by default.** `match.inputs = FALSE` gives the model as published in Clark
  (2019) and the results of earlier versions of the package.
* The warning that a generated schedule departs from its inputs by more than 10% (0.2.1) now
  applies only with `match.inputs = FALSE`.
* New function `plotqx()` plots the schedules returned by `qx()` on the natural or the logit
  scale, and optionally saves the plot as a PNG or PDF file.

# SVDMx 0.2.1 (not released on CRAN; included in 0.3.0)

* `qx()` accepts a much wider range of inputs. Versions 0.1.0 and 0.2.0 stopped for 5q0
  outside 0.003-0.3. `qx()` now accepts any 5q0, and 45q15 if given, within the range of
  the life tables the selected calibration was estimated from: for example, 5q0 from 0.00048
  to 0.59 for females with the 2026 calibration. This covers the lowest levels of child
  mortality in the Human Mortality Database today. Inputs outside that range are still an
  error, because the models do not extrapolate well. See `?qx` for the ranges.
* `qx()` now warns when the 5q0 of a generated schedule, or its 45q15 when `am` is given,
  differs from the input by more than 10%. SVD-Comp does not force a generated schedule to
  reproduce its inputs exactly. The difference is small for 5q0 between about 0.004 and 0.4,
  and it grows toward the extremes.
* Fixed: different inputs that round to the same three decimal places, such as 5q0 = 0.0301
  and 0.0304, gave output columns with the same label. Labels now use more decimal places
  when needed; labels for all other inputs are unchanged.
* Documentation: the input ranges for each calibration, how close the generated schedules
  are to their inputs, what the column labels show, `outlogit`, and the `smooth` setting
  used in Clark (2019).

# SVDMx 0.2.0

* New calibration of the models from the October 29, 2026 version of the Human Mortality
  Database (HMD), `mods2026`, which `qx()` now uses by default.
* Every calibration is now included and documented, each named for the HMD version it was
  estimated from: `mods2018` (4,610 life tables per sex, the models in the *Demography* paper),
  `mods2021` (4,765), `mods2022` (4,857), `mods2024` (4,902), `mods2025` (4,946), and
  `mods2026` (4,987).
* `mods2018` is now the model in the *Demography* paper. The object called `mods2018` in
  version 0.1.0 was estimated from the HMD as downloaded in February 2021 and is now `mods2021`.
* Fixed: `qx(..., modsv = 2018)` returned the 2024 models; any other unrecognized `modsv`
  also silently returned the 2024 models. An unknown `modsv` is now an error.
* Fixed: `sex` is no longer case sensitive (`"Female"` used to pass the checks and then fail).
* `qx()` now checks its inputs: missing values in `cm` or `am`, `am` outside 0-1, and
  `am = NULL` given explicitly are handled with clear messages.
* Fixed: `q1to5()` accepts a single vector, as documented, and checks that 110 single-year
  age groups (0-109) are supplied.
* Documentation of the `mods` data sets now gives the correct element names
  (`components`, `components.sm`, `female`, `male`).
* Added tests.

# SVDMx 0.1.0

* First CRAN release.
