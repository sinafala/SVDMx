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
