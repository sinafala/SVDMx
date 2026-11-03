# Ranges of child mortality (5q0) and adult mortality (45q15) in the Human Mortality
# Database (HMD) life tables used to estimate each calibration, by sex. qx() does not
# extrapolate beyond them. The limits are rounded outward to four significant digits.
# The HMD data behind the 2021 calibration (February 2021) are no longer available, so
# its ranges are those spanned by the 2018 and 2022 data.
# Not exported; the same values are listed in the documentation of qx().
calibrationRanges <- data.frame(
  modsv = c(2018, 2018, 2021, 2021, 2022, 2022, 2024, 2024, 2025, 2025, 2026, 2026),
  sex = rep(c("female", "male"), 6),
  cm.min = c(0.0008728, 0.001332, 0.0004839, 0.001332, 0.0004839, 0.001332,
             0.0004839, 0.001342, 0.0004839, 0.001342, 0.0004839, 0.001342),
  cm.max = rep(c(0.5903, 0.6087), 6),
  am.min = c(0.03438, 0.05821, 0.03221, 0.05463, 0.03221, 0.05463,
             0.03221, 0.05463, 0.03074, 0.05185, 0.03074, 0.05185),
  am.max = rep(c(0.7842, 0.8649), 6),
  stringsAsFactors = FALSE
)
