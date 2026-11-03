## SVDMx: the SVD-Comp mortality model indexed by child or child/adult mortality

`qx()` generates complete age schedules of mortality, single-year 1qx or five-year 5qx,
from child mortality (5q0) alone or from child and adult mortality (5q0 and 45q15).

```r
install.packages("SVDMx")
library(SVDMx)
qx("female", 0.05)                 # 5qx from 5q0 = 0.05
qx("male", 0.03, am = 0.26)        # from 5q0 and 45q15
qx("female", 0.05, out5 = FALSE)   # single-year 1qx
qx("female", 0.05, modsv = 2018)   # the models in the Demography paper
```

The models are calibrated on the Human Mortality Database (HMD). Each calibration is
included and named for the HMD version it was estimated from: `mods2018` (the
*Demography* paper), `mods2021`, `mods2022`, `mods2024`, `mods2025`, and `mods2026`
(the default).

Model and code: Clark (2019) <https://doi.org/10.1007/s13524-019-00785-3>, preprint
<https://arxiv.org/abs/1612.01408>, and the replication materials at
<https://github.com/sinafala/svd-comp>.
