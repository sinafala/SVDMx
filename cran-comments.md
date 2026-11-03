## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update of an existing package (0.2.0 -> 0.2.1).
* qx() now accepts inputs over the whole range of the data each calibration was estimated
  from (versions 0.1.0 and 0.2.0 stopped outside 0.003-0.3, which excluded the lowest
  current levels of child mortality). It also warns when a generated schedule departs from
  its inputs, and it gives distinct column labels to distinct inputs.
* 'SVD' stands for 'singular value decomposition'. 'nqx' is standard notation in demography for
  the conditional probability of dying between ages x and x+n for those who survive to age x.
