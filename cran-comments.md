## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update of an existing package (0.2.0 -> 0.3.0).
* qx() can now make the generated schedules reproduce their inputs exactly (new argument
  match.inputs, TRUE by default); match.inputs = FALSE gives the previous results. qx() also
  accepts inputs over the whole range of the data each calibration was estimated from
  (0.2.0 stopped outside 0.003-0.3, which excluded the lowest current levels of child
  mortality), and gives distinct column labels to distinct inputs.
* 'SVD' stands for 'singular value decomposition'. 'nqx' is standard notation in demography for
  the conditional probability of dying between ages x and x+n for those who survive to age x.
