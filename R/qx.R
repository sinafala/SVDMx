#' Generate 1qx or 5qx age schedule of mortality from child or child/adult mortality
#'
#' @description Generate single-year 1qx or 5-year 5qx age-specific probabilities of dying using the SVD-Comp mortality model indexed by child mortality, 5q0, or child and adult mortality, 5q0 and 45q15.
#' @author Samuel J. Clark, \email{work@samclark.net}
#' @references Clark (2016) \doi{doi:10.48550/arXiv.1612.01408} and Clark (2019) \doi{doi:10.1007/s13524-019-00785-3}

#'
#' @param sex Character: 'female' or 'male' (not case sensitive).
#' @param cm Decimal: the input value(s) for 5q0; either a single value or a vector of values. Must lie within the range of 5q0 in the life tables the selected calibration was estimated from (see Details).
#' @param smooth Boolean: use either smooth or raw SVD-derived components. Default is TRUE. The validation results in Clark (2019) use the raw components (FALSE).
#' @param outlogit Boolean: output either logit-scale or natural-scale nqx values. Default is FALSE. With 5-year output (out5 = TRUE) the open age group 110+ is not returned on the logit scale because logit(1) is not defined.
#' @param out5 Boolean: output either 5-year or 1-year age groups. Default is TRUE.
#' @param am Optional decimal: input value(s) for 45q15; either a single value or a vector of values. If a vector, must have the same number of elements as cm. Must lie within the range of 45q15 in the life tables the selected calibration was estimated from (see Details). If not given, 45q15 is predicted from 5q0.
#' @param modsv Optional integer: the version of the calibration models to use, named for the year of the Human Mortality Database (HMD) they were estimated from: 2018 (the models in the \emph{Demography} paper), 2021, 2022, 2024, 2025, or 2026. Default is 2026. See \code{\link{mods2026}} and the other \code{mods} data sets.
#' @return Data frame: generated 1qx (ages 0 to 109) or 5qx (age groups 0, 1-4, 5-9, ..., 105-109, 110+) values. For 5-year output the oldest age group, 110+, is 1.0. Each column is labeled with its input value of 5q0 and with the input value of 45q15 or, when am is not given, the value predicted from 5q0 (not the 45q15 of the generated schedule). The labels have three decimal places, more for values below 0.001 or when needed to keep the labels of different inputs distinct.
#'
#' @details
#' **Range of the inputs.** `qx()` does not extrapolate beyond the life tables the models were
#' estimated from: `cm`, and `am` if given, must lie within the range of 5q0 and 45q15 in the
#' Human Mortality Database life tables used to estimate the selected calibration:
#'
#' | `modsv` | sex | 5q0 | 45q15 |
#' |:---|:---|:---|:---|
#' | 2018 | female | 0.0008728 -- 0.5903 | 0.03438 -- 0.7842 |
#' | 2018 | male | 0.001332 -- 0.6087 | 0.05821 -- 0.8649 |
#' | 2021, 2022 | female | 0.0004839 -- 0.5903 | 0.03221 -- 0.7842 |
#' | 2021, 2022 | male | 0.001332 -- 0.6087 | 0.05463 -- 0.8649 |
#' | 2024 | female | 0.0004839 -- 0.5903 | 0.03221 -- 0.7842 |
#' | 2024 | male | 0.001342 -- 0.6087 | 0.05463 -- 0.8649 |
#' | 2025, 2026 | female | 0.0004839 -- 0.5903 | 0.03074 -- 0.7842 |
#' | 2025, 2026 | male | 0.001342 -- 0.6087 | 0.05185 -- 0.8649 |
#'
#' The data behind the 2021 calibration are no longer available; its ranges are those
#' spanned by the 2018 and 2022 data.
#'
#' **Agreement with the inputs.** SVD-Comp predicts the whole age schedule from 5q0 (and
#' 45q15), but it does not force the generated schedule to reproduce the input values
#' exactly. For 5q0 between about 0.004 and 0.4 the 5q0 of the generated schedule is within
#' about 9% of the input. The difference grows toward the extremes: at the lowest observed
#' levels of 5q0 (0.002--0.003) the generated 5q0 is about 8--21% too low, depending on the
#' calibration and sex, and above 5q0 = 0.4 it is increasingly too high. `qx()` gives a
#' warning when the 5q0 of a generated schedule, or its 45q15 when `am` is given, differs
#' from the input by more than 10%. For the life tables in the October 2026 HMD, that happens
#' for about 2% of the tables when only 5q0 is given, and for about 4--7% when both 5q0 and
#' 45q15 are given.
#'
#' @examples
#' qx("female",0.05)
#' qx("female",0.05,modsv=2018)
#' qx("male",0.03,am=0.26)
#' qx("male",0.03,TRUE,TRUE,TRUE,0.26)
#' qx("male",c(0.03,0.01))
#' @importFrom stats predict
#' @export
qx <- function(sex,cm,smooth=TRUE,outlogit=FALSE,out5=TRUE,am=NULL,modsv=2026) {

  # sex: female or male
  # cm is a vector of 5q0 values
  # smooth: boolean, use smooth components or not
  # logit: boolean, use to return logit(qx) values
  # out5: boolean, output in 5-year age groups 5qx
  # am is a vector of 45q15 values - if am not supplied, then aml predicted from cml
  # modsv: model version is 2018, 2021, 2022, 2024, 2025, or 2026, defaults to 2026

  # ensure essential parameters have reasonable values
  if (missing(sex) | missing(cm))
    stop('<sex> (female or male) and <cm> (5q0) must both be given')
  if (!is.character(sex) || length(sex) != 1 || !(tolower(sex) %in% c("female","male")))
    stop('<sex> must be "female" or "male"')
  sex <- tolower(sex)

  # set mods: the calibration models for the requested version, 2026 unless otherwise specified
  if (length(modsv) != 1 || is.na(modsv))
    stop('<modsv> must be one of 2018, 2021, 2022, 2024, 2025, or 2026')
  if (modsv == 2018) {
    mods <- mods2018
  } else if (modsv == 2021) {
    mods <- mods2021
  } else if (modsv == 2022) {
    mods <- mods2022
  } else if (modsv == 2024) {
    mods <- mods2024
  } else if (modsv == 2025) {
    mods <- mods2025
  } else if (modsv == 2026) {
    mods <- mods2026
  } else {
    stop('<modsv> must be one of 2018, 2021, 2022, 2024, 2025, or 2026')
  }

  # inputs must lie within the range of the life tables used to estimate these models
  lim <- calibrationRanges[calibrationRanges$modsv == modsv & calibrationRanges$sex == sex,]
  if (!is.numeric(cm) | length(cm) == 0)
    stop('<cm> (5q0) must be a number or a vector of numbers')
  if (any(is.na(cm)))
    stop('<cm> (5q0) must not be missing')
  if (any(cm < lim$cm.min) | any(cm > lim$cm.max))
    stop(paste('<cm> (5q0) must be between ',lim$cm.min,' and ',lim$cm.max,' for ',sex,'s with modsv = ',modsv
               ,', the range of 5q0 in the life tables used to estimate those models',sep=''))
  am.given <- !is.null(am)
  if (am.given) {
    if (!is.numeric(am))
      stop('<am> (45q15) must be a number or a vector of numbers')
    if (length(cm) != length(am))
      stop('<am> must be same length as <cm>')
    if (any(is.na(am)))
      stop('<am> (45q15) must not be missing')
    if (any(am < lim$am.min) | any(am > lim$am.max))
      stop(paste('<am> (45q15) must be between ',lim$am.min,' and ',lim$am.max,' for ',sex,'s with modsv = ',modsv
                 ,', the range of 45q15 in the life tables used to estimate those models',sep=''))
  }

  # transform inputs
  cml <- logit(cm)
  cmls <- cml^2
  cmlc <- cml^3
  # if no adult mortality specified, predict adult mortality
  if (is.null(am)) {
    preds.aml <- data.frame(
      cm = as.numeric(cm),
      cml = as.numeric(cml),
      cmls = as.numeric(cmls),
      cmlc = as.numeric(cmlc)
    )
    aml <- predict(mods[[sex]]$aml,newdata=preds.aml)
    am <- expit(aml)
  } else {
    aml <- logit(am)
  }
  amls <- aml^2
  amlc <- aml^3
  cmlaml <- cml*aml

  # predict vs using child and adult mortality
  preds.vs <- data.frame(
    cm = as.numeric(cm),
    cml = as.numeric(cml),
    cmls = as.numeric(cmls),
    cmlc = as.numeric(cmlc),
    am = as.numeric(am),
    amls = as.numeric(amls),
    amlc = as.numeric(amlc),
    cmlaml = as.numeric(cmlaml)
  )
  v1 <- predict(mods[[sex]]$v1,newdata=preds.vs)
  v2 <- predict(mods[[sex]]$v2,newdata=preds.vs)
  v3 <- predict(mods[[sex]]$v3,newdata=preds.vs)
  v4 <- predict(mods[[sex]]$v4,newdata=preds.vs)

  # pick either raw or smoothed components
  if (smooth) {
    cs <- mods[[sex]]$components.sm
  } else {
    cs <- mods[[sex]]$components
  }

  # calculate predicted qx values
  v <- cbind(v1,v2,v3,v4)
  r.p <- matrix(data=0,ncol=length(cml),nrow=dim(cs)[2])
  for (z in 1:4) {
    r.p <- r.p + cs[z,] %*% t(v[,z])
  }
  r.p <- r.p + mods[[sex]]$offset

  # predict q0 separately
  cmls <- cml^2
  preds.q0 <- data.frame(
    cml = as.numeric(cml),
    cmls = as.numeric(cmls)
  )
  r.p[1,] <- predict(mods[[sex]]$q0,newdata=preds.q0)

  # check how closely the generated schedules reproduce the inputs: SVD-Comp predicts the
  # whole schedule from 5q0 (and 45q15) but does not force it to match them exactly
  q1 <- expit(r.p)
  cm.out <- 1 - apply(1 - q1[1:5,,drop=FALSE],2,prod)
  off <- abs(cm.out/cm - 1) > 0.1
  if (any(off))
    warning(agreementMessage("5q0",cm[off],cm.out[off]),call.=FALSE)
  if (am.given) {
    am.out <- 1 - apply(1 - q1[16:60,,drop=FALSE],2,prod)
    off <- abs(am.out/am - 1) > 0.1
    if (any(off))
      warning(agreementMessage("45q15",am[off],am.out[off]),call.=FALSE)
  }

  # label the return object r.p
  r.p <- data.frame(r.p)
  colnames(r.p) <- qxLabels(cm,am)
  rownames(r.p) <- mods[[sex]]$rownames

  # returns the matrix of predicted qx values
  if (outlogit) {
    if (out5) {
      q5 <- q1to5(expit(r.p))
      logit.q5 <- data.frame(logit(q5)[1:23,])
      rownames(logit.q5) <- rownames(q5)[1:23]
      colnames(logit.q5) <- colnames(r.p)
      return(logit.q5)
    }
    return(r.p)
  } else {
    if (out5) {
      return(q1to5(expit(r.p)))
    }
    return(expit(r.p))
  }

}

# column labels for qx(): the input 5q0 and 45q15 with three decimal places, more for
# values below 0.001 and whenever needed to keep the labels of different inputs distinct
qxLabels <- function(cm,am) {
  d <- 3
  if (min(cm,am) < 0.001) {
    d <- ceiling(-log10(min(cm,am))) + 1
  }
  repeat {
    labels <- paste("cm-",formatC(cm,format="f",digits=d),".am-",formatC(am,format="f",digits=d),sep="")
    if (!anyDuplicated(labels) | d >= 10) {
      return(labels)
    }
    d <- d + 1
  }
}

# warning text for qx() when generated schedules do not reproduce their inputs closely
agreementMessage <- function(name,input,output) {
  n <- length(input)
  show <- 1:min(n,3)
  pairs <- paste(name," = ",signif(input[show],3)," gives ",signif(output[show],3)
                 ," (",sprintf("%+.0f",100*(output[show]/input[show] - 1)),"%)",sep="",collapse="; ")
  paste("the ",name," of the generated schedule differs from the input by more than 10% for "
        ,n," input value(s): ",pairs,if (n > 3) "; ..." else "",". SVD-Comp does not force the "
        ,"generated schedule to reproduce its inputs exactly; the difference is largest at very "
        ,"low and very high mortality. See ?qx.",sep="")
}
