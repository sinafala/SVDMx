#' Generate 1qx or 5qx age schedule of mortality from child or child/adult mortality
#'
#' @description Generate single-year 1qx or 5-year 5qx age-specific probabilities of dying using the SVD-Comp mortality model indexed by child mortality, 5q0, or child and adult mortality, 5q0 and 45q15.
#' @author Samuel J. Clark, \email{work@samclark.net}
#' @references Clark (2016) \doi{doi:10.48550/arXiv.1612.01408} and Clark (2019) \doi{doi:10.1007/s13524-019-00785-3}

#'
#' @param sex Character: 'female' or 'male' (not case sensitive).
#' @param cm Decimal: the input value(s) for 5q0, between 0.003 and 0.3; either a single value or a vector of values.
#' @param smooth Boolean: use either smooth or raw SVD-derived components. Default is TRUE.
#' @param outlogit Boolean: output either logit-scale or natural-scale nqx values. Default is FALSE. When TRUE the oldest age group is not returned because logit(1) is not defined.
#' @param out5 Boolean: output either 5-year or 1-year age groups. Default is TRUE.
#' @param am Optional decimal: input value(s) for 45q15, between 0 and 1; either a single value or a vector of values. If a vector, must have the same number of elements as cm. If not given, 45q15 is predicted from 5q0.
#' @param modsv Optional integer: the version of the calibration models to use, named for the year of the Human Mortality Database (HMD) they were estimated from: 2018 (the models in the \emph{Demography} paper), 2021, 2022, 2024, 2025, or 2026. Default is 2026. See \code{\link{mods2026}} and the other \code{mods} data sets.
#' @return Data frame: generated 1qx (ages 0 to 109) or 5qx (age groups 0, 1-4, 5-9, ..., 105-109, 110+) values. For 5-year output the oldest age group, 110+, is 1.0. Columns labeled with the input child and adult mortality values.
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
    stop('<sex> (female or male) and <cm> (5q0, a number between 0.003 and 0.3) must both be given')
  if (!is.character(sex) || length(sex) != 1 || !(tolower(sex) %in% c("female","male")))
    stop('<sex> must be "female" or "male"')
  sex <- tolower(sex)
  if (!is.numeric(cm) | length(cm) == 0)
    stop('<cm> (5q0) must be a number or a vector of numbers between 0.003 and 0.3')
  if (any(is.na(cm)) | any(cm < 0.003) | any(cm > 0.3))
    stop('<cm> (5q0) must be between 0.003 and 0.3, and not missing')
  if (!is.null(am)) {
    if (!is.numeric(am))
      stop('<am> (45q15) must be a number or a vector of numbers between 0 and 1')
    if (length(cm) != length(am))
      stop('<am> must be same length as <cm>')
    if (any(is.na(am)) | any(am <= 0) | any(am >= 1))
      stop('<am> (45q15) must be between 0 and 1, and not missing')
  }

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

  # label the return object r.p
  r.p <- data.frame(r.p)
  cm.col <- format(round(cm, 3), nsmall = 3)
  am.col <- format(round(am, 3), nsmall = 3)
  colnames(r.p) <- paste("cm-",cm.col,".am-",am.col,sep="")
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
