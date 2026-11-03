#' SVD-Comp models data set - 'mods2018'
#'
#' @description An R object containing a hierarchy of lists that contain SVD-derived components,
#'     estimated model coefficients, and other parameter
#'     values necessary to generate new 1qx values using the SVD Component
#'     mortality model indexed by child/child-adult mortality implemented by
#'     the 'qx()' function.  The model objects
#'     have been 'cleaned' to remove large collections
#'     of data that are not necessary to perform predictions - this dramatically
#'     reduces their size. This is the 2018 version of the models, estimated from 4,610 life tables for each sex in the
#'     Human Mortality Database as downloaded on November 2, 2018. These are the models in the
#'     \emph{Demography} paper. (In version 0.1.0 of this package the object called 'mods2018'
#'     was the 2021 version, now called 'mods2021'.)
#' @author Samuel J. Clark, \email{work@samclark.net}
#'
#' @format An R list object with members:
#' \describe{
#'   \item{\strong{female}:}{
#'       \describe{
#'         \item{\strong{components}: 4 raw SVD-derived components}{}
#'         \item{\strong{components.sm}: 4 smoothed SVD-derived components}{}
#'         \item{\strong{aml}: lm() model object for adult mortality model}{}
#'         \item{\strong{v1}: lm() model object for v1}{}
#'         \item{\strong{v2}: lm() model object for v2}{}
#'         \item{\strong{v3}: lm() model object for v3}{}
#'         \item{\strong{v4}: lm() model object for v4}{}
#'         \item{\strong{offset}: offset used when calculating SVD}{}
#'         \item{\strong{q0}: lm() model object for mortality at age 0}{}
#'         \item{\strong{rownames}: row labels for the predicted values}{}
#'       }
#'     }
#'   \item{\strong{male}:}{
#'       \describe{
#'         \item{\strong{components}: 4 raw SVD-derived components}{}
#'         \item{\strong{components.sm}: 4 smoothed SVD-derived components}{}
#'         \item{\strong{aml}: lm() model object for adult mortality model}{}
#'         \item{\strong{v1}: lm() model object for v1}{}
#'         \item{\strong{v2}: lm() model object for v2}{}
#'         \item{\strong{v3}: lm() model object for v3}{}
#'         \item{\strong{v4}: lm() model object for v4}{}
#'         \item{\strong{offset}: offset used when calculating SVD}{}
#'         \item{\strong{q0}: lm() model object for mortality at age 0}{}
#'         \item{\strong{rownames}: row labels for the predicted values}{}
#'       }
#'     }
#' }
#' @source See model development in Clark (2016) \doi{doi:10.48550/arXiv.1612.01408} and Clark (2019) \doi{doi:10.1007/s13524-019-00785-3}
"mods2018"

# load the model data: svd components and regression coefficients
load("./data/mods2018.RData")
