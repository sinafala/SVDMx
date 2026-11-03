# Make generated schedules reproduce their inputs exactly: an addition to the model in
# Clark (2019), using the method developed for the MDMx model. Not exported; see the
# details of ?qx.
#
# z: ages x schedules matrix of predicted logit(1qx), ages 0-109, after the offset and the
#    separate prediction of 1q0
# cs: the 4 x 110 matrix of (raw or smoothed) components used to generate z
# cm, am: the inputs, 5q0 and (optionally, otherwise NULL) 45q15, one value per schedule
#
# For each schedule the change is a combination of one direction per input: the gradient of
# logit(5q0) or logit(45q15) with respect to the logit(1qx) schedule, projected onto the span
# of the components. To first order this is the smallest change of the schedule (sum of
# squared changes in logit(1qx)) that removes the differences, and the corrected schedule is
# still a combination of the components. The coefficients of the directions solve the
# matching equations exactly by Newton's method, halving a step whenever it would not reduce
# the largest difference.
matchInputs <- function(z,cs,cm,am=NULL,tol=1e-10,max.iter=50) {

  # orthonormal basis of the span of the components (110 x 4)
  Q <- qr.Q(qr(t(cs)))
  # rows of the indicators' age windows: ages 0-4 for 5q0 and 15-59 for 45q15
  windows <- list(1:5,16:60)
  targets <- rbind(cm,am)
  m <- nrow(targets)

  # nqx implied by a logit(1qx) schedule over an age window
  nqx <- function(zz,w) {
    -expm1(sum(log1p(-expit(zz[w]))))
  }
  # differences between the implied and the input values, on the logit scale
  f <- function(zz,t) {
    sapply(1:m,function(j) logit(nqx(zz,windows[[j]]))) - t
  }
  # gradients of logit(nqx) with respect to logit(1qx): 1qx/nqx within the window, 0 outside
  grad <- function(zz) {
    sapply(1:m,function(j) {
      g <- numeric(length(zz))
      w <- windows[[j]]
      g[w] <- expit(zz[w])/nqx(zz,w)
      g
    })
  }

  converged <- rep(TRUE,ncol(z))
  for (k in seq_len(ncol(z))) {
    z0 <- z[,k]
    t <- logit(targets[,k])
    # directions: the gradients projected onto the components, fixed at the starting schedule
    D <- matrix(Q %*% crossprod(Q,grad(z0)),ncol=m)
    mu <- numeric(m)
    fz <- f(z0,t)
    it <- 0
    while (max(abs(fz)) >= tol & it < max.iter) {
      it <- it + 1
      J <- crossprod(matrix(grad(z0 + D %*% mu),ncol=m),D)
      step <- -solve(J,fz)
      lambda <- 1
      repeat {
        mu.new <- mu + lambda*step
        f.new <- f(z0 + D %*% mu.new,t)
        if ((all(is.finite(f.new)) && max(abs(f.new)) < max(abs(fz))) | lambda < 1e-10) break
        lambda <- lambda/2
      }
      mu <- mu.new
      fz <- f.new
    }
    z[,k] <- z0 + D %*% mu
    converged[k] <- max(abs(fz)) < tol
  }

  return(list(z=z,converged=converged))

}
