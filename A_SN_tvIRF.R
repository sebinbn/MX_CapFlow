SN_tvIRF = function(x, impulse = NULL, response = NULL, n.ahead = 10,y.names,
                    ortho = TRUE,  ortho.cov = c("tv", "const"), 
                    bw.cov = NULL, cumulative = FALSE, unit.shock = FALSE){
  
  IRF = tvIRF(x, cumulative = F, bw.cov = bw.cov, ortho = T, ortho.cov = "tv")
  
  tkernel <- x$tkernel
  est <- x$est
  Sigma.hat <- tvCov(x$residuals, bw = bw.cov, tkernel = tkernel, est = est)
  
  for (i in 1:length(impulse))
  {
    if (isTRUE(unit.shock)) {
      IRF$irf[[i]] <- IRF$irf[[i]]/array(rep(sqrt(Sigma.hat[i,i,]),
                                     length(response)*(n.ahead + 1)),
                                 dim = dim(IRF$irf[[i]]) )          #normalizing IRFs to be 1 unit shock
    }
    if (cumulative)
      IRF$irf[[i]] <- aperm (apply(IRF$irf[[i]], 1:2, cumsum), c(2, 3, 1))
  }
  IRF = c( IRF, list(Sigma.hat = Sigma.hat))
  class(IRF) = "tvirf"
  return(IRF)
}


# Confidence Interval with 1 unit shock -----------------------------------

SN_CI_tvirf <- function(object, parm, level = 0.95, 
                          runs = 100, tboot = c("wild", "wild2") , ...)
{
  if (!inherits(object, "tvirf"))
    stop("\nConfidence intervals not implemented for this class.\n")
  if(runs <= 0)
    stop("\nVariable 'runs' accepts integer values greater than 0.\n")
  if (level <= 0 | level > 1)
    stop("\nVariable 'level' accepts values between 0 and 1.\n")
  tboot <- match.arg(tboot)
  BOOT <- object$BOOT
  if(is.null(BOOT))
  {
    BOOT <- SN_tvboot(x = object, runs = runs, tboot = tboot)
  }
  else
  {
    if (object$tboot != tboot)
    {
      BOOT <- SN_tvboot(x = object, runs = runs, tboot = tboot)
    }
    else if (object$tboot == tboot & object$runs < runs)
    {
      temp <- SN_tvboot(x = object, runs = runs - object$runs, tboot = tboot)
      BOOT <- c(object$BOOT, temp)
    }
  }
  object$level <- level
  object$runs <- runs
  object$tboot <- tboot
  irf <- object$irf
  impulse <- object$impulse
  response <- object$response
  obs <- object$x$obs
  n.ahead <- object$n.ahead
  idx1 <- length(impulse)
  idx2 <- length(response)
  idx3 <- n.ahead + 1
  mat.l <- array(NA, dim = c(obs, idx2, n.ahead + 1))
  mat.u <- array(NA, dim = c(obs, idx2, n.ahead + 1))
  temp <- matrix(NA, nrow = obs, ncol = runs)
  alpha <-  1 - level
  lower <- alpha/2
  upper <- 1 - lower
  Lower <- list()
  Upper <- list()
  for (j in 1:idx1)
  {#impulse
    for (m in 1:idx2)
    {#response
      for (l in 1:idx3)
      {#horizon
        for (i in 1:runs)
        {
          temp[,i] <- BOOT$irf[[i]][[j]][, m, l]/sqrt(Sigma.hat[j,j,])
        }
        sd.star <- apply(temp, 1, stats::sd)
        if (sum(sd.star) == 0)
          c.hat <- apply(temp - irf[[j]][, m, l], 1, stats::quantile,
                         prob = upper, na.rm = TRUE)
        else
          c.hat <- apply(abs(temp - irf[[j]][, m, l])/sd.star, 1, stats::quantile,
                         prob = upper, na.rm = TRUE)
        mat.l[,m, l] <- irf[[j]][, m, l] - c.hat*sd.star
        mat.u[,m, l] <- irf[[j]][, m, l] + c.hat*sd.star
      }
    }
    dimnames(mat.l) <- list(NULL, response, NULL)
    dimnames(mat.u) <- list(NULL, response, NULL)
    Lower[[j]] <- mat.l
    Upper[[j]] <- mat.u
  }
  names(Lower) <- impulse
  names(Upper) <- impulse
  object$BOOT <- BOOT
  object$Lower <- Lower
  object$Upper <- Upper
  return(object)
}

SN_tvboot<-function (x, runs = 0, tboot = "wild", ...)
{
  ortho <- x$ortho
  cumulative <- x$cumulative
  impulse <- x$impulse
  response <- x$response
  bw.cov <- x$bw.cov
  ortho.cov <- x$ortho.cov
  n.ahead <- x$n.ahead
  x <- x$x
  if (inherits(x, "tvvar"))
  {
    VAR <- eval.parent(x)
  }
  else
  {
    stop("Bootstrap not implemented for this class.\n")
  }
  p <- VAR$p
  neq <- VAR$neq
  X <- VAR$x
  obs <- VAR$obs
  type <- VAR$type
  B <- tvBcoef(VAR)
  BOOT <- vector("list", runs)
  ystar <- matrix(0, nrow = VAR$totobs, ncol = neq)
  y.names <- colnames(VAR$y)
  colnames(ystar) <- y.names
  Zdet <- NULL
  if (NCOL(X) > (neq * (p + 1)))
  {#in case there are exogen variables
    Zdet <- as.matrix(X[, (neq * (p + 1) + 1):NCOL(X)])
  }
  resorig <- scale(VAR$residuals, scale = FALSE)
  fitted <- VAR$fitted
  yorig <- VAR$y.orig
  prob <- c(0.7236067977499789360962267892318777740001678466796875,
            0.2763932022500210639037732107681222259998321533203125)
  binar <- c((1-sqrt(5))*0.5, (1+sqrt(5))*0.5)
  for (i in 1:runs)
  {
    if (tboot == "wild")
    {
      resid <- resorig*sample(binar, obs, replace = TRUE, prob = prob)
      lasty <- c(t(yorig[p:1, ]))
      ystar[c(1:p), ] <- yorig[c(1:p), ]
    }
    else if (tboot == "wild2")
    {
      resid  <-resorig*stats::rnorm(obs*neq)
      lasty <- c(t(yorig[p:1, ]))
      ystar[c(1:p), ] <- yorig[c(1:p), ]
    }
    for (j in 1:obs)
    {
      lasty <- lasty[1:(neq * p)]
      Z <- c(lasty, Zdet[j, ])
      if (type == "const")
        Z <- c(Z, 1)
      ystar[j + p, ] <- B[j,,]%*%Z + resid[j, ]
      lasty <- c(ystar[j + p, ], lasty)
    }
    VAR$y.orig <- ystar
    VAR$y <- ystar[-c(1:p),]
    VAR$residuals <- resid
    temp <- stats::embed(ystar, dimension = p + 1)[, -c(1:neq)]
    if(type == "const")
      temp <- cbind(temp, 1L)
    VAR$x[, 1:NCOL(temp)] <- temp
    varboot <- update(VAR)
    result <- SN_tvIRF(x = varboot, impulse = impulse, response = response, y.names = y.names,
                        n.ahead = n.ahead, ortho = ortho, cumulative = cumulative,
                        ortho.cov = ortho.cov, bw.cov = bw.cov)
    BOOT[[i]] = list(irf = result$irf, Sigma.hat = result$Sigma.hat)
  }
  return(BOOT)
}
