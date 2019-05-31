model_fit <- function(fit, n_warmup, Data) {
  nresp <- length(Data$y)           # Number of respondents.
  nscns <- length(Data$y[[1]])      # Number of choice tasks per respondent.
  nalts <- nrow(Data$X[[1]])/nscns  # Number of product alternatives per choice task.
  nvars <- ncol(Data$X[[1]])        # Number of (estimable) attribute levels.
  ncovs <- ncol(Data$Z)             # Number of respondent-level covariates.
  ndraw <- dim(fit$betadraw)[3]     # Number of total draws.
  
  # Prepare for computing in-sample fit.
  post_burnin <- length(n_warmup:ndraw)
  model_fit <- vector(mode = "double", length = 3)
  
  # LMD.
  model_fit[1] <- logMargDenNR(fit$llikedraw[n_warmup:ndraw])
  
  # Deviance information criterion.
  ll_mnl <- function (beta, y, X) {
    nvars = ncol(X)        # Number of attribute levels.
    nscns = length(y)      # Number of choice tasks.
    nalts = nrow(X)/nscns  # Number of alternatives.
    # Compute Xbeta across all choice tasks.
    Xbeta = matrix(exp(X%*%beta),byrow=TRUE,ncol=nalts)
    # Numerator: Xbeta values associated with each choice.
    choices = cbind(c(1:nscns),y)
    numerator = Xbeta[choices]
    # Denominator: Xbeta values associated with each task.
    iota = c(rep(1,nalts))
    denominator = Xbeta%*%iota
    # Return the logit summed across choice tasks.
    return(sum(log(numerator) - log(denominator)))
  }
  mean_resp = rep(NA,nresp)
  for (resp in 1:nresp) {
    mean_draw = rep(NA,post_burnin)
    for (draw in n_warmup:ndraw) {
      mean_draw[draw-(n_warmup-1)] = ll_mnl(fit$betadraw[resp,,draw],Data$y[[resp]],Data$X[[resp]])
    }
    mean_resp[resp] = mean(mean_draw)
  }
  mean_dev = mean(mean_resp)
  dev_mean = mean(fit$llikedraw[n_warmup:ndraw])
  model_fit[2] <- (2*mean_dev) - (4*dev_mean)
  
  # WAIC.
  model_fit[3] <- loo::waic(matrix(fit$llikedraw[n_warmup:ndraw], nrow = post_burnin, ncol = 1))$waic
  
  # Return
  model_fit
}
