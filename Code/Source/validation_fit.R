validation_fit <- function(fit, n_warmup, Data) {
  nresp <- length(Data$y)           # Number of respondents.
  nhold <- length(Data$y_ho)        # Number of hold-out respondents.
  nscns <- length(Data$y[[1]])      # Number of choice tasks per respondent.
  nalts <- nrow(Data$X[[1]])/nscns  # Number of product alternatives per choice task.
  nvars <- ncol(Data$X[[1]])        # Number of (estimable) attribute levels.
  ncovs <- ncol(Data$Z)             # Number of respondent-level covariates.
  ndraw <- dim(fit$betadraw)[3]     # Number of total draws.
  
  # Prepare for computing model fit.
  post_burnin <- length(n_warmup:ndraw)
  # model_fit <- vector(mode = "double", length = 5)
  model_fit <- vector(mode = "double", length = 4)
  
  # # LMD.
  # model_fit[1] <- round(logMargDenNR(fit$llikedraw[n_warmup:ndraw]), 3)
  
  # # DIC.
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
  # mean_resp = rep(NA,nresp)
  # for (resp in 1:nresp) {
  #   mean_draw = rep(NA,post_burnin)
  #   for (draw in n_warmup:ndraw) {
  #     mean_draw[draw-(n_warmup-1)] = ll_mnl(fit$betadraw[resp,,draw],Data$y[[resp]],Data$X[[resp]])
  #   }
  #   mean_resp[resp] = mean(mean_draw)
  # }
  # mean_dev = mean(mean_resp)
  # dev_mean = mean(fit$llikedraw[n_warmup:ndraw])
  # model_fit[2] <- round((2*mean_dev) - (4*dev_mean), 3)
  
  # # WAIC.
  # model_fit[3] <- round(loo::waic(matrix(fit$llikedraw[n_warmup:ndraw], nrow = post_burnin, ncol = 1))$waic, 3)
  
  # HR and HP for in-sample respondents.
  llike <- array(NA, dim = c(post_burnin, nscns, nresp))
  hits <- matrix(NA, nrow = post_burnin, ncol = nscns)
  prob <- matrix(NA, nrow = nresp, ncol = 1)
  for (resp in 1:nresp) {
    prob_resp <- matrix(NA, nrow = nscns, ncol = 1)
    for (scn in 1:nscns) {
      Y_scn <- Data$y[[resp]][scn]
      X_scn <- Data$X[[resp]][((nalts * scn) - (nalts - 1)):(nalts * scn), ]
      Y_predict <- matrix(NA, nrow = post_burnin, ncol = 1)
      prob_draw <- matrix(NA, nrow = post_burnin, ncol = 1)
      for (draw in 1:post_burnin) {
        # Determine draw-specific log likelihood, predicted Y, and predicted probability.
        llike[draw, scn, resp] <- ll_mnl(fit$betadraw[resp,,draw], Y_scn, X_scn)
        Y_predict[draw, ] <- which.max(X_scn %*% fit$betadraw[resp,,draw])
        prob_draw[draw, ] <- exp(llike[draw, scn, resp])
      }
      # Identify hits and average the predicted probabilities.
      hits[, scn] <- (rep(Y_scn, post_burnin) == as.vector(Y_predict)) * 1
      prob_resp[scn] <- sum(prob_draw) / post_burnin
    }
    # Average the predicted probabilities and print progress.
    prob[resp, ] <- sum(prob_resp) / nscns
    print(paste(round(resp / nresp, 2) * 100, "Percent Done"))
  }
  model_fit[1] <- round(sum(hits) / length(hits), 3)
  model_fit[2] <- round(sum(prob) / nresp, 3)
  
  # Draw hold-out sample betas.
  beta_hold <- array(NA, dim = c(nhold, nvars, post_burnin))
  for (resp in 1:nhold) {
    for (rep in n_warmup:ndraw) {
      Gammadraw <- matrix(fit$Gammadraw[rep, ], ncol = nvars)
      Vbetadraw <- matrix(fit$Vbetadraw[rep, ], ncol = nvars)
      beta_hold[resp, , (rep - n_warmup + 1)] <- rmvnorm(
        1,
        mean = t(Gammadraw) %*% matrix(Data$Z_ho, ncol = ncovs)[resp, ],
        sigma = Vbetadraw
      )
    }
  }
  
  # HR and HP for hold-out respondents.
  llike <- array(NA, dim = c(post_burnin, nscns, nhold))
  hits <- matrix(NA, nrow = post_burnin, ncol = nscns)
  prob <- matrix(NA, nrow = nhold, ncol = 1)
  for (resp in 1:nhold) {
    prob_resp <- matrix(NA, nrow = nscns, ncol = 1)
    for (scn in 1:nscns) {
      Y_scn <- Data$y_ho[[resp]][scn]
      X_scn <- Data$X_ho[[resp]][((nalts * scn) - (nalts - 1)):(nalts * scn), ]
      Y_predict <- matrix(NA, nrow = post_burnin, ncol = 1)
      prob_draw <- matrix(NA, nrow = post_burnin, ncol = 1)
      for (draw in 1:post_burnin) {
        # Determine draw-specific log likelihood, predicted Y, and predicted probability.
        llike[draw, scn, resp] <- ll_mnl(beta_hold[resp, , draw], Y_scn, X_scn)
        Y_predict[draw, ] <- which.max(X_scn %*% beta_hold[resp, , draw])
        prob_draw[draw, ] <- exp(llike[draw, scn, resp])
      }
      # Identify hits and average the predicted probabilities.
      hits[, scn] <- (rep(Y_scn, post_burnin) == as.vector(Y_predict)) * 1
      prob_resp[scn] <- sum(prob_draw) / post_burnin
    }
    # Average the predicted probabilities and print progress.
    prob[resp, ] <- sum(prob_resp) / nscns
    print(paste(round(resp / nhold, 2) * 100, "Percent Done"))
  }
  # model_fit[4] <- round(sum(hits) / length(hits), 3)
  # model_fit[5] <- round(sum(prob) / nhold, 3)
  model_fit[3] <- round(sum(hits) / length(hits), 3)
  model_fit[4] <- round(sum(prob) / nhold, 3)
  
  # Return.
  model_fit
}
