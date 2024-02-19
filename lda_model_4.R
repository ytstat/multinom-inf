library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(npcs)
library(naivebayes)
library(doParallel)
library(smotefamily)
library(nnet)
library(stringr)
library(glmnet)
library(pemultinom)
library(HandTill2001)


Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename <- paste("/moto/home/yt2661/work/multinom-inf/experiments/lda_model_4/result/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}

# ---------------------------------------------------------
# define the alternative methods for comparison
multisplits <- function(x, y, lambda = c("lambda.1se", "lambda.min"), ncores = 1, B = 50) {
  lambda <- match.arg(lambda)
  K <- length(unique(y))
  n <- length(y)
  p <- ncol(x)

  p_value_list <- rep(list(matrix(1,nrow = p+1, ncol = B)), K)
  y.fac <- relevel(as.factor(y), ref = K)
  for (b in 1:B) {
    while (1) {
      ind <- sample(n, size = floor(n/2))

      fit_lasso <- cv.pemultinom(x = x[ind, ], y = y[ind], ncores = ncores)

      if (lambda == "lambda.1se") {
        S_active <- which(rowSums(fit_lasso$beta.1se[-1, ] != 0) > 0)
      } else if (lambda == "lambda.min") {
        S_active <- which(rowSums(fit_lasso$beta.min[-1, ] != 0) > 0)
      }

      if (length(S_active) > 0) {
        fit_ols <- multinom(y.fac~x[, S_active, drop = FALSE], trace = FALSE)
        z <- summary(fit_ols)$coefficients/summary(fit_ols)$standard.errors
        pvalue <- (1 - pnorm(abs(z))) * 2
      } else {
        fit_ols <- multinom(y.fac~1, trace = FALSE)
        z <- summary(fit_ols)$coefficients/summary(fit_ols)$standard.errors
        pvalue <- (1 - pnorm(abs(z))) * 2
      }

      if (all(is.finite(summary(fit_ols)$standard.errors))) {
        break
      }


    }

    for (k in 1:(K-1)) {
      p_value_list[[k]][c(1, 1+S_active), b] <- sapply(1:NCOL(pvalue), function(j){
        min(pvalue[k,j]*length(S_active), 1)
      })
    }
  }


  p_value <- sapply(1:(K-1), function(k){
    sapply(1:(p+1), function(j){
      Qj <- sapply(seq(0.05, 1, 0.01), function(gamma){
        min(1, quantile(p_value_list[[k]][j, ]/gamma, gamma, na.rm = TRUE))
      })
      min(1, (1-log(0.05))*min(Qj))
    })
  })

  return(p_value)
}


bootstrap_multinom <- function(x, y, lambda = c("lambda.1se", "lambda.min"), ncores = 1, B = 50, alpha = 0.05,
                               type = c("vector", "residual")) {
  lambda <- match.arg(lambda)
  type <- match.arg(type)
  K <- length(unique(y))
  n <- length(y)
  p <- ncol(x)

  L <- rep(list(matrix(1,nrow = p+1, ncol = B)), K)

  if (type == "vector") {
    for (b in 1:B) {
      while (1) {
        y_b <- 1
        while (length(unique(y_b)) != K) {
          ind_b <- sample(n, size = n, replace = TRUE)
          y_b <- y[ind_b]
          x_b <- x[ind_b, ]
        }
        fit_b <- try(cv.pemultinom(x = x_b, y = y_b, ncores = ncores))

        if (lambda == "lambda.1se") {
          for (k in 1:(K-1)) {
            L[[k]][, b] <- fit_b$beta.1se[, k]
          }
        } else if (lambda == "lambda.min") {
          for (k in 1:(K-1)) {
            L[[k]][, b] <- fit_b$beta.min[, k]
          }
        }


      }
    }
  } else if (type == "residual") {
    fit_initial <- cv.pemultinom(x = x, y = y, ncores = ncores)
    if (lambda == "lambda.1se") {
      beta <- fit_initial$beta.1se
    } else if (lambda == "lambda.min") {
      beta <- fit_initial$beta.min
    }

    prob <- predict_pemultinom(beta = beta, ref = K, xnew = x, type = "prob")

    for (b in 1:B) {

      y_b <- 1
      while (length(unique(y_b)) != K) {
        y_b <- sapply(1:n, function(i){
          sample(1:K, size = 1, prob = prob[i, ])
        })
      }
      fit_b <- try(cv.pemultinom(x = x, y = y_b, ncores = ncores))

      if (lambda == "lambda.1se") {
        for (k in 1:(K-1)) {
          L[[k]][, b] <- fit_b$beta.1se[, k]
        }
      } else if (lambda == "lambda.min") {
        for (k in 1:(K-1)) {
          L[[k]][, b] <- fit_b$beta.min[, k]
        }
      }

    }
  }

  CI <- sapply(1:(K-1), function(k){
    t(sapply(1:(p+1), function(j){
      c(quantile(L[[k]][j, ], alpha/2, na.rm = TRUE), quantile(L[[k]][j, ], 1-alpha/2, na.rm = TRUE))
    }))
  }, simplify = FALSE)


  return(CI)
}

# ------------------------------------------------------------
n <- c(100, 200, 400)
p <- 200
K <- 4
sig.strength <- 1
s <- 3
set.seed(914)
ind1 <- sample(1:p, s)
ind2 <- sample(1:p, s)
ind3 <- sample(1:p, s)


beta_coef <- matrix(0, nrow = p+1, ncol = K-1)
beta_coef[ind1+1, 1] <- c(1, -1, 1)
beta_coef[ind2+1, 2] <- c(1, 1, -1)
beta_coef[ind3+1, 3] <- c(-1, 1, 1)

Sigma <- outer(1:p, 1:p, function(x,y) {
  0.5^(abs(x-y))
})
R <- chol(Sigma)

prior <- c(0.3, 0.2, 0.3, 0.2)
mu <- matrix(0, nrow = p, ncol = K)
mu[, K] <- sample(c(0.5,-0.5)*2, size = p, replace = TRUE)

for (k in 1:(K-1)) {
  mu[, k] <- mu[, K] + Sigma %*% beta_coef[-1, k]
  beta_coef[1, k] <- - t(mu[, k]-mu[, K])%*%solve(Sigma)%*%(mu[, k]+mu[, K])/2 + log(prior[k]/prior[K])
}


cov_prob <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
ci_len <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
fwer <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
mse <- matrix(nrow = length(n), ncol = 2)
ci_test <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)

set.seed(seed, kind = "L'Ecuyer-CMRG")

for (i in 1:length(n)) {

  y <- sample(1:K, size = n[i], replace = TRUE, prob = prior)

  x <- t(sapply(1:n[i], function(j){
    R %*% rnorm(p) + mu[, y[j]]
  }))

  fit_glmnet <- cv.glmnet(x = x, y = y, family = "multinomial")
  lambda.min.ind <- which(fit_glmnet$lambda == fit_glmnet$lambda.min)
  beta_glmnet <- sapply(1:(K-1), function(k){
    c(fit_glmnet$glmnet.fit$a0[k, lambda.min.ind], fit_glmnet$glmnet.fit$beta[[k]][, lambda.min.ind]) -
      c(fit_glmnet$glmnet.fit$a0[K, lambda.min.ind], fit_glmnet$glmnet.fit$beta[[K]][, lambda.min.ind])
  })


  fit <- cv.pemultinom(x, y, nfolds = 5, ncores = ncores)
  beta <- fit$beta.min

  mse[i, 1] <- sum((beta - beta_coef)^2)
  mse[i, 2] <- sum((beta_glmnet - beta_coef)^2)

  fit_debiased <- debiased_lasso(x = x, y = y, beta = beta, ncores = ncores, lambda.choice = "lambda.min")

  fit_multisplits <- multisplits(x, y, lambda = "lambda.min", ncores = ncores, B = 200)
  fit_bootstrap <- bootstrap_multinom(x, y, lambda = "lambda.min", ncores = ncores, B = 200)


  cov_prob[[1]][i, ] <- c(mean(c(mean(beta_coef[1+ind1, 1] >= fit_debiased[[1]]$CI_lower[ind1] &
                                        beta_coef[1+ind1, 1] <= fit_debiased[[1]]$CI_upper[ind1]),
                                 mean(beta_coef[1+ind2, 2] >= fit_debiased[[2]]$CI_lower[ind2] &
                                        beta_coef[1+ind2, 2] <= fit_debiased[[2]]$CI_upper[ind2]),
                                 mean(beta_coef[1+ind3, 3] >= fit_debiased[[3]]$CI_lower[ind3] &
                                        beta_coef[1+ind3, 3] <= fit_debiased[[3]]$CI_upper[ind3]))),
                          mean(c(mean(beta_coef[-c(1, 1+ind1), 1] >= fit_debiased[[1]]$CI_lower[-ind1] &
                                        beta_coef[-c(1, 1+ind1), 1] <= fit_debiased[[1]]$CI_upper[-ind1]),
                                 mean(beta_coef[-c(1, 1+ind2), 2] >= fit_debiased[[2]]$CI_lower[-ind2] &
                                        beta_coef[-c(1, 1+ind2), 2] <= fit_debiased[[2]]$CI_upper[-ind2]),
                                 mean(beta_coef[-c(1, 1+ind3), 3] >= fit_debiased[[3]]$CI_lower[-ind3] &
                                        beta_coef[-c(1, 1+ind3), 3] <= fit_debiased[[3]]$CI_upper[-ind3]))))


  cov_prob[[2]][i, ] <- c(mean(c(mean(beta_coef[1+ind1, 1] >= fit_bootstrap[[1]][1+ind1, 1] &
                                        beta_coef[1+ind1, 1] <= fit_bootstrap[[1]][1+ind1, 2]),
                                 mean(beta_coef[1+ind2, 2] >= fit_bootstrap[[2]][1+ind2, 1] &
                                        beta_coef[1+ind2, 2] <= fit_bootstrap[[2]][1+ind2, 2]),
                                 mean(beta_coef[1+ind3, 3] >= fit_bootstrap[[3]][1+ind3, 1] &
                                        beta_coef[1+ind3, 3] <= fit_bootstrap[[3]][1+ind3, 2]))),
                          mean(c(mean(beta_coef[-c(1, 1+ind1), 1] >= fit_bootstrap[[1]][-c(1, 1+ind1), 1] &
                                        beta_coef[-c(1, 1+ind1), 1] <= fit_bootstrap[[1]][-c(1, 1+ind1), 2]),
                                 mean(beta_coef[-c(1, 1+ind2), 2] >= fit_bootstrap[[2]][-c(1, 1+ind2), 1] &
                                        beta_coef[-c(1, 1+ind2), 2] <= fit_bootstrap[[2]][-c(1, 1+ind2), 2]),
                                 mean(beta_coef[-c(1, 1+ind3), 3] >= fit_bootstrap[[3]][-c(1, 1+ind3), 1] &
                                        beta_coef[-c(1, 1+ind3), 3] <= fit_bootstrap[[3]][-c(1, 1+ind3), 2]))))


  ci_len[[1]][i, ] <- c(mean(c(fit_debiased[[1]]$CI_upper[ind1] - fit_debiased[[1]]$CI_lower[ind1],
                               fit_debiased[[2]]$CI_upper[ind2] - fit_debiased[[2]]$CI_lower[ind2],
                               fit_debiased[[3]]$CI_upper[ind3] - fit_debiased[[3]]$CI_lower[ind3])),
                        mean(c(fit_debiased[[1]]$CI_upper[-ind1] - fit_debiased[[1]]$CI_lower[-ind1],
                               fit_debiased[[2]]$CI_upper[-ind2] - fit_debiased[[2]]$CI_lower[-ind2],
                               fit_debiased[[3]]$CI_upper[-ind3] - fit_debiased[[3]]$CI_lower[-ind3])))

  ci_len[[2]][i, ] <- c(mean(c(fit_bootstrap[[1]][1+ind1, 2] - fit_bootstrap[[1]][1+ind1, 1],
                               fit_bootstrap[[2]][1+ind2, 2] - fit_bootstrap[[2]][1+ind2, 1],
                               fit_bootstrap[[3]][1+ind3, 2] - fit_bootstrap[[3]][1+ind3, 1])),
                        mean(c(fit_bootstrap[[1]][-c(1, 1+ind1), 2] - fit_bootstrap[[1]][-c(1, 1+ind1), 1],
                               fit_bootstrap[[2]][-c(1, 1+ind2), 2] - fit_bootstrap[[2]][-c(1, 1+ind2), 1],
                               fit_bootstrap[[3]][-c(1, 1+ind3), 2] - fit_bootstrap[[3]][-c(1, 1+ind3), 1])))

  ci_test[[1]][i, ] <- c(mean(c(mean(0 < fit_debiased[[1]]$CI_lower[ind1] |
                                       0 > fit_debiased[[1]]$CI_upper[ind1]),
                                mean(0 < fit_debiased[[2]]$CI_lower[ind2] |
                                       0 > fit_debiased[[2]]$CI_upper[ind2]),
                                mean(0 < fit_debiased[[3]]$CI_lower[ind3] |
                                       0 > fit_debiased[[3]]$CI_upper[ind3]))),
                         mean(c(mean(0 >= fit_debiased[[1]]$CI_lower[-ind1] &
                                       0 <= fit_debiased[[1]]$CI_upper[-ind1]),
                                mean(0 >= fit_debiased[[2]]$CI_lower[-ind2] &
                                       0 <= fit_debiased[[2]]$CI_upper[-ind2]),
                                mean(0 >= fit_debiased[[3]]$CI_lower[-ind3] &
                                       0 <= fit_debiased[[3]]$CI_upper[-ind3]))))

  ci_test[[2]][i, ] <- c(mean(c(mean(0 < fit_bootstrap[[1]][ind1+1, 1] |
                                       0 > fit_bootstrap[[1]][ind1+1, 2]),
                                mean(0 < fit_bootstrap[[2]][ind2+1, 1] |
                                       0 > fit_bootstrap[[2]][ind2+1, 2]),
                                mean(0 < fit_bootstrap[[3]][ind3+1, 1] |
                                       0 > fit_bootstrap[[3]][ind3+1, 2]))),
                         mean(c(mean(0 >= fit_bootstrap[[1]][-c(1,ind1+1), 1] &
                                       0 <= fit_bootstrap[[1]][-c(1,ind1+1), 2]),
                                mean(0 >= fit_bootstrap[[2]][-c(1,ind2+1), 1] &
                                       0 <= fit_bootstrap[[2]][-c(1,ind2+1), 2]),
                                mean(0 >= fit_bootstrap[[3]][-c(1,ind3+1), 1] &
                                       0 <= fit_bootstrap[[3]][-c(1,ind3+1), 2]))))

  fwer[[1]][i, ] <- c(mean(c(mean(fit_debiased[[1]]$p_value[ind1] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[2]]$p_value[ind2] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[3]]$p_value[ind3] <= 0.05/((K-1)*p)))),
                      mean(c(mean(fit_debiased[[1]]$p_value[-ind1] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[2]]$p_value[-ind2] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[3]]$p_value[-ind3] <= 0.05/((K-1)*p)))))

  fwer[[2]][i, ] <- c(mean(c(mean(fit_multisplits[ind1+1, 1] <= 0.05),
                             mean(fit_multisplits[ind2+1, 2] <= 0.05),
                             mean(fit_multisplits[ind3+1, 1] <= 0.05))),
                      mean(c(mean(fit_multisplits[-c(1, 1+ind1), 1] <= 0.05),
                             mean(fit_multisplits[-c(1, 1+ind2), 2] <= 0.05),
                             mean(fit_multisplits[-c(1, 1+ind3), 1] <= 0.05))))
}




save(cov_prob, fwer, ci_len, mse, ci_test, file = filename)

