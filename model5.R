.libPaths("/burg/home/yt2661/R/x86_64-pc-linux-gnu-library/4.1/")

library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(naivebayes)
library(doParallel)
library(nnet)
library(stringr)
library(glmnet)
library(pemultinom)
library(HandTill2001)
library(MASS)


Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename <- paste("/burg/home/yt2661/projects/multinom-inf/experiments/model5/result/", seed, ".RData", sep = "")
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
# function to find the nearest odd number: for kNN
find_nearst_odd_number <- function(x) {
  if (floor(x) %% 2 == 0) {
    return(floor(x) - 1)
  } else {
    return(floor(x))
  }
}

# model set up
n <- c(100, 200, 400)
p <- 200
K <- 3
sig.strength <- 2
s <- 3
ind1 <- 1:s
ind2 <- s+1:s
beta_coef <- matrix(0, nrow = p+1, ncol = K-1)
beta_coef[ind1+1, 1] <- rep(1, s)*sig.strength
beta_coef[ind2+1, 2] <-  rep(1, s)*sig.strength

Sigma <- outer(1:p, 1:p, function(x,y) {
  0.75^(abs(x-y))
})

R <- chol(Sigma)


cov_prob <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
ci_test <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
ci_len <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
fwer <- rep(list(matrix(nrow = length(n), ncol = 2)), 2)
mse <- matrix(nrow = length(n), ncol = 2)
classification_error <- matrix(nrow = length(n), ncol = 5, dimnames = list(NULL, c("pemultinom", "svm", "lda", "rf", "knn")))
inf_result <- list()

# iteration for each n = 100, 200, 400
for (i in 1:length(n)) {
  print(i)

  # generate training data
  x <- matrix(rnorm(n[i]*p), ncol = p) %*% R

  y <- sapply(1:n[i], function(j){
    prob_i <- c(sapply(1:(K-1), function(k){
      exp(sum(x[j, ]*beta_coef[-1, k]))
    }), 1)
    prob_i <- prob_i/sum(prob_i)
    sample(1:K, size = 1, replace = TRUE, prob = prob_i)
  })

  # generate test data
  x_test <- matrix(rnorm(1000*p), ncol = p) %*% R

  y_test <- sapply(1:1000, function(j){
    prob_i <- c(sapply(1:(K-1), function(k){
      exp(sum(x_test[j, ]*beta_coef[-1, k]))
    }), 1)
    prob_i <- prob_i/sum(prob_i)
    sample(1:K, size = 1, replace = TRUE, prob = prob_i)
  })

  # fit the over-parameterized penalized model using glmnet
  fit_glmnet <- cv.glmnet(x = x, y = y, family = "multinomial")
  lambda.min.ind <- which(fit_glmnet$lambda == fit_glmnet$lambda.min)
  beta_glmnet <- sapply(1:(K-1), function(k){
    c(fit_glmnet$glmnet.fit$a0[k, lambda.min.ind], fit_glmnet$glmnet.fit$beta[[k]][, lambda.min.ind]) -
      c(fit_glmnet$glmnet.fit$a0[K, lambda.min.ind], fit_glmnet$glmnet.fit$beta[[K]][, lambda.min.ind])
  })

  # fit the contrast-based penalized model using our package
  fit <- cv.pemultinom(x, y, nfolds = 5, nlambda = 100, ncores = ncores, max_iter = 1e4, tol = 1e-5, lambda_min_ratio = 1e-4,
                       standardized = F, intercept = F)
  beta <- fit$beta.min

  # obtain the prediction result for different methods
  y_pred <- as.numeric(predict_pemultinom(beta, ref = K, xnew = x_test, type = "class"))

  fit_svm <- svm(x = x, y = factor(y), kernel = "radial")
  y_pred_svm <- predict(fit_svm, x_test)

  fit_lda <- lda(x = x, grouping = y)
  y_pred_lda <- predict(fit_lda, x_test)$class

  fit_rf <- randomForest(x = x, y = factor(y))
  y_pred_rf <- predict(fit_rf, x_test)

  fit_knn <- knn3(x = x, y = factor(y), k = find_nearst_odd_number(sqrt(n[i])))
  y_pred_knn <- predict(fit_knn, x_test, type = "class")

  # collect misclassification error
  classification_error[i, ] <- c(mean(y_test != y_pred), mean(y_test != y_pred_svm), mean(y_test != y_pred_lda),
                                 mean(y_test != y_pred_rf), mean(y_test != y_pred_knn))

  # collect misclassification error
  mse[i, 1] <- sum((beta - beta_coef)^2)
  mse[i, 2] <- sum((beta_glmnet - beta_coef)^2)

  # run the debiased Lasso
  fit_debiased <- debiased_lasso(x, y, beta = fit$beta.min, ncores = ncores, nfolds = 5, lambda.choice = "lambda.3se", nlambda = 100,
                                 lambda_min_ratio = 1e-4, max_iter = 1e4, tol = 1e-5, r = 0.5)
  inf_result[[i]] <- fit_debiased

  # run the multiple splitting method
  fit_multisplits <- multisplits(x, y, lambda = "lambda.min", ncores = ncores, B = 200)
  
  # run the bootstrap method
  fit_bootstrap <- bootstrap_multinom(x, y, lambda = "lambda.min", ncores = ncores, B = 200)

  # collect average coverage probability for debiased Lasso
  cov_prob[[1]][i, ] <- c(mean(c(mean(beta_coef[ind1+1, 1] >= fit_debiased[[1]]$CI_lower[ind1] &
                                        beta_coef[ind1+1, 1] <= fit_debiased[[1]]$CI_upper[ind1]),
                                 mean(beta_coef[ind2+1, 2] >= fit_debiased[[2]]$CI_lower[ind2] &
                                        beta_coef[ind2+1, 2] <= fit_debiased[[2]]$CI_upper[ind2]))),
                          mean(c(mean(beta_coef[-c(1, ind1+1), 1] >= fit_debiased[[1]]$CI_lower[-ind1] &
                                        beta_coef[-c(1, ind1+1), 1] <= fit_debiased[[1]]$CI_upper[-ind1]),
                                 mean(beta_coef[-c(1, ind2+1), 2] >= fit_debiased[[2]]$CI_lower[-ind2] &
                                        beta_coef[-c(1, ind2+1), 2] <= fit_debiased[[2]]$CI_upper[-ind2]))))

  # collect average coverage probability for the bootstrap method
  cov_prob[[2]][i, ] <- c(mean(c(mean(beta_coef[ind1+1, 1] >= fit_bootstrap[[1]][ind1+1, 1] &
                                        beta_coef[ind1+1, 1] <= fit_bootstrap[[1]][ind1+1, 2]),
                                 mean(beta_coef[ind2+1, 2] >= fit_bootstrap[[2]][ind2+1, 1] &
                                        beta_coef[ind2+1, 2] <= fit_bootstrap[[2]][ind2+1, 2]))),
                          mean(c(mean(beta_coef[-c(1, ind1+1), 1] >= fit_bootstrap[[1]][-c(1, ind1+1), 1] &
                                        beta_coef[-c(1, ind1+1), 1] <= fit_bootstrap[[1]][-c(1, ind1+1), 2]),
                                 mean(beta_coef[-c(1, ind2+1), 2] >= fit_bootstrap[[2]][-c(1, ind2+1), 1] &
                                        beta_coef[-c(1, ind2+1), 2] <= fit_bootstrap[[2]][-c(1, ind2+1), 2]))))

  # collect the average CI length for debiased Lasso
  ci_len[[1]][i, ] <- c(mean(c(fit_debiased[[1]]$CI_upper[ind1] - fit_debiased[[1]]$CI_lower[ind1],
                               fit_debiased[[2]]$CI_upper[ind2] - fit_debiased[[2]]$CI_lower[ind2])),
                        mean(c(fit_debiased[[1]]$CI_upper[-ind1] - fit_debiased[[1]]$CI_lower[-ind1],
                               fit_debiased[[2]]$CI_upper[-ind2] - fit_debiased[[2]]$CI_lower[-ind2])))

  # collect the average CI length for the bootstrap method
  ci_len[[2]][i, ] <- c(mean(c(fit_bootstrap[[1]][ind1+1, 2] - fit_bootstrap[[1]][ind1+1, 1],
                               fit_bootstrap[[2]][ind2+1, 2] - fit_bootstrap[[2]][ind2+1, 1])),
                        mean(c(fit_bootstrap[[1]][-c(1, ind1+1), 2] - fit_bootstrap[[1]][-c(1, ind1+1), 1],
                               fit_bootstrap[[2]][-c(1, ind2+1), 2] - fit_bootstrap[[2]][-c(1, ind2+1), 1])))

  # collect the individual testing results for debiased Lasso
  ci_test[[1]][i, ] <- c(mean(c(mean(0 < fit_debiased[[1]]$CI_lower[ind1] |
                                       0 > fit_debiased[[1]]$CI_upper[ind1]),
                                mean(0 < fit_debiased[[2]]$CI_lower[ind2] |
                                       0 > fit_debiased[[2]]$CI_upper[ind2]))),
                         mean(c(mean(0 >= fit_debiased[[1]]$CI_lower[-ind1] &
                                       0 <= fit_debiased[[1]]$CI_upper[-ind1]),
                                mean(0 >= fit_debiased[[2]]$CI_lower[-ind2] &
                                       0 <= fit_debiased[[2]]$CI_upper[-ind2]))))

  # collect the individual testing results for the bootstrap method
  ci_test[[2]][i, ] <- c(mean(c(mean(0 < fit_bootstrap[[1]][ind1+1, 1] |
                                       0 > fit_bootstrap[[1]][ind1+1, 2]),
                                mean(0 < fit_bootstrap[[2]][ind2+1, 1] |
                                       0 > fit_bootstrap[[2]][ind2+1, 2]))),
                         mean(c(mean(0 >= fit_bootstrap[[1]][-c(1,ind1+1), 1] &
                                       0 <= fit_bootstrap[[1]][-c(1,ind1+1), 2]),
                                mean(0 >= fit_bootstrap[[2]][-c(1,ind2+1), 1] &
                                       0 <= fit_bootstrap[[2]][-c(1,ind2+1), 2]))))

  # collect the multiple testing results for debiased Lasso
  fwer[[1]][i, ] <- c(mean(c(mean(fit_debiased[[1]]$p_value[ind1] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[2]]$p_value[ind2] <= 0.05/((K-1)*p)))),
                      mean(c(mean(fit_debiased[[1]]$p_value[-ind1] <= 0.05/((K-1)*p)),
                             mean(fit_debiased[[2]]$p_value[-ind2] <= 0.05/((K-1)*p)))))

  # collect the multiple testing results for the multiple splitting method
  fwer[[2]][i, ] <- c(mean(c(mean(fit_multisplits[ind1+1, 1] <= 0.05),
                             mean(fit_multisplits[ind2+1, 2] <= 0.05))),
                      mean(c(mean(fit_multisplits[-c(1, ind1+1), 1] <= 0.05),
                             mean(fit_multisplits[-c(1, ind2+1), 2] <= 0.05))))
}


# save the results
save(cov_prob, fwer, ci_len, mse, ci_test, classification_error, inf_result, file = filename)

