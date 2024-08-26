# -------------------------------------
# multinomial model: K = 3
# -------------------------------------
# ---------------
# CI coverage
L_debiased <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3_s/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_debiased_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})

L <- rbind(L_debiased, L_debiased_CI, L_bootstrap, L_bootstrap_CI)
L_sd <- rbind(L_debiased_sd, L_debiased_CI_sd, L_bootstrap_sd, L_bootstrap_CI_sd)

# print the CI table
for (k in 1:8) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{4}{*}{Debiased-Lasso}} & Average coverage probability on $S$ ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Average coverage probability on $S^c$ ")
  } else if (k == 3) {
    cat("\\multicolumn{1}{c}{}                 & Average length of CI ($S$)            ")
  } else if (k == 4) {
    cat("\\multicolumn{1}{c}{}                   & Average length of CI ($S^c$)          ")
  } else if (k == 5) {
    cat("\\multirow{4}{*}{Bootstrap}                & Average coverage probability on $S$   ")
  } else if (k == 6) {
    cat("                                         & Average coverage probability on $S^c$ ")
  } else if (k == 7) {
    cat("                                          & Average length of CI ($S$)            ")
  } else if (k == 8) {
    cat("                                         & Average length of CI ($S^c$)           ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 4) {
    cat("\\hline")
  } else if (k == 8) {
    cat("\\hline")
  }
  cat("\n")
}


# ---------------
# single test
S_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3_s/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L, na.rm = TRUE)
})
S_bootstrap <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})
S_bootstrap_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})

L <- rbind(S_debiased, S_bootstrap)
L_sd <- rbind(S_debiased_sd, S_bootstrap_sd)

# print the single test table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & Type-1 error ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Bootstrap}                & Type-1 error ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}

# ----------------
# multiple testing
H_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_multisplit <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})
H_multisplit_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})

L <- rbind(H_debiased, H_multisplit)
L_sd <- rbind(H_debiased_sd, H_multisplit_sd)

# print the multiple testing table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & FWER ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Multiple-Spliting}                & FWER ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}


# --------------
# MSE
mse_debiased <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_glmnet <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_debiased_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})
mse_glmnet_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})

L <- rbind(mse_debiased, mse_glmnet)
L_sd <- rbind(mse_debiased_sd, mse_glmnet_sd)

# print the MSE table
for (k in 1:2) {
  if (k == 1) {
    cat("\\multicolumn{1}{c|}{Contrast} ")
  } else if (k == 2) {
    cat("Over-parameterization         ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  }
  cat("\n")
}

# -------------------------------------
# multinomial model: K = 4
# -------------------------------------
# ---------------
# CI coverage
L_debiased <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_debiased_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})

L <- rbind(L_debiased, L_debiased_CI, L_bootstrap, L_bootstrap_CI)
L_sd <- rbind(L_debiased_sd, L_debiased_CI_sd, L_bootstrap_sd, L_bootstrap_CI_sd)

# print the CI table
for (k in 1:8) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{4}{*}{Debiased-Lasso}} & Average coverage probability on $S$ ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Average coverage probability on $S^c$ ")
  } else if (k == 3) {
    cat("\\multicolumn{1}{c}{}                 & Average length of CI ($S$)            ")
  } else if (k == 4) {
    cat("\\multicolumn{1}{c}{}                   & Average length of CI ($S^c$)          ")
  } else if (k == 5) {
    cat("\\multirow{4}{*}{Bootstrap}                & Average coverage probability on $S$   ")
  } else if (k == 6) {
    cat("                                         & Average coverage probability on $S^c$ ")
  } else if (k == 7) {
    cat("                                          & Average length of CI ($S$)            ")
  } else if (k == 8) {
    cat("                                         & Average length of CI ($S^c$)           ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 4) {
    cat("\\hline")
  } else if (k == 8) {
    cat("\\hline")
  }
  cat("\n")
}


# ---------------
# single test
S_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_bootstrap <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})
S_bootstrap_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})

L <- rbind(S_debiased, S_bootstrap)
L_sd <- rbind(S_debiased_sd, S_bootstrap_sd)

# print the single test table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & Type-1 error ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Bootstrap}                & Type-1 error ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}

# ----------------
# multiple testing
H_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_multisplit <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})
H_multisplit_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})

L <- rbind(H_debiased, H_multisplit)
L_sd <- rbind(H_debiased_sd, H_multisplit_sd)

# print the multiple testing table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & FWER ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Multiple-Spliting}                & FWER ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}


# --------------
# MSE
mse_debiased <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_glmnet <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_debiased_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})
mse_glmnet_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/multinomial_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})

L <- rbind(mse_debiased, mse_glmnet)
L_sd <- rbind(mse_debiased_sd, mse_glmnet_sd)

# print the MSE table
for (k in 1:2) {
  if (k == 1) {
    cat("\\multicolumn{1}{c|}{Contrast} ")
  } else if (k == 2) {
    cat("Over-parameterization         ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  }
  cat("\n")
}

# -------------------------------------
# LDA model: K = 3
# -------------------------------------
# ---------------
# CI coverage
L_debiased <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_debiased_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})

L <- rbind(L_debiased, L_debiased_CI, L_bootstrap, L_bootstrap_CI)
L_sd <- rbind(L_debiased_sd, L_debiased_CI_sd, L_bootstrap_sd, L_bootstrap_CI_sd)

# print the CI table
for (k in 1:8) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{4}{*}{Debiased-Lasso}} & Average coverage probability on $S$ ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Average coverage probability on $S^c$ ")
  } else if (k == 3) {
    cat("\\multicolumn{1}{c}{}                 & Average length of CI ($S$)            ")
  } else if (k == 4) {
    cat("\\multicolumn{1}{c}{}                   & Average length of CI ($S^c$)          ")
  } else if (k == 5) {
    cat("\\multirow{4}{*}{Bootstrap}                & Average coverage probability on $S$   ")
  } else if (k == 6) {
    cat("                                         & Average coverage probability on $S^c$ ")
  } else if (k == 7) {
    cat("                                          & Average length of CI ($S$)            ")
  } else if (k == 8) {
    cat("                                         & Average length of CI ($S^c$)           ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 4) {
    cat("\\hline")
  } else if (k == 8) {
    cat("\\hline")
  }
  cat("\n")
}


# ---------------
# single test
S_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_bootstrap <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})
S_bootstrap_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})

L <- rbind(S_debiased, S_bootstrap)
L_sd <- rbind(S_debiased_sd, S_bootstrap_sd)

# print the single test table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & Type-1 error ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Bootstrap}                & Type-1 error ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}

# ----------------
# multiple testing
H_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_multisplit <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})
H_multisplit_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})

L <- rbind(H_debiased, H_multisplit)
L_sd <- rbind(H_debiased_sd, H_multisplit_sd)

# print the multiple testing table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & FWER ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Multiple-Spliting}                & FWER ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}


# --------------
# MSE
mse_debiased <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_glmnet <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_debiased_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})
mse_glmnet_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_3/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})

L <- rbind(mse_debiased, mse_glmnet)
L_sd <- rbind(mse_debiased_sd, mse_glmnet_sd)

# print the MSE table
for (k in 1:2) {
  if (k == 1) {
    cat("\\multicolumn{1}{c|}{Contrast} ")
  } else if (k == 2) {
    cat("Over-parameterization         ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  }
  cat("\n")
}

# -------------------------------------
# LDA model: K = 4
# -------------------------------------
# ---------------
# CI coverage
L_debiased <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_bootstrap_CI <- sapply(1:3, function(j){
  rowMeans(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), na.rm = TRUE)
})
L_debiased_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      cov_prob[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_debiased_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[1]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})
L_bootstrap_CI_sd <- sapply(1:3, function(j){
  apply(sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      ci_len[[2]][j, ]
    } else {
      rep(NA, 2)
    }
  }), 1, function(x){sd(x, na.rm = TRUE)})
})

L <- rbind(L_debiased, L_debiased_CI, L_bootstrap, L_bootstrap_CI)
L_sd <- rbind(L_debiased_sd, L_debiased_CI_sd, L_bootstrap_sd, L_bootstrap_CI_sd)

# print the CI table
for (k in 1:8) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{4}{*}{Debiased-Lasso}} & Average coverage probability on $S$ ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Average coverage probability on $S^c$ ")
  } else if (k == 3) {
    cat("\\multicolumn{1}{c}{}                 & Average length of CI ($S$)            ")
  } else if (k == 4) {
    cat("\\multicolumn{1}{c}{}                   & Average length of CI ($S^c$)          ")
  } else if (k == 5) {
    cat("\\multirow{4}{*}{Bootstrap}                & Average coverage probability on $S$   ")
  } else if (k == 6) {
    cat("                                         & Average coverage probability on $S^c$ ")
  } else if (k == 7) {
    cat("                                          & Average length of CI ($S$)            ")
  } else if (k == 8) {
    cat("                                         & Average length of CI ($S^c$)           ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 4) {
    cat("\\hline")
  } else if (k == 8) {
    cat("\\hline")
  }
  cat("\n")
}


# ---------------
# single test
S_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_bootstrap <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  rowMeans(L)
})
S_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[1]][j, 2], ci_test[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})
S_bootstrap_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(1-ci_test[[2]][j, 2], ci_test[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  apply(L, 1, sd)
})

L <- rbind(S_debiased, S_bootstrap)
L_sd <- rbind(S_debiased_sd, S_bootstrap_sd)

# print the single test table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & Type-1 error ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Bootstrap}                & Type-1 error ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}

# ----------------
# multiple testing
H_debiased <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_multisplit <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(mean(L[1,]>0, na.rm = TRUE), mean(L[2,], na.rm = TRUE))
})
H_debiased_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[1]][j, 2], fwer[[1]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})
H_multisplit_sd <- sapply(1:3, function(j){
  L <- sapply(1:200, function(i){
    a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
    if (class(a) != "try-error") {
      c(fwer[[2]][j, 2], fwer[[2]][j, 1])
    } else {
      rep(NA, 2)
    }
  })
  c(sd(L[1,]>0, na.rm = TRUE), sd(L[2,], na.rm = TRUE))
})

L <- rbind(H_debiased, H_multisplit)
L_sd <- rbind(H_debiased_sd, H_multisplit_sd)

# print the multiple testing table
for (k in 1:4) {
  if (k == 1) {
    cat("\\multicolumn{1}{c}{\\multirow{2}{*}{Debiased-Lasso}} & FWER ")
  } else if (k == 2) {
    cat("\\multicolumn{1}{c}{}                   & Power ")
  } else if (k == 3) {
    cat("\\multirow{2}{*}{Multiple-Splitting}                & FWER ")
  } else if (k == 4) {
    cat("                                         & Power ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  } else if (k == 4) {
    cat("\\hline")
  }
  cat("\n")
}


# --------------
# MSE
mse_debiased <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_glmnet <- rowMeans(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), na.rm = TRUE)
mse_debiased_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 1]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})
mse_glmnet_sd <- apply(sapply(1:200, function(i){
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/lda_model_4/", i, ".RData")))
  if (class(a) != "try-error") {
    mse[, 2]
  } else {
    rep(NA, 3)
  }
}), 1, function(x){sd(x, na.rm = TRUE)})

L <- rbind(mse_debiased, mse_glmnet)
L_sd <- rbind(mse_debiased_sd, mse_glmnet_sd)

# print the MSE table
for (k in 1:2) {
  if (k == 1) {
    cat("\\multicolumn{1}{c|}{Contrast} ")
  } else if (k == 2) {
    cat("Over-parameterization         ")
  }

  for (j in 1:3) {
    cat(paste0("&", round(L[k, j], 3), " (", round(L_sd[k, j], 3), ") "))
  }
  cat("\\\\")
  if (k == 2) {
    cat("\\hline")
  }
  cat("\n")
}



# ------------------------------------------------
# L-R rule: subtype
# ------------------------------------------------
library(stringr)
library(doParallel)
library(dplyr)
library(ggplot2)
library(ggrepel)

debiased_matrix <- rep(list(matrix(0, nrow = 467-1, ncol = 500)),4)
coef_matrix <- rep(list(matrix(0, nrow = 467-1, ncol = 500)),4)
U_matrix <- rep(list(matrix(0, nrow = 467-1, ncol = 500)),4)
n <- 12797
for (i in 1:500) {
  a <- try(load(paste0("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/results/version 1/logistic_subtype_5/", i, ".RData")))
  if (class(a) == "try-error") {
    next
  }
  for (k in 1:4) {
    debiased_matrix[[k]][, i] <- fit_debiased$fitted_values[[k]][, 2]
    coef_matrix[[k]][, i] <- fit_debiased$fitted_values[[k]][, 1]
    U_matrix[[k]][, i] <- fit_debiased$sigma2.hat[, k]/sqrt(n)
  }

}

theta <- sapply(1:4, function(k){
  rowMeans(coef_matrix[[k]])
})


z <- sapply(1:4, function(k){
  q_z <- qnorm(1-debiased_matrix[[k]])
  q_z[q_z == Inf] <- 10 # truncated at 10 to avoid Inf
  rowMeans(q_z)
})

B <- sapply(1:4, function(k){
  q_z <- qnorm(1-debiased_matrix[[k]])
  q_z[q_z == Inf] <- 10 # truncated at 10 to avoid Inf
  apply(q_z, 1, var)
})

m <- 500
T_m <- 1+(1+1/m)*B
r <- (1+1/m)*B
v <- (m-1)*(1+1/r)^2

p_value <- 1-pt(z/T_m, df = v)

result_table <- rep(list(matrix(nrow = 467-1, ncol = 3, dimnames = list(NULL, c("beta", "odds ratio", "p-value")))), 4)
for (k in 1:4) {
  result_table[[k]][, 1] <- theta[, k]
  result_table[[k]][, 2] <- exp(theta[, k])
  result_table[[k]][, 3] <- p_value[, k]
  rownames(result_table[[k]]) <- str_replace_all(rownames(fit_debiased$fitted_values[[1]]), "x.", "")
}


print(result_table[[1]][order(result_table[[1]][, 3])[1:20], ], digits = 3)
print(result_table[[2]][order(result_table[[2]][, 3])[1:20], ], digits = 3)
print(result_table[[3]][order(result_table[[3]][, 3])[1:20], ], digits = 3)
print(result_table[[4]][order(result_table[[4]][, 3])[1:20], ], digits = 3)


grouping_info <- readxl::read_xlsx("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/codes/version 1/grouping-info-subtype-5.xlsx", sheet = "names")
grouping_info <- grouping_info[, 2:3]
grouping_info <- data.frame(grouping_info)

grouping_info[!(grouping_info[, 1] %in% rownames(result_table[[1]])), 1]
rownames(result_table[[1]])[!(rownames(result_table[[1]]) %in% grouping_info[, 1])]

grouping_info[grouping_info[, 2] == "C", 2] <- "Clinician Judgment of Symptoms and Diagnosis"
grouping_info[grouping_info[, 2] == "D", 2] <- "Subject/Co-participant Demographics"
grouping_info[grouping_info[, 2] == "F", 2] <- "Function"
grouping_info[grouping_info[, 2] == "FH", 2] <- "Family History"
grouping_info[grouping_info[, 2] == "MH", 2] <- "Medical History"
grouping_info[grouping_info[, 2] == "NPS", 2] <- "Neuropsychiatric Symptoms"
grouping_info[grouping_info[, 2] == "O", 2] <- "Biomarkers"
grouping_info[grouping_info[, 2] == "P", 2] <- "Physical"
grouping_info[grouping_info[, 2] == "PT", 2] <- "Psychometric Tests"
grouping_info[grouping_info[, 2] == "SM", 2] <- "Subject Medications"
grouping_info[grouping_info[, 2] == "V", 2] <- "Visiting Information"


code <- names(table(grouping_info[,2]))

result_table_ordered <- sapply(1:4, function(k){
  foreach(i = code, .combine = "rbind") %do% {
    data.frame(result_table[[k]][grouping_info[grouping_info[, 2] == i, 1], ], group = i)
  }
}, simplify = FALSE)


# table(grouping_info[grouping_info[, 1] %in% rownames(result_table[[1]][result_table[[1]][, 3] < 0.05,]),2])
# table(grouping_info[grouping_info[, 1] %in% rownames(result_table[[2]][result_table[[2]][, 3] < 0.05,]),2])
# table(grouping_info[grouping_info[, 1] %in% rownames(result_table[[3]][result_table[[3]][, 3] < 0.05,]),2])

num <- table(grouping_info[,2])


for (k in 1:4) {
  for (i in 1:nrow(result_table_ordered[[k]])) {
    if (result_table_ordered[[k]][i, 1] >= 0) {
      rownames(result_table_ordered[[k]])[i] <- paste0(rownames(result_table_ordered[[k]])[i], "(+)")
    } else {
      rownames(result_table_ordered[[k]])[i] <- paste0(rownames(result_table_ordered[[k]])[i], "(-)")
    }
  }
}


sig.ind <- sort(unique(Reduce("c", sapply(1:4, function(k){
  which(result_table_ordered[[k]]$p.value <= 0.05)
}, simplify = FALSE))))

rt.merged <- foreach(k = 1:4, .combine = "rbind") %do% {
  result_table_ordered[[k]][sig.ind, ]
}

plot.df <- data.frame(rt.merged, variable = c(rownames(result_table_ordered[[1]])[sig.ind],
                                                rownames(result_table_ordered[[2]])[sig.ind],
                                                rownames(result_table_ordered[[3]])[sig.ind],
                                                rownames(result_table_ordered[[4]])[sig.ind]),
                      Subtype = rep(c("AD", "LBD/PD", "FTLD", "VBI"), each= length(sig.ind)))
plot.df <- data.frame(feature = rep(1:nrow(result_table_ordered[[1]][sig.ind,]), 4), plot.df)
plot.df <- plot.df[plot.df$p.value <= 0.05,]

group <- result_table_ordered[[1]]$group[sig.ind]
group_names <- unique(group)

# save as a 13*8 plot
# plot.df %>% ggplot(mapping = aes(y = feature, x = p.value, color = Subtype, shape = Subtype)) +
#   geom_point() + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,4.5), "cm")) +
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black", size = 0.25) + theme(legend.position="bottom") +
#   xlab("p-value") +
#   geom_text_repel(aes(label = as.character(variable)), box.padding = 0.15, point.padding = 0.1, size = 2.5, show.legend = FALSE) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[1], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[2], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[3], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[4], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[5], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[6], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[7], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[8], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[9], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[10], linetype="dashed", size = 0.25) +
#   geom_hline(yintercept=0.5+cumsum(table(group))[11], linetype="dashed", size = 0.25) +
#   coord_cartesian(xlim = c(0, 0.05), clip = "off") +
#   annotate("text", y = (0+cumsum(table(group))[1])/2+2, x = -0.0055, label = "Clinician Judgment", size = 3) +
#   annotate("text", y = (0+cumsum(table(group))[1])/2-2.8+2, x = -0.0055, label = "of Symptoms", size = 3) +
#   annotate("text", y = (0+cumsum(table(group))[1])/2-2.8*2+2, x = -0.0055, label = "and Diagnosis", size = 3) +
#   annotate("text", y = (cumsum(table(group))[1]+cumsum(table(group))[2])/2+1, x = -0.0055, label = group_names[2], size = 3) +
#   annotate("text", y = (cumsum(table(group))[2]+cumsum(table(group))[3])/2+1, x = -0.0055, label = group_names[3], size = 3) +
#   annotate("text", y = (cumsum(table(group))[3]+cumsum(table(group))[4])/2+1, x = -0.0055, label = group_names[4], size = 3) +
#   annotate("text", y = (cumsum(table(group))[4]+cumsum(table(group))[5])/2+2, x = -0.0055, label = "Neuropsychiatric", size = 3) +
#   annotate("text", y = (cumsum(table(group))[4]+cumsum(table(group))[5])/2-2.8+2, x = -0.0055, label = "Symptoms", size = 3) +
#   annotate("text", y = (cumsum(table(group))[5]+cumsum(table(group))[6])/2+0.8, x = -0.0055, label = group_names[6], size = 3) +
#   annotate("text", y = (cumsum(table(group))[6]+cumsum(table(group))[7])/2+0.9, x = -0.0055, label = group_names[7], size = 3) +
#   annotate("text", y = (cumsum(table(group))[7]+cumsum(table(group))[8])/2+2, x = -0.0055, label = "Psychometric", size = 3) +
#   annotate("text", y = (cumsum(table(group))[7]+cumsum(table(group))[8])/2-2.8+2, x = -0.0055, label = "Tests", size = 3) +
#   annotate("text", y = (cumsum(table(group))[8]+cumsum(table(group))[9])/2+2, x = -0.0055, label = "Subject", size = 3) +
#   annotate("text", y = (cumsum(table(group))[8]+cumsum(table(group))[9])/2-2.8+2, x = -0.0055, label = "Medications", size = 3) +
#   annotate("text", y = (cumsum(table(group))[9]+ length(sig.ind))/2+2, x = -0.0055, label = "Subject/Co-participant", size = 3) +
#   annotate("text", y = (cumsum(table(group))[9]+ length(sig.ind))/2-2.8+2, x = -0.0055, label = "Demographics", size = 3) +
#   annotate("text", y = (cumsum(table(group))[10]+cumsum(table(group))[11])/2+0.8, x = -0.0055, label = group_names[11], size = 3)

plot.df %>% ggplot(mapping = aes(y = feature, x = p.value, color = Subtype, shape = Subtype)) +
  geom_point() + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,4.5), "cm")) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black", size = 0.25) + theme(legend.position="bottom") +
  xlab("p-value") +
  geom_text_repel(aes(label = as.character(variable)), box.padding = 0.15, point.padding = 0.1, size = 2.5, show.legend = FALSE) +
  geom_hline(yintercept=0.5+cumsum(table(group))[1], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[2], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[3], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[4], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[5], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[6], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[7], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[8], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[9], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[10], linetype="dashed", size = 0.25) +
  geom_hline(yintercept=0.5+cumsum(table(group))[11], linetype="dashed", size = 0.25) +
  coord_cartesian(xlim = c(0, 0.05), clip = "off") +
  annotate("text", y = (0+cumsum(table(group))[1])/2+1, x = -0.0055, label = group_names[1], size = 3) +
  annotate("text", y = (0+cumsum(table(group))[2])/2+2, x = -0.0055, label = "Clinician Judgment", size = 3) +
  annotate("text", y = (0+cumsum(table(group))[2])/2-2.8+2, x = -0.0055, label = "of Symptoms", size = 3) +
  annotate("text", y = (0+cumsum(table(group))[2])/2-2.8*2+2, x = -0.0055, label = "and Diagnosis", size = 3) +
  annotate("text", y = (cumsum(table(group))[2]+cumsum(table(group))[3])/2+1, x = -0.0055, label = group_names[3], size = 3) +
  annotate("text", y = (cumsum(table(group))[3]+cumsum(table(group))[4])/2+1, x = -0.0055, label = group_names[4], size = 3) +
  annotate("text", y = (cumsum(table(group))[4]+cumsum(table(group))[5])/2+1, x = -0.0055, label = group_names[5], size = 3) +
  annotate("text", y = (cumsum(table(group))[5]+cumsum(table(group))[6])/2+2, x = -0.0055, label = "Neuropsychiatric", size = 3) +
  annotate("text", y = (cumsum(table(group))[5]+cumsum(table(group))[6])/2-2.8+2, x = -0.0055, label = "Symptoms", size = 3) +
  annotate("text", y = (cumsum(table(group))[6]+cumsum(table(group))[7])/2+0.9, x = -0.0055, label = group_names[7], size = 3) +
  annotate("text", y = (cumsum(table(group))[7]+cumsum(table(group))[8])/2+2, x = -0.0055, label = "Psychometric", size = 3) +
  annotate("text", y = (cumsum(table(group))[7]+cumsum(table(group))[8])/2-2.8+2, x = -0.0055, label = "Tests", size = 3) +
  annotate("text", y = (cumsum(table(group))[8]+cumsum(table(group))[9])/2+2, x = -0.0055, label = "Subject", size = 3) +
  annotate("text", y = (cumsum(table(group))[8]+cumsum(table(group))[9])/2-2.8+2, x = -0.0055, label = "Medications", size = 3) +
  annotate("text", y = (cumsum(table(group))[9]+ length(sig.ind))/2+2, x = -0.0055, label = "Subject/Co-participant", size = 3) +
  annotate("text", y = (cumsum(table(group))[9]+ length(sig.ind))/2-2.8+2, x = -0.0055, label = "Demographics", size = 3) +
  annotate("text", y = (cumsum(table(group))[10]+cumsum(table(group))[11])/2+0.8, x = -0.0055, label = group_names[11], size = 3)


# making tables
library(rtf)
rtffile <- RTF("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/Inference on high dim multinomial regression/codes/rtf.doc")  # this can be an .rtf or a .doc
for (i in 1:4) {
  tb.logistic_l1 <- data.frame(feature = names(result_table[[i]][order(result_table[[i]][, 3])[1:10], 1]),
                               coef = round(result_table[[i]][order(result_table[[i]][, 3])[1:10], 1], 4),
                               odds = round(result_table[[i]][order(result_table[[i]][, 3])[1:10], 2], 4),
                               pvalue = round(result_table[[i]][order(result_table[[i]][, 3])[1:10], 3], 4))
  addParagraph(rtffile, "This is the output of a regression coefficients:\n")
  addTable(rtffile, tb.logistic_l1)
}

done(rtffile)



# radar chart: 14*8.5 pdf
library(fmsb)
library(scales)
par(mfrow = c(2,2))
radarchart.tb <- data.frame(matrix(0, nrow = 4+2, ncol = length(code)))
colnames(radarchart.tb) <- code
rownames(radarchart.tb) <- c("Max", "Min", "AD", "LBD/PD", "FTLD", "VBI")
for (k in 1:4) {
  v <- table(result_table_ordered[[k]][result_table_ordered[[k]]$p.value <= 0.05, ]$group)
  radarchart.tb[k+2, names(v)] <- v/num[names(v)]
}
colnames(radarchart.tb)[2] <- "Clinician Judgment\n of Symptoms\n and Diagnosis"
colnames(radarchart.tb)[3] <- "Family\n History"
colnames(radarchart.tb)[6] <- "Neuropsychiatric\n Symptoms"
colnames(radarchart.tb)[8] <- "Psychometric\n Tests"
colnames(radarchart.tb)[9] <- "Subject\n Medications"
colnames(radarchart.tb)[10] <- "Subject/Co-participant\n Demographics"
colnames(radarchart.tb)[11] <- "Visiting\n Information"

radarchart.tb["Max", ] <- max(radarchart.tb["AD", ])
radarchart.tb["Min", ] <- 0
radarchart(radarchart.tb[c("Max", "Min", "AD"), ], pcol = hue_pal()(4)[1], pfcol = scales::alpha(hue_pal()(4)[1], 0.5), axistype = 1,
           centerzero = F, caxislabels = paste0(round(seq(0, max(radarchart.tb["AD", ]), length.out = 5)*100, digits = 2), "%"), title = "AD",
           vlcex = 1.1, plwd = 1.5, cglwd = 1.5, calcex = 1.2)
radarchart.tb["Max", ] <- max(radarchart.tb["FTLD", ])
radarchart(radarchart.tb[c("Max", "Min", "FTLD"), ], pcol = hue_pal()(4)[2], pfcol = scales::alpha(hue_pal()(4)[2], 0.5), axistype = 1,
           centerzero = F, caxislabels = paste0(round(seq(0, max(radarchart.tb["FTLD", ]), length.out = 5)*100, digits = 2), "%"), title = "FTLD",
           vlcex = 1.1, plwd = 1.5, cglwd = 1.5, calcex = 1.2)
radarchart.tb["Max", ] <- max(radarchart.tb["LBD/PD", ])
radarchart(radarchart.tb[c("Max", "Min", "LBD/PD"), ], pcol = hue_pal()(4)[3], pfcol = scales::alpha(hue_pal()(4)[3], 0.5), axistype = 1,
           centerzero = F, caxislabels = paste0(round(seq(0, max(radarchart.tb["LBD/PD", ]), length.out = 5)*100, digits = 2), "%"), title = "LBD/PD",
           vlcex = 1.1, plwd = 1.5, cglwd = 1.5, calcex = 1.2)
radarchart.tb["Max", ] <- max(radarchart.tb["VBI", ])
radarchart(radarchart.tb[c("Max", "Min", "VBI"), ], pcol = hue_pal()(4)[4], pfcol = scales::alpha(hue_pal()(4)[4], 0.5), axistype = 1,
           centerzero = F, caxislabels = paste0(round(seq(0, max(radarchart.tb["VBI", ]), length.out = 5)*100, digits = 2), "%"), title = "VBI",
           vlcex = 1.1, plwd = 1.5, cglwd = 1.5, calcex = 1.2)



# pie chart
piechart.tb <- data.frame(matrix(0, nrow = 4, ncol = length(code)))
rownames(piechart.tb)<- c("AD", "LBD/PD", "FTLD", "VBI")
colnames(piechart.tb) <- code
for (k in 1:4) {
  v <- table(result_table_ordered[[k]][result_table_ordered[[k]]$p.value <= 0.05, ]$group)
  piechart.tb[k, names(v)] <- v/sum(v)
}


data.frame(category = code, percentage = as.numeric(piechart.tb[1, ])) %>%
  ggplot(aes(x = "", y = percentage, fill = category)) +
  geom_col() +
  coord_polar(theta = "y")

# bar chart: 10*10 pdf
X_Axis_Labels <- c("Biomarkers",
                   "Clinician Judgment<br>of Symptoms<br>and Diagnosis",
                   "Family<br>History",
                   "Function",
                   "Medical<br>History",
                   "Neuropsychiatric<br>Symptoms",
                   "Physical",
                   "Psychometric<br>Tests",
                   "Subject<br>Medications",
                   "Subject/<br>Co-participant<br>Demographics",
                   "Visiting<br>Information")


p1 <- data.frame(Category = code, Percentage = as.numeric(piechart.tb[1, ])*100) %>%
  ggplot(aes(x = Category, y = Percentage)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = X_Axis_Labels) +
  theme(axis.text.x = element_markdown(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("AD")

p2 <- data.frame(Category = code, Percentage = as.numeric(piechart.tb[2, ])*100) %>%
  ggplot(aes(x = Category, y = Percentage)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = X_Axis_Labels) +
  theme(axis.text.x = element_markdown(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("LBD/PD")

p3 <- data.frame(Category = code, Percentage = as.numeric(piechart.tb[3, ])*100) %>%
  ggplot(aes(x = Category, y = Percentage)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = X_Axis_Labels) +
  theme(axis.text.x = element_markdown(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("FTLD")

p4 <- data.frame(Category = code, Percentage = as.numeric(piechart.tb[4, ])*100) %>%
  ggplot(aes(x = Category, y = Percentage)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = X_Axis_Labels) +
  theme(axis.text.x = element_markdown(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("VBI")

ggarrange(p1, p3, p2, p4, ncol = 1, nrow = 4)
