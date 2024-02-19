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

filename <- paste("/moto/home/yt2661/work/multinom-inf/experiments/logistic_subtype_5/result/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------
# NACC
# ---------------------------------------------------------

# load the raw data
# load("/Users/yetian/Desktop/Dropbox/Columbia/Research/Project/NACC NP Classification/NACC/NACC Data/subtype_all_inf.RData")
load("/moto/home/yt2661/work/multinom-inf/datasets/subtype_all_inf.RData")


# ---------------------------------------------------------
# Preprocessing
# ---------------------------------------------------------
D <- D_all
ind.np <- substr(colnames(D), 0, 2)
names.np.rm <- colnames(D)[which(ind.np == "NP")]
names.np.kept <- c("NPIQINF", "NPIQINFX", "NPSYLAN", "NPSYLANX", "NPSYDEV", "NPSYCLOC")
names.np.rm <- setdiff(names.np.rm, names.np.kept)
D <- D %>% select(-NACCID, -NACCADC, -PACKET, -NACCMCII, -NACCIDEM, -NACCLBDM, -NACCFTD, -NACCMRSA, -NACCAUTP, -NACCDIED, -NACCAGEB,
                  -NACCADMU, -NACCFTDM, -NACCNORM, -NACCNVST, -NACCAVST, -NACCACTV, -NACCNMRI, -NACCAUTP, -NACCNOVS, -NGDSWGS,
                  -NGDSEXOM, -NGDSGWAS, -NACCNCRD, -NGDSWES, -ADGCEXOM, -ADGCGWAS, -NACCMOD, -NACCYOD, -NACCINT,
                  -NACCBRNN, -NACCBRAA, -NACCNEUR, -NACCDIFF, -NACCVASC, -NACCAMY, -NACCINF, -NACCMICR, -NACCHEM, -NACCARTE,
                  -NACCNEC, -NACCLEWY, -NACCPICK, -NACCCBD, -NACCPROG, -NACCPRIO, -NACCDOWN, -NACCOTHP, -NACCWRI1, -NACCWRI2,
                  -NACCWRI3, -NACCBNKF, -NACCFORM, -NACCPARA, -NACCCSFP, -NACCDAGE, -NACCAVAS, -ADGCGWAS, -ADGCEXOM, -ADGCRND,
                  -ADGCEXR, -NGDSGWAS, -NGDSEXOM, -NGDSWGS, -NGDSWES, -NGDSGWAC, -NGDSEXAC, -NGDSWGAC, -NGDSWEAC, -NACCNCRD,
                  -NACCMDSS, -FORMVER, -ALCFREQ, -DIABETES, -OTHSLEEP, -DELSEV, -AGITSEV, -DEPDSEV, -ANXSEV,
                  -ELATSEV, -APASEV, -DISNSEV, -IRRSEV, -MOTSEV, -NITESEV, -APPSEV, -RACE, -NACCPCSF, -NACCTCSF, -NACCNAPA)

# D <- D %>% select(-NACCID, -NACCADC, -PACKET, -NACCMCII, -NACCIDEM, -NACCLBDM, -NACCFTD, -NACCMRSA, -NACCAUTP, -NACCDIED, -NACCAGEB,
#                   -NACCADMU, -NACCFTDM, -NACCNORM, -NACCNVST, -NACCAVST, -NACCACTV, -NACCNMRI, -NACCAUTP, -NACCNOVS,
#                   -NACCMOD, -NACCYOD, -NACCINT,
#                   -NACCBRNN, -NACCBRAA, -NACCNEUR, -NACCDIFF, -NACCVASC, -NACCAMY, -NACCINF, -NACCMICR, -NACCHEM, -NACCARTE,
#                   -NACCNEC, -NACCLEWY, -NACCPICK, -NACCCBD, -NACCPROG, -NACCPRIO, -NACCDOWN, -NACCOTHP, -NACCWRI1, -NACCWRI2,
#                   -NACCWRI3, -NACCBNKF, -NACCFORM, -NACCPARA, -NACCCSFP, -NACCDAGE, -NACCAVAS,
#                   -NACCMDSS, -FORMVER, -ALCFREQ, -DIABETES, -OTHSLEEP, -DELSEV, -AGITSEV, -DEPDSEV, -ANXSEV,
#                   -ELATSEV, -APASEV, -DISNSEV, -IRRSEV, -MOTSEV, -NITESEV, -APPSEV, -RACE, -NACCPCSF, -NACCTCSF, -NACCNAPA)
D <- D[, !(colnames(D) %in% names.np.rm)]
D$subtype <- as.numeric(D$subtype)



risk_factors <- c("DIABTYPE", "DIABETES", "NACCDBMD", "DIABET", "TOBAC30", "TOBAC100", "SMOKYRS", "PACKSPER", "QUITSMOK",
                  "DEPTREAT", "DEP", "DEPIF", "DEPDSEV", "DEP2YRS", "DEPOTHR", "ANXIETY", "DEPD", "MEDS", "DYSILL",
                  "COGOTH", "ALCDEM", "NEOP", "BRNINJ", "DOWNS", "COGOTH2", "OTHPSY", "ALCFREQ", "ALCOCCAS", "ALCDEM",
                  "ALCABUSE", "ALCOHOL", "HYPERTEN", "HXHYPER", "HYPERT", "OTHSLEEP", "SLEEPAP", "SLEEPOTH", "APNEA",
                  "RBD", "BEREM", "NACCBEHF")

test_v <- sapply(1:ncol(D), function(j){
  if (any(D[, j] %in% 95:98, na.rm = T) && max(D[, j], na.rm = T) <= 100) {
    TRUE
  } else if (any(D[, j] %in% 995:998, na.rm = T) && max(D[, j], na.rm = T) <= 100){
    TRUE
  } else {
    NA
  }
})

test_v[is.na(test_v)] <- FALSE
test_v_names <- colnames(D)[test_v]

D_abnormal <- sapply(which(test_v), function(j){
  v <- D[, j]
  v0 <- rep(0, nrow(D))
  if (any(v %in% 95:98, na.rm = T) && max(v, na.rm = T) <= 100) {
    v0[(!is.na(v)) & v >= 95 & v<= 98] <- 1
  } else if (any(v %in% 995:998, na.rm = T) && max(v, na.rm = T) <= 100){
    v0[(!is.na(v)) & v >= 995 & v<= 998] <- 1
  }
  v0
})
colnames(D_abnormal) <- paste0(colnames(D)[test_v], "_abn")
A <- t(unique(t(D_abnormal)))
D_abnormal <- data.frame(A)

D <- cbind(D, D_abnormal)
for (j in which(test_v)) {
  D[(!is.na(D[, j])) & D[, j] >= 95 & D[, j]<= 98, j] <- -4
}

# ******* Apr 30 noon *******


v_names_8 <- c("BRADY", "PARKGAIT", "PACEMAKE", "HVALVE", "CORTDEF", "SLOWINGR", "OTHMUT", "POSTINST", "CORTDEF", "SIVDFIND",
               "IMAGLINF", "RESTTRL", "RESTTRR", "IMAGLAC", "IMAGMACH", "IMAGMICH", "IMAGMWMH", "IMAGEWMH", "CVDMOTL", "CVDMOTR",
               "AMYLCSF", "FDGAD", "TPETFTLD", "MRFTLD", "DATSCAN", "CORTVISR", "SOMATL", "SOMATR", "ANGIOCP", "ANGIOPCI",
               "PACKSPER", "AXIALPSP", "GAITPSP", "AMYLPET", "AMYLCSF", "FDGAD", "HIPPATR", "TPETFTLD", "MRFTLD",
               "APRAXSP", "APRAXL", "APRAXR", "CORTSENL", "CORTSENR", "ATAXL", "ATAXR", "ALIENLML", "ALIENLMR", "DYSTONL", "DYSTONR",
               "MYOCLLT", "MYOCLRT", "EYEPSP", "DYSPSP", "HYPERT", "ANGINA", "HYPCHOL", "VB12DEF", "THYDIS", "ARTH", "PDNORMAL", "REMDIS", "HYPOSOM", "SLEEPOTH", "CVDIMAG1",
               "CVDIMAG2", "CVDIMAG3", "CVDIMAG4", "URINEINC", "BOWLINC", "SLEEPAP", "CVDCOG", "STROKCOG", "CVDIMAG", "MYOINF",
               "CONGHRT", "AFIBRILL", "TAUPETAD", "CSFTAU", "FDGFTLD", "PACKSPER")
unknown_num <- sapply(1:ncol(D), function(j){
  if (colnames(D)[j] %in% c(risk_factors, "NACCAPOE", "NACCNE4S")) {
    0
  } else if (colnames(D)[j] %in% v_names_8) {
    sum(D[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9, 8), na.rm = T) + sum(is.na(D[, j]))
  } else {
    sum(D[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9), na.rm = T) + sum(is.na(D[, j]))
  }

})


D1 <- D[, unknown_num <= 0.1*nrow(D)] # only keep predictors whose proportion of missingness is <= 10%

# calculate how many unique values each variable has
s <- sapply(1:ncol(D1), function(j){length(unlist(unique(D1[, j])))})

# remove variables with a single value (no predictive power)
D1 <- D1[, s > 1]

# re-calculate how many unique values each variable has
s <- sapply(1:ncol(D1), function(j){length(unlist(unique(D1[, j])))})

categorical.vnames <- setdiff(unique(c(colnames(D1)[s <= 12], "NACCBEHF", "DRUG1", "DRUG2", "DRUG3", "NACCETPR")), c("HEIGHT", "CDRSUM", "CDRGLOB"))



# Impute the missing values by randomly sampling with replacement mmean/mode.

for (j in 1:ncol(D1)) {
  # print(j)
  if ((colnames(D1)[j] %in% risk_factors) &&(colnames(D1)[j] %in% categorical.vnames)) {
    next
  }
  if (colnames(D1)[j] %in% v_names_8) {
    ind.missing <- which(D1[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9, 8))
  } else {
    ind.missing <- which(D1[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9))
  }

  if (colnames(D1)[j] %in% categorical.vnames) {
    D1[ind.missing, j] <- Mode(unlist(D1[-ind.missing, j]))
  } else {
    D1[ind.missing, j] <- mean(unlist(D1[-ind.missing, j]))
  }

}


unknown_num1 <- sapply(1:ncol(D1), function(j){
  if ((colnames(D1)[j] %in% risk_factors) &&(colnames(D1)[j] %in% categorical.vnames)) {
    0
  } else {
    if (colnames(D1)[j] %in% v_names_8) {
      sum(D1[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9, 8), na.rm = T) + sum(is.na(D1[, j]))
    } else {
      sum(D1[, j] %in% c(-4, 9, 99, 999, 88, 888, 8888, 9999, 888.8, 88.8, -4.4, 99.9), na.rm = T) + sum(is.na(D1[, j]))
    }
  }
})


for (j in 1:ncol(D1)) {
  # print(j)
  if (colnames(D1)[j] %in% categorical.vnames) {
    D1[, j] <- as.factor(unlist(D1[, j]))
  }
}

# re-check the number of different values for each variable and remove useless variables
s <- sapply(1:ncol(D1), function(j){length(unlist(unique(D1[, j])))})
D1 <- D1[, s > 1]


D1.standardized <- D1
# standardize the numerical variables
for (j in 1:ncol(D1.standardized)) {
  # print(j)
  if (!(colnames(D1.standardized)[j] %in% categorical.vnames)) {
    D1.standardized[, j] <- scale(D1.standardized[, j])
  }
}



# Study the drug variable.
ind <- which(str_detect(colnames(D), "DRUG"))
Drug <- D[, ind]
all_drug <- unlist(Drug)
freq <- sort(table(all_drug), decreasing = T) # frequency of using of different drugs

drug_15 <- names(freq)[1:15]
drug_indicator_matrix <- foreach(i = 1:nrow(D), .combine = "rbind") %do% {
  drug_i_patient <- D[i, ind]
  (drug_15 %in% drug_i_patient)*1
}

colnames(drug_indicator_matrix) <- drug_15
rownames(drug_indicator_matrix) <- NULL
D1 <- data.frame(cbind(D1, drug_indicator_matrix))
D1.standardized <- data.frame(cbind(D1.standardized, drug_indicator_matrix))


# collinearity: remove some variables by checking >95% correlations
# variable-to-remove in the original variable list: ALCFREQ, DIABETES, OTHSLEEP, DELSEV, AGITSEV, DEPDSEV, ANXSEV, ELATSEV,
# APASEV, DISNSEV, IRRSEV, MOTSEV, NITESEV, APPSEV, RACE, NACCPCSF, NACCTCSF, NACCNAPA
# variable-to-remove list after transformation to dummy variables: x.INRACE2, x.INRACE5, x.INLIVWTH1, x.INCALLS8, x.VISWCORR8,
# x.HEARWAID8, x.MEMORY0.5, x.FRSTCHG8, x.ALCDEM8, x.ALCABUSE8, x.SLEEPAP0, x.MEMUNITS_abn1, x.UDSBENTD_abn1, x.UDSBENTD_abn1
# x.DIGIB_abn1, x.TRAILARR_abn1, x.TRAILBRR_abn1, x.UDSVERTN_abn1, x.MOCAREGI_abn1, x.MOCADIGI_abn1, x.MOCASER7_abn1
# x.MOCAREPE_abn1, x.MOCARECN_abn1, x.MOCAORCT_abn1, x.DIGBACCT_abn1, x.CRAFTDVR_abn1, x.HALLSEV8




D.cur <- data.frame(x = D1.standardized[, -1], y = D1.standardized$subtype)
x.model <- model.matrix(y~.-1, D.cur)
x.corr <- cor(x.model)
x.corr <- x.corr-diag(diag(x.corr))
ind <- which(abs(x.corr) > 0.95, arr.ind = T)
ind <- ind[ind[, 1] < ind[, 2], ]
ind.names <- t(sapply(1:nrow(ind), function(i){
  c(rownames(x.corr)[ind[i, 1]], colnames(x.corr)[ind[i, 2]], ind[i, 1], ind[i, 2])
}))

# ---------------------------------------------------------
# NP
# ---------------------------------------------------------

# Fit a model on all data (used for variable ranking)
D.cur <- data.frame(x = D1.standardized[, -1], y = D1.standardized$subtype)
x.model <- model.matrix(y~.-1, D.cur)

v.rm <- c("x.INRACE2", "x.INRACE5", "x.INLIVWTH1", "x.INCALLS8", "x.VISWCORR8", "x.HEARWAID8", "x.MEMORY0.5", "x.FRSTCHG8", "x.ALCDEM8",
          "x.ALCABUSE8", "x.SLEEPAP0", "x.MEMUNITS_abn1", "x.UDSBENTD_abn1", "x.UDSBENTD_abn1", "x.DIGIB_abn1", "x.TRAILARR_abn1",
          "x.TRAILBRR_abn1", "x.UDSVERTN_abn1", "x.MOCAREGI_abn1", "x.MOCADIGI_abn1", "x.MOCASER7_abn1", "x.MOCAREPE_abn1",
          "x.MOCARECN_abn1", "x.MOCAORCT_abn1", "x.DIGBACCT_abn1", "x.CRAFTDVR_abn1", "x.NACCNE4S2", "x.NGDSEXOM1", "x.HALLSEV8")

x.model <- x.model[, !(colnames(x.model) %in% v.rm)]



fit_pemultinom <- cv.pemultinom(x = x.model, y = as.numeric(D.cur$y), nfolds = 5, nlambda = 100, max_iter = 200, tol = 1e-3,
                                ncores = ncores, standardized = FALSE)

multinomial.coef.1se <- fit_pemultinom$beta.1se
multinomial.coef.min <- fit_pemultinom$beta.min

fit_debiased <- debiased_lasso(x = x.model, y = as.numeric(D.cur$y), beta = multinomial.coef.min, ncores = ncores,
                               lambda.choice = "lambda.min", sigma.choice = "product", intercept = FALSE)


# # save the result
save(fit_debiased, multinomial.coef.1se, multinomial.coef.min, file = filename)




