# Code of "$\ell_1$-penalized Multinomial Regression: Estimation, inference, and prediction, with an application to risk factor identification for different dementia subtypes" (2023) by Tian et al.

## Package installation
We have made the contrast-based Lasso and the debiased Lasso for multinomial regression models in an R package `pemultinom` on CRAN. The version used in Version 2 of our page has not been synced to CRAN yet. The users can download the source package `pemultinom_0.1.1.tar.gz` from this page then install it in R by the following code
```
install.packages('.../pemultinom_0.1.1.tar.gz', repos = NULL, type = 'source')
```
where `...` needs to be replaced by the location of the source package file.


## Simulation
The R files `model1.R`, ..., `model8.R` correspond to eight examples in our paper (Models 1-4 in the main text and Models 5-8 in the appendix). The script files `model1.sh`, ..., `model8.sh` are used to run the R files on clusters. When using the code, the users need to replace the file paths therein by their own paths.

## Real-data analysis
The R file `logistic_subtype_5.R` includes the code for the real-data analysis in our paper. However, the dataset we used is a private dataset and cannot be shared in public. The users can write their own code based on our code using our package. The script file `logistic_subtype_5.sh` is used to run the R file on clusters.

## Table and figure generation
We used R file `rs_summary.R` to summarize the output results from simulations and the real-data study, and generate all the tables and figures in our paper. The users may need to translate the tables output by the code to the Lates format.
