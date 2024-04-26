## clean the R environment --------------------
graphics.off()
rm(list = ls())
freshr::freshr()

## load packages -----------------------------
library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all()

## check the directory for the file
# here::dr_here()
here::set_here()

## setup for plm ------------------------------

anchor <- c(14, 30, 60, 90)
bsk_knots <- c(20, 50, 100, 150, 200, 250)
kappa1 <- seq(4, 100, by = 2)

mlmf <- "outcome_score ~ surgery_type + surgery_type:baseline0 + surgery_type:t0 +
                      patient_gender + patient_gender:baseline0 + patient_gender:t0 +
                      adi_value + adi_value:baseline0 + adi_value:t0 +
                      primary_payer + primary_payer:baseline0 + primary_payer:t0 +
                      bmi + bmi:baseline0 + bmi:t0 +
                      patient_age + patient_age:baseline0 + patient_age:t0 +
                      baseline0 + t0"

gf <- "outcome_score ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

lmf <- "outcome_score ~ as.factor(time) + 
                      surgery_type + surgery_type:baseline0 + surgery_type:t0 +
                      patient_gender + patient_gender:baseline0 + patient_gender:t0 +
                      adi_value + adi_value:baseline0 + adi_value:t0 +
                      primary_payer + primary_payer:baseline0 + primary_payer:t0 +
                      bmi + bmi:baseline0 + bmi:t0 +
                      patient_age + patient_age:baseline0 + patient_age:t0 +
                      baseline0 + t0"

## euclidean kappa ---------------------------------
## change the slm and kappa number
e_kcv_kappa1 <- map(kappa1, 
                    ~people_like_us(train_data = tsa_train,
                                    test_data = tsa_test,
                                    anchor_time = anchor,
                                    brokenstick_knots = bsk_knots,
                                    linear_model = "mlm",
                                    linear_formula = mlmf,
                                    gamlss_formula = gf,
                                    gamlss_sigma = gs,
                                    tmin = 0,
                                    tmax = 100,
                                    id_var = "id",
                                    outcome_var = "outcome_score",
                                    time = "time",
                                    weight = FALSE,
                                    match_plot = FALSE,
                                    predict_plot = FALSE,
                                    match_methods = "euclidean",
                                    match_number = .x),
                    .progress = TRUE)

## summary ------------------------------------------

meanout <- function(dataset){
  # browser()
  result0 <- dataset %>%
    as.data.frame() %>%
    mutate(mse = bias^2) 
  result1 <- result0 %>%
    colMeans() %>%
    unlist()
  return(result1)}


result_kappa1 <- map(e_kcv_kappa1,
                     ~try(map(.x, "centiles_observed") %>%
                       map_dfr(~try(meanout(.))) %>%
                       dplyr::select(coverage50, coverage80, 
                                     coverage90, bias, mse) %>%
                       colMeans()))

## saresult_kappa1## saving the results --------------------------------

save(e_kcv_kappa1, file = paste0("figure/new2_tsa_16_kappa1_cross_validation_", Sys.time(), ".Rdata"))

save(result_kappa1, file = paste0("figure/new2_tsa_16_table_kappa1_cross_validation_", Sys.time(), ".Rdata"))

