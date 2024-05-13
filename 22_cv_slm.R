## clean the R environment --------------------
graphics.off()
rm(list = ls())

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
kappa1 <- seq(4, 100, by = 1)

mlmf <- "outcome_score ~ adi_value + adi_value:outcome0 + adi_value:t0 +
                      bmi + bmi:outcome0 + bmi:t0 +
                      patient_age + patient_age:outcome0 + patient_age:t0 +
                      patient_gender + patient_gender:outcome0 + patient_gender:t0 +
                      primary_payer + primary_payer:outcome0 + primary_payer:t0 +
                      surgery_type + surgery_type:outcome0 + surgery_type:t0 +
                      outcome0 + t0"

gf <- "outcome_score ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

lmf <- "outcome_score ~ as.factor(time) + 
                      adi_value + adi_value:outcome0 + adi_value:t0 + 
                      bmi + bmi:outcome0 + bmi:t0 +
                      patient_age + patient_age:outcome0 + patient_age:t0 +
                      patient_gender + patient_gender:outcome0 + patient_gender:t0 +
                      primary_payer + primary_payer:outcome0 + primary_payer:t0 +
                      surgery_type + surgery_type:outcome0 + surgery_type:t0 +
                      outcome0 + t0"

## euclidean kappa ---------------------------------
## change the slm and kappa number
e_kcv_kappa1 <- map(kappa1, 
                    ~people_like_us(train_data = tsa_train1,
                                    test_data = tsa_test1,
                                    anchor_time = anchor,
                                    brokenstick_knots = bsk_knots,
                                    linear_model = "lm",
                                    linear_formula = lmf,
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

# save(e_kcv_kappa1, file = paste0("results/tsa_22_slm_cv_", Sys.time(), ".Rdata"))
## summary ------------------------------------------

meanout <- function(dataset){
  
  result0 <- dataset %>%
    as.data.frame() %>%
    mutate(mse = bias^2,
           cr50 = `75` - `25`)
  result1 <- result0 %>%
    colMeans() %>%
    unlist()
  return(result1)}


slm_kappa1 <- map(e_kcv_kappa1,
                  ~try(map(.x, "centiles_observed") %>%
                         map_dfr(~try(meanout(.))) %>%
                         colMeans()))

# result_kappa1 <- map(e_kcv_kappa1,
#                      ~try(map(.x, "centiles_observed") %>%
#                             map_dfr(~try(meanout(.))) %>%
#                             dplyr::select(coverage50, coverage80, 
#                                           coverage90, bias, mse) %>%
#                             colMeans()))

## saresult_kappa1## saving the results --------------------------------

save(e_kcv_kappa1, file = paste0("results/tsa_22_slm_cv_", Sys.time(), ".Rdata"))

save(slm_kappa1, file = paste0("results/tsa_22_table_slm_cv_", Sys.time(), ".Rdata"))

