## clean the R environment
graphics.off()
rm(list = ls())
freshr::freshr()

## load packages
library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(gtsummary, quietly = TRUE)
library(flextable, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all()

## check the directory for the file
# here::dr_here()
here::set_here()

anchor <- c(30, 90, 200)
bsk_knots <- c(50, 100, 150)

# kappa6 <- seq(10, 100, by = 10)
# kappa6 <- seq(110, 200, by = 10)
# kappa6 <- seq(210, 300, by = 10)
# kappa6 <- seq(310, 400, by = 10)
# kappa6 <- seq(410, 500, by = 10)
kappa6 <- seq(510, 600, by = 10)
kappa6
mlmf <- "log_outcome ~ surgery_type + patient_gender + adi_value +
                      adi_value:log_baseline + primary_payer + 
                      bmi + patient_age +
                      patient_age:log_baseline + log_baseline + t0"
gf <- "log_outcome ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

lmf <- "log_outcome ~ as.factor(time) + surgery_type + patient_gender +
                      adi_value + adi_value:log_baseline + primary_payer +
                      bmi + patient_age +
                      patient_age:log_baseline + log_baseline + t0"

## change the slm and kappa number
e_kcv_kappa6 <- map(kappa6, 
                    ~people_like_us(train_data = tsa_train,
                                    test_data = tsa_test,
                                    anchor_time = anchor,
                                    brokenstick_knots = bsk_knots,
                                    linear_model = "lm",
                                    linear_formula = lmf,
                                    gamlss_formula = gf,
                                    gamlss_sigma = gs,
                                    tmin = 0,
                                    tmax = 100,
                                    id_var = "id",
                                    outcome_var = "log_outcome",
                                    time = "time",
                                    weight = FALSE,
                                    match_plot = FALSE,
                                    predict_plot = FALSE,
                                    match_methods = "euclidean",
                                    match_number = .x))

save(e_kcv_kappa6, file = "figure/tsa_kappa6_cross_validation_", Sys.time(), ".Rdata")


meanout <- function(dataset){
  # browser()
  result0 <- dataset %>%
    as.data.frame() %>%
    mutate(mse = bias^2) 
  result1 <- result0 %>%
    colMeans() %>%
    unlist()
  return(result1)}


result_kappa6 <- map(e_kcv_kappa6,
                ~map(.x, "centiles_observed") %>%
                  map_dfr(~try(meanout(.))) %>%
                  dplyr::select(coverage50, coverage80, 
                                coverage90, bias, mse) %>%
                  colMeans())

save(result_kappa6, file = "figure/tsa_table_kappa6_cross_validation_", Sys.time(), ".Rdata")
