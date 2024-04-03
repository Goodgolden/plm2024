library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(cli, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all()

## check the directory for the file
# here::dr_here()
here::set_here()

load("~/Desktop/paper2023/R/train_test.rda")

set.seed(555)

anchor <- c(10, 12, 15)
bsk_knots <- c(5, 10, 12, 15)
kappa4 <- seq(155, 200, by = 5)
# kappa4 <- seq(55, 100, by = 5)

gf <- "ht ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

lmf <- "ht ~ as.factor(time) * sex + genotype + baseline"

e_kcv_kappa4 <- map(kappa4, 
                    ~people_like_us(train_data = train,
                                    test_data = test,
                                    anchor_time = anchor,
                                    brokenstick_knots = bsk_knots,
                                    linear_model = "lm",
                                    linear_formula = lmf,
                                    gamlss_formula = gf,
                                    gamlss_sigma = gs,
                                    tmin = 0,
                                    tmax = 100,
                                    id_var = "id",
                                    outcome_var = "ht",
                                    time = "time",
                                    weight = FALSE,
                                    match_plot = FALSE,
                                    predict_plot = FALSE,
                                    match_methods = "euclidean",
                                    match_number = .x), 
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))

save(e_kcv_kappa4, file = paste0("paper2023/plm2023_16_kappa4_cross_validation_", Sys.time(), ".Rdata"))
# save(e_kcv_kappa4, file = "paper2023/plm2023_16_kappa4_cross_validation_", Sys.time(), ".Rdata")