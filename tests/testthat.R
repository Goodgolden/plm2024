# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(plm2024)

test_check("plm2024")


## 3.3 dis_match_pred ----------------------------------------------------------


# all_people <- linear$testing %>%
#   group_by("id") %>%
#   group_map(data.frame) %>%
#   map(~ .x[[as_label(enquo(outcome_var))]]) %>%
#   map(~distance_df(lb_train = linear$training,
#                   lb_test_ind = .,
#                   match_methods = "mahalanobis",
#                   id_var = "id",
#                   time_var = "time",
#                   outcome_var = "ht"))



# test_103104 <- test %>% filter(id == 156392)
#
# plm_individual <- people_like_me(train_data = train,
#                                  test_data = test_103104,
#                                  outcome_var = "ht",
#                                  time_var = "time",
#                                  id_var = "id",
#                                  brokenstick_knots = c(5, 12, 15),
#                                  anchor_time = c(5, 10, 11, 12),
#                                  linear_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
#                                  match_methods = "mahalanobis",
#                                  match_alpha = 0.99,
#                                  match_number = NULL,
#                                  weight = FALSE,
#                                  match_plot = TRUE)
#
#
# View(plm_individual)
# View(plm_individual$gamlss_data)
# View(test)
#
# attributes(plm_individual)





