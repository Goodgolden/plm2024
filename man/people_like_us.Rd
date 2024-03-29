% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_people-like-me.R
\name{people_like_us}
\alias{people_like_us}
\title{Title People-Like-Me methods for multiple testing dataset individuals}
\usage{
people_like_us(
  train_data,
  test_data,
  outcome_var = "ht",
  time_var = "time",
  id_var = "id",
  tmin = 0,
  tmax = 17,
  brokenstick_knots,
  anchor_time,
  linear_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
  gamlss_formula = "ht ~ cs(time, df = 3)",
  gamlss_sigma = "~ cs(time, df = 1)",
  match_methods = c("euclidean", "mahalanobis", "single"),
  weight = FALSE,
  match_alpha = NULL,
  match_number = NULL,
  match_plot = TRUE,
  predict_plot = TRUE,
  ...
)
}
\description{
Title People-Like-Me methods for multiple testing dataset individuals
}
