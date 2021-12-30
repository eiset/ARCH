# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@ph.au.dk
# Title:    ARCH: PTSD, the analyses
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
source("setup.R")

# Complete-case: Crude estimate  --------------------------------------------
psw_bs_dta <- readr::read_rds("mock_data/bs_data_final.rds") #contains the crude data set
dta_unimpd <- psw_bs_dta$data

PointEstCrude <- function(data_set, indices) {
  glm(
    ptsd >= 2.5 ~ migr,
    family = binomial(link = "identity"),
    data = data_set[indices,])[["coefficients"]][["migr"]]
}

PointEstCrude(dta_unimpd)

t <- glm(
  ptsd >= 2.5 ~ migr,
  family = binomial(link = "identity"),
  data = dta_unimpd)

confint(t)

set.seed(451)
boot_out_cc_crude <- boot::boot(
  data = dta_unimpd,
  statistic = PointEstCrude,
  R = 999,
  parallel = "multicore",
  ncpus = 2 # set as appropriate
)

boot.ci(boot_out_cc_crude)


# Complete-case: Weighted estimate ------------------------------------------------------
psw_bs_dta <- readr::read_rds("mock_data/bs_data_final.rds") #contains the crude data set
list_for_mi <- readr::read_rds("mockdata/list_for_mi.rds")

dta_unimpd <- psw_bs_dta$data

PointEstWgtd <- function(data_set, indices) {
  w_out_unimpd <- list_for_mi$WeightAndTrunc(
    ps_formula = as.formula("migr ~ age + age_sb + sex + who + who_sb + viol + ses"),
    data = data_set[indices, ],
    reference_in_ATT = "1",
    truncate_upper_quantile = 0.99)

  glm(
    ptsd >= 2.5 ~ migr,
    family = binomial(link = "identity"),
    data = data_set[indices,],
    weights = w_out_unimpd
  )[["coefficients"]][["migr"]]
}

PointEstWgtd(dta_unimpd)

set.seed(451)
boot_out_cc_wgtd <- boot::boot(
  data = dta_unimpd,
  statistic = PointEstWgtd,
  R = 999,
  parallel = "multicore",
  ncpus = 2 # set as appropriate
)

boot::boot.ci(boot_out_cc_wgtd)

# Multiply imputed weighted estimate ---------------------------------------------------

library(boot)
psw_bs_dta <- readr::read_rds("data/bs_data_final.rds")

#Point estimate
psw_bs_dta$t0

#Confidence interval
boot.ci(psw_bs_dta, type = "all")

