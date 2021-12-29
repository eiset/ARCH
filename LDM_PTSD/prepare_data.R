# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@clin.au.dk
# Title:    ARCH: PTSD, Load and prepare data set
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

source("setup.R")

# Import data, join and tidy  --------------------------------
tmps$dta_sets <- c("demog", "anxi", "depres", "htq", "who5", "migr_hist",
                   "blood", "swab", "health") #the data sets that contain the variables
tmps$vrbl_to_extract <- c("age", "sex", "violence", "ses", "highest_edu",
                          "cldrn", "smoking", "bp_sys$", "marital", "hgt$") #the variables that are needed

leb <- CreateCombine(country_id = "leb",
                     df_subset = tmps$dta_sets,
                     df_version = "1",
                     variables = tmps$vrbl_to_extract) %>%
  mutate(migr = 0)

dk <- CreateCombine("dk",
                    df_subset = tmps$dta_sets,
                    df_version = "1",
                    variables = tmps$vrbl_to_extract) %>%
  mutate(migr = 1,
         hgt = as.numeric(hgt))

# In smcfcs package all categorical variables must be factor type
joind_dta <- bind_rows(leb, dk) %>%
  dplyr::mutate(bp = if_else(bp_sys == 333, NA_real_, bp_sys),
                child = factor(cldrn, ordered = TRUE) %>%
                  forcats::fct_collapse("0-2" = c("0", "1", "2"),
                                        "5+" = "5"),
                edu = factor(highest_educ, ordered = TRUE),
                ses = factor(ses, ordered = TRUE) %>%
                  forcats::fct_collapse("Above average or refuse answer" =
                                          c("Do not know/do not wish to answer",
                                            "Above average")),
                smok = factor(smoking, ordered = FALSE),
                age_log = log(age),
                who_sqrt = sqrt(who5_score),
                anx_sqrt = sqrt(hscl_anx_score),
                bp_log = log(bp),
                hgt_log = log(hgt),
                mari = factor(marital_status, ordered = FALSE) %>%
                  forcats::fct_collapse("Other" = c("Single", "Widowed", "Other"))) %>%
  dplyr::rename(who = who5_score,
                viol = expir_violence,
                ptsd = htq_score,
                anx = hscl_anx_score) %>%
  dplyr::select(-c(rec_id, cldrn, bp_sys, hscl_depres_score, smoking,
                   highest_educ, marital_status))

rm(dk, leb)

# Add interaction term variables to data set ----------------------

joind_dta <- joind_dta %>%
  bind_cols(CreateInteractVrbl(., "who", NULL, "sex", "Female"),
            CreateInteractVrbl(., "sex", "Female", "age", NULL),
            CreateInteractVrbl(., "who", NULL, "age", NULL))

# Add spline basis variables to data set ------------------------------------------

tmps$vrbls_to_spline <- c("age",
                          "who",
                          "ptsd",
                          "anx",
                          "bp",
                          "hgt")

list_for_mi$knt_pos <- purrr::map(
  1:length(tmps$vrbls_to_spline),
  ~ ComputeKnotPosition(joind_dta,
                        tmps[["vrbls_to_spline"]][[.x]],
                        number_of_knots = 3)) %>%
  rlang::set_names(tmps$vrbls_to_spline)

joind_dta <- dplyr::bind_cols(joind_dta,
                              SplineBasisNamed(joind_dta,
                                               tmps$vrbls_to_spline,
                                               list_for_mi$knt_pos))

# To get rid of suffix "1" to all spline basis vars. Only necessary if >3 knots
colnames(joind_dta) <- stringr::str_replace(colnames(joind_dta), "[[:digit:]]", "")

# Get predictor matrix and MI method-string --------------------------------------------------------
# Link to predictor matrix (with variable type info)
tmps$lnk <- "mi_matrix.csv"
tmps$pred_mtx_and_type <- readr::read_csv(tmps$lnk, col_types = cols())

# Define variables to be passively imputed
tmps$pas_vars <- tibble::tibble(age = "exp(age_log)",
                                age_sb = "list_for_mi$ComputeSplineBasis(age, list_for_mi[['knt_pos']][['age']])[[1]]",
                                "sexFemale:age" = "as.numeric(sex == 'Female') * age",
                                who = "who_sqrt ^2",
                                who_sb = "list_for_mi$ComputeSplineBasis(who, list_for_mi[['knt_pos']][['who']])[[1]]",
                                "who:sexFemale" = "who * as.numeric(sex == 'Female')",
                                "who:age" = "who * age",
                                ptsd_sb = "list_for_mi$ComputeSplineBasis(ptsd, list_for_mi[['knt_pos']][['ptsd']])[[1]]",
                                anx = "anx ^2",
                                anx_sb = "list_for_mi$ComputeSplineBasis(anx, list_for_mi[['knt_pos']][['anx']])[[1]]",
                                bp = "exp(bp_log)",
                                bp_sb = "list_for_mi$ComputeSplineBasis(bp, list_for_mi[['knt_pos']][['bp']])[[1]]",
                                hgt = "exp(hgt_log)",
                                hgt_sb ="list_for_mi$ComputeSplineBasis(hgt, list_for_mi[['knt_pos']][['hgt']])[[1]]"
) %>%
  tidyr::pivot_longer(everything())

list_for_mi <- c(list_for_mi, MtrxMethod(joind_dta,
                                         tmps$pred_mtx_and_type$type,
                                         tmps$pred_mtx_and_type,
                                         tmps$pas_vars))

# Sort data frame to match predictor matrix
joind_dta <- joind_dta %>%
  dplyr::select(colnames(list_for_mi$predictor_matrix))

# smcfcs evaluates ":" in column name as function so throws error. Replace with X
colnames(joind_dta) <- stringr::str_replace(colnames(joind_dta), ":", "X")

#write_rds(joind_dta, "full_data_set.rds")
#write_rds(list_for_mi, "list_for_mi.rds")