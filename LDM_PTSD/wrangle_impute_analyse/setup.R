# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@clin.au.dk
# Title:    ARCH: PTSD, setup and functions
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Initialise and load packages --------------------------------------------

# rm(list = ls()) #UNHASH: will clear the environment
graphics.off()

# unload all loaded packages
if(is.null(lapply(
  names(sessionInfo()$loadedOnly),
  library,
  character.only = TRUE)) != 0) {
  
  invisible(lapply(
    names(sessionInfo()$loadedOnly),
    library,
    character.only = TRUE))
  
  invisible(lapply(
    paste0('package:', names(sessionInfo()$otherPkgs)),
    detach,
    character.only = TRUE,
    unload = TRUE,
    force = TRUE))
}

tmps <- new.env()
tmps$pckg <- c("Hmisc", "tidyverse", "smcfcs", "WeightIt", "cobalt", "boot", "furrr")
lapply(tmps$pckg, library, character.only = TRUE)
print(sapply(tmps$pckg, packageVersion))

# Function to extract the data sets ---------------------------------------

tmps$SetPath <- function(cntr_id) {
  if (cntr_id == "dk") {
    file.path("mock_data/raw/dk")
  } else if (cntr_id == "leb") {
    file.path("mock_data/raw/leb")
  } else {
    stop("Country ID is not recognised")
  }
}

tmps$GetRawSubset <- function(country_id, df_subset, df_version) {
  files_subset <- paste0(df_subset, sep = "(.*)")
  if (!missing(df_version)) {
    files <- paste(files_subset, df_version, sep = "_", collapse = "|")
    files_list <- list.files(path = tmps$SetPath(country_id),
                             pattern = files,
                             full.names = TRUE)
    if (length(files_list) == 0) {
      stop("Defined version of data does not exist")
    }
  } else {
    files <- files_subset
    files_list <- list.files(path = tmps$SetPath(country_id),
                             pattern = files,
                             full.names = TRUE)
    if (length(files_list) == 0) {
      stop("Defined subset of data does not exist")
    }
  }
  nms <- stringr::str_extract(files_list, paste0(df_subset, collapse = "|"))
  purrr::map(files_list, read_rds) %>% rlang::set_names(nms)
}

# Function: Subtract variables as stated in analysis plan -----------------------------------------------

tmps$SubsetVars <-
  function(country_id, df_subset, df_version, variables) {
    a <- tmps$GetRawSubset(country_id, df_subset, df_version)
    
    b <- suppressMessages(lapply(a, function(x)
      x[stringr::str_subset(names(x), "rec_id")]) %>%
        dplyr::bind_cols() %>%
        dplyr::select(rec_id = 1))  #suppress message about creating new variable names because we just want a "rec_id"
    
    vrbls <- paste0(variables, collapse = "|")
    
    lapply(a, function(x)
      x[stringr::str_subset(names(x), vrbls)]) %>%
      dplyr::bind_cols(b)
  }

tmps$CalcPsycScore <- function(dta_set, psyc_scale, summary_score = function(x) rowMeans(x)){
  dta_set[stringr::str_subset(names(dta_set), psyc_scale)] %>%
    dplyr::bind_cols() %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(-stringr::str_which(names(.), "(.*)complete")) %>%
    summary_score
}

CreateCombine <- function(country_id, df_subset, df_version, variables) {
  full_data_set <- tmps$GetRawSubset(country_id, df_subset, df_version)
  
  who5_score <- tmps$CalcPsycScore(full_data_set,
                                   "who5",
                                   summary_score = function(x) rowSums(x) * 4)
  
  htq_score <- tmps$CalcPsycScore(full_data_set,
                                  "htq")
  
  hscl_depres_score <- tmps$CalcPsycScore(full_data_set,
                                          "depr")
  
  hscl_anx_score <- tmps$CalcPsycScore(full_data_set,
                                       "anx")

  covars <- tmps$SubsetVars(country_id, df_subset, df_version, variables)
  
  dplyr::bind_cols(covars,
                   who5_score = who5_score,
                   htq_score = htq_score,
                   hscl_anx_score = hscl_anx_score,
                   hscl_depres_score = hscl_depres_score)
}



# Function: Compute spline knots --------------------------------------

ComputeKnotPosition <- function(data_set, vrbl, number_of_knots) {
  attr(Hmisc::rcspline.eval(data_set[[vrbl]],
                            inclx = TRUE,
                            nk = number_of_knots),
       "knots")
}

# Function: Compute cubic spline basis ------------------------------------------------

list_for_mi <- list()

list_for_mi$ComputeSplineBasis <- function(variable_to_spline, knot_positions) {
  vrbl_sb <- variable_to_spline
  knots <- knot_positions
  knots <- sort(unique(knots))
  knot_min <- knots[1]
  knot_max <- knots[length(knots)]
  knot_maxminus <- knots[length(knots) - 1]
  kd <- (knot_max - knot_min) ^ (2 / 3)
  power <- 3
  
  SplineBasisFormula <- function(knbr_to_spline) {
    spline_var <- pmax((vrbl_sb - knots[knbr_to_spline]) / kd, 0) ^ power +
      ((knot_maxminus - knots[knbr_to_spline]) * pmax((vrbl_sb - knot_max) / kd, 0) ^
         power -
         (knot_max - knots[knbr_to_spline]) * (pmax((
           vrbl_sb - knot_maxminus
         ) / kd, 0) ^ power)) /
      (knot_max - knot_maxminus)
  }
  
  nbr_sb_vars <- 1:(length(knots) - 2)
  
  lapply(nbr_sb_vars, SplineBasisFormula)
}


# Function: Create spline basis as new variable in data set -----------

SplineBasisNamed <- function(data_set,
                             names_of_variables_to_spline,
                             list_of_knot_positions) {
  
  tmp_data_set <- data_set[, names_of_variables_to_spline]
  knots <- list_of_knot_positions
  nbr_sb_vars <- 1:(length(knots[[1]]) - 2)
  
  purrr::map(names_of_variables_to_spline,
             ~ list_for_mi$ComputeSplineBasis(tmp_data_set[[.x]],
                                              knots[[.x]]) %>%
               rlang::set_names(paste0(.x, "_sb", nbr_sb_vars)))
}



# Function: Create interaction term as new variable in data set -----------------------------
CreateInteractVrbl <- function(data_set,
                               var1, subset_var1 = NULL,
                               var2, subset_var2 = NULL) {
  if (!is.null(subset_var1)) {
    v1 <- as.numeric(data_set[, var1] == subset_var1)
  } else {
    v1 <- as.numeric(data_set[, var1])
  }
  if (!is.null(subset_var2)) {
    v2 <- as.numeric(data_set[, var2] == subset_var2)
  } else {
    v2 <- as.numeric(data_set[, var2])}
  
  out <- data.frame(v1 * v2)
  
  colnames(out) <- paste0(var1, subset_var1, ":", var2, subset_var2)
  
  out
  
}

# Function: Weights, compute and truncate --------------------------------------------------

list_for_mi$WeightAndTrunc <-
  function(ps_formula,
           data,
           reference_in_ATT,
           truncate_upper_quantile) {
    
    a <- WeightIt::weightit(
      formula = ps_formula,
      data = data,
      estimand = "ATT",
      focal = reference_in_ATT,
      method = "ps")
    
    WeightIt::trim(a,
                   at = truncate_upper_quantile,
                   lower = TRUE)$weights
  }

# Function: Predictor matrix and model formula --------------------------------------

MtrxMethod <- function(data_set, vrbl_type, pred_mtrx, passive_vars){
  type <- vrbl_type
  
  a <- pred_mtrx %>%
    dplyr::mutate(mi_m = dplyr::if_else(
      response %in% names(which(sapply(data_set, anyNA))),
      type, ""),
      mi_m = dplyr::case_when(
        mi_m == "b" ~ "logreg",
        mi_m == "c" ~ "norm",
        mi_m == "o" ~ "podds",
        mi_m == "u" ~ "mlogit",
        mi_m == "p" ~ "poisson",
        mi_m == "pas" ~ "passive",
        TRUE ~ ""
      )
    ) %>%
    dplyr::left_join(passive_vars,
                     by = c("response" = "name")) %>%
    dplyr::mutate(mi_m = dplyr::if_else(mi_m == "passive", value, mi_m)) %>%
    dplyr::select(-value)
  
  pred_mtx <- a %>%
    dplyr::select(3:(length(a)-1)) %>%
    as.matrix(.,
              nrow = ncol(.),
              ncol = ncol(.))
  rownames(pred_mtx) <- colnames(pred_mtx)
  
  meth_str <- as.character(a$mi_m)
  
  out <- list(predictor_matrix = pred_mtx,
              method_string = meth_str)
}

# Function: Combine PS weighting and MI ---------------------------------------------

list_for_mi$EstPSw <- function(data,
                               indices,
                               ps_formula,
                               mi_mthd,
                               pred_mtrx,
                               nbr_imputations,
                               nbr_iterations,
                               reject_lim,
                               reference_in_ATT,
                               truncate_upper_quantile,
                               subst_model_of_int) {
  
  invisible(capture.output(impd <- smcfcs::smcfcs(
    data[indices, ],
    smtype = "logistic",
    smformula = ps_formula,
    method = mi_mthd,
    predictorMatrix = pred_mtrx,
    m = nbr_imputations,
    numit = nbr_iterations,
    rjlimit = reject_lim,
    noisy = FALSE
  )))
  
  w_out <-
    suppressMessages(purrr::map(
      impd[[1]],
      ~ list_for_mi$WeightAndTrunc(ps_formula,
                                   data = .x,
                                   reference_in_ATT,
                                   truncate_upper_quantile)
    ))
  
  suppressWarnings(mean(purrr::map_dbl(
    1:nbr_imputations,
    ~ stats::glm(
      as.formula(subst_model_of_int),
      family = binomial(link = "identity"),
      data = impd[[1]][[.x]],
      weights = w_out[[.x]]
    )$coeff[2]
  )))
}




# Function: Bootstrap full analysis ----------------------------------------

list_for_mi$BootInChunks <- function(first_seed,
                                     last_seed,
                                     bs_replicates = 25,
                                     boot_cores,
                                     map_cores,
                                     ps_model,
                                     analysis_model) {
  
  BSout <- function(many_seeds,
                    boot_cores,
                    map_cores,
                    bs_replicates,
                    ps_model = "migr ~ age + age_sb + sex + who + who_sb + viol + ses",
                    analysis_model = "ptsd >= 2.5 ~ migr") {
    BS <- function(one_seed) {
      set.seed(one_seed)
      boot_out <- boot::boot(
        data = dta,
        list_for_mi$EstPSw,
        ps_formula = as.formula(ps_model),
        mi_mthd = list_for_mi$method_string,
        pred_mtrx = list_for_mi$predictor_matrix,
        nbr_imputations = 10,
        nbr_iterations = 40,
        reject_lim = 800,
        reference_in_ATT = "1",
        truncate_upper_quantile = 0.99,
        subst_model_of_int = analysis_model,
        R = bs_replicates,
        parallel = "multicore",
        ncpus = boot_cores)
    }
    
    future::plan(multisession, workers = map_cores)
    
    furrr::future_map(
      many_seeds,
      ~ BS(one_seed = .x),
      .options = furrr_options(seed = 451)) %>%
      rlang::set_names(many_seeds)
  }
  seeds <- first_seed:last_seed
  
  start_time <- Sys.time()
  boot_out <- BSout(many_seeds = seeds,
                    bs_replicates = bs_replicates,
                    boot_cores = boot_cores,
                    map_cores = map_cores)
  end_time <- Sys.time()
  
  cat("\n", "Time elapsed: ", end_time - start_time, "\n\n")
  
  write_rds(boot_out,
            paste0("boot_out_chunk_",
                   min(seeds),
                   "_to_",
                   max(seeds), "_",
                   end_time,
                   ".rds"))
  
  return(boot_out)
}