# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@ph.au.dk
# Title:    ARCH: PTSD, run analysis in bootstrap chunks
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

source("setup.R")

dta <- readr::read_rds("mock_data/full_data_set.rds")
list_for_mi <- readr::read_rds("mock_data/list_for_mi.rds")

# Set first and last seed, number of replications and cores as appropriate.
# By default each seed produces 25 bootstrap replicates, that is, to get 999,
# replicates the analysis must be run with 40 different seeds

# Because of the bootstrapping there may be instances of data set replicates with
# no missing in a variable that has missings in the original data set. In these
# instances the multiple imputation will fail and throw an error. Thus, the seeds
# needed to run may be well above the ideal situation

boot_out <- list_for_mi$BootInChunks(first_seed = 1,
                                     last_seed = 10,
                                     bs_replicates = 25,
                                     boot_cores = 5, #beware: the number of cores used are at some places in the code approximately multiplied 
                                     map_cores = 6)

# The function saves the chunks in the working directory. The chunks should then
# be moved to an appropriate location e.g. the "data" folder
