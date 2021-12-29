# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@ph.au.dk
# Title:    ARCH: PTSD, run analysis in bootstrap chunks
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

source("R/load_packages_and_reset_graphics.R")

dta <- readr::read_rds("data/full_data_set.rds")
list_for_mi <- readr::read_rds("data/list_for_mi.rds")

# Set first and last seed, number of replications and cores as appropriate.
# By default each seed produces 25 bootstrap replicates, that is, to get 999,
# replicates the analysis must be run with 40 different seeds
boot_out <- list_for_mi$BootInChunks(first_seed = 1,
                                     last_seed = 40,
                                     bs_replicates = 25,
                                     boot_cores = 12,
                                     map_cores = 12)

# The function saves the chunks in the working directory. The chunks should then
# be moved to an appropriate location e.g. the "data" folder
