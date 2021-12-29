# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@clin.au.dk
# Title:    ARCH: PTSD, merge the bootstrap chunks to one final data set
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# Two small convienience functions -----------------------------------------------------

PathToFiles <- function(path_to_files) {
  p <- list.files(path = path_to_files,
                         full.names = TRUE)
  if(length(p) == 0) {
    stop("Empty or non-existing folder specified - are you in the right working directory?")
  } else {
    return(p)
  }}


CombineFromList <- function(list_to_combine) {
  library(boot)
  out <- list_to_combine[[1]]
  for(i in names(list_to_combine)[-1]) {
    out <- c(out, list_to_combine[[i]])
  }
  out
}

# Retrieve the bootstrap chunks -------------------------------------------

#set the path according to where the bootstrap chunks are stored (in step 2)
files_list <- PathToFiles("data/boot_out")

boot_collect <- purrr::map(files_list, readr::read_rds)
boot_collect <- unlist(boot_collect, recursive = FALSE)

# Some seeds may produce errors. Remove them e.g.
boot_collect[["13"]] <- NULL

# Merge the chunks (one list for each)
bs_data_final <- CombineFromList(boot_collect)

# readr::write_rds(bs_data_final, "data/bs_data_final.rds")


