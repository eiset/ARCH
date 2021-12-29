# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset, eiset@clin.au.dk
# Title:    ARCH: PTSD, bootstrap diagnostics
# Licence:  GNU GPLv3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# Diagnostic plot for bootstrap replicates

dta <- readr::read_rds("data/bs_data_final.rds")
plot(dta)

pdf("diagnostic_bs.pdf",
    width = 7,
    height = 4)
plot(dta)
dev.off()

# To plot each bootstrap estimate to check extremes
library(ggplot2)

plot_dta <- tibble::tibble(bs_repli = dta$t)

g <- ggplot(plot_dta, aes(bs_repli)) + theme_bw()

g + geom_histogram(bins = 100)

ggplot(plot_dta, aes(sample = bs_repli)) +
  stat_qq() +
  stat_qq_line()



ggplot(plot_dta) +
  geom_point(aes(x = as.numeric(row.names(plot_dta)),
                 y = bs_repli)) +
  labs(x = "Bootstrap number",
       y = "Estimate")