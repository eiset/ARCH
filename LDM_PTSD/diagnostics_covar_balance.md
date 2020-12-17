Diagnostics, covariate balance
================
Andreas Halgreen Eiset
04/11/2020

  - [Functions used](#functions-used)
  - [Set up the data](#set-up-the-data)
  - [Plots](#plots)

<details>

<summary>*Click here for R and package version*</summary> R version and
loaded packages

    ##                _                           
    ## platform       x86_64-pc-linux-gnu         
    ## arch           x86_64                      
    ## os             linux-gnu                   
    ## system         x86_64, linux-gnu           
    ## status                                     
    ## major          4                           
    ## minor          0.3                         
    ## year           2020                        
    ## month          10                          
    ## day            10                          
    ## svn rev        79318                       
    ## language       R                           
    ## version.string R version 4.0.3 (2020-10-10)
    ## nickname       Bunny-Wunnies Freak Out

    ## $ggplot2
    ## [1] 3 3 2
    ## 
    ## $gridExtra
    ## [1] 2 3
    ## 
    ## $tidyverse
    ## [1] 1 3 0
    ## 
    ## $smcfcs
    ## [1] 1 4 1
    ## 
    ## $WeightIt
    ## [1]  0 10  2
    ## 
    ## $cobalt
    ## [1] 4 2 3
    ## 
    ## $boot
    ## [1]  1  3 25

</details>

### Functions used

``` r
ImputeOneTime <- function(data, ps_formula, mthd, mtrx) {
    smcfcs::smcfcs(data,
               smtype = "logistic",
               smformula = ps_formula,
               method = mthd,
               predictorMatrix = mtrx,
               m = 1,
               numit = 40,
               rjlimit = 800,
               noisy = FALSE)
}

WeightAndTrunc <-
    function(ps_formula,
             data,
             truncate_percentile) {

        a <- WeightIt::weightit(
            formula = ps_formula,
            data = data,
            estimand = "ATT",
            focal = "1",
            method = "ps")

        b <- purrr::map(truncate_percentile,
            ~ WeightIt::trim(a,
                             at = .x,
                             lower = TRUE))

        map(1:length(b), ~ b[[.x]]$weights) %>%
            rlang::set_names(truncate_percentile)
    }

BalancePlot <- function(data_set_name) {
    a <- data_set_name

    set.seed(451)
    b <- ImputeOneTime(data = dta[[a]], 
                       ps_formula = list_for_mi$ps_formula[[a]],
                       mthd = list_for_mi$method_string[[a]],
                       mtrx = list_for_mi$predictor_matrix[[a]])

    w_out <- WeightAndTrunc(list_for_mi$ps_formula[[a]],
                            data = data.frame(b[[1]]),
                            truncate_percentile = list_for_mi$truncate_at)

    plot_balance <- cobalt::love.plot(list_for_mi$ps_formula[[a]],
                              data = data.frame(b[[1]]),
                              estimand = "ATT",
                              focal = "1",
                              weights = list(w1 = w_out[["0.99"]],
                                             w2 = w_out[["0.95"]]),
                              var.order = "w1",
                              abs = TRUE,
                              line = TRUE,
                              stars = "raw",
                              thresholds = c(m = .1),
                              #var.names = new.names,
                              shapes = c("triangle filled", "circle filled", "square filled"),
                              sample.names = c("No truncation",
                                               "...at .01 and .99",
                                               "...at .05 and .95"),
                              limits = c(0, .70),
                              title = paste0("PS model: ",
                                             "'",
                                             as.name(data_set_name),
                                             "'")) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::theme(legend.position = c(.75, .25),
              legend.box.background = element_rect(),
              legend.box.margin = margin(1, 1, 1, 1))

    CorrectLegendTitle <- function(type) {
        plot_balance[["labels"]][[type]] <<- "Weight truncation"
    }

    purrr::map(c("colour", "shape", "size", "stroke"), CorrectLegendTitle)
    plot_balance
}

GridArrangeSharedLegend <- function(...,
                                    ncol = 1,
                                    nrow = length(list(...)),
                                    position = c("right", "right")) {

        plots <- list(...)

        RemoveMultipleXtitle <- function(plot_nbr) {
            plots[[plot_nbr]][["labels"]]$x <<- NULL
            plots[[plot_nbr]][["labels"]]$xintercept <<- NULL
        }

        map(1:(length(plots)-1), RemoveMultipleXtitle)

        position <- match.arg(position)
        g <- ggplot2::ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs

        legend <- g[[which(sapply(g, function(x)
            x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x)
            x + theme(legend.position = "none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)

        combined <- switch(
            position,
            "bottom" = gridExtra::arrangeGrob(
                do.call(arrangeGrob, gl),
                legend,
                ncol = 1,
                heights = grid::unit.c(unit(1, "npc") - lheight, lheight)
            ),
            "right" = gridExtra::arrangeGrob(
                do.call(arrangeGrob, gl),
                legend,
                ncol = 2,
                widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)
            )
        )

        grid::grid.newpage()
        grid::grid.draw(combined)

        # return gtable invisibly
        invisible(combined)

    }
```

### Set up the data

``` r
tmps$mts <- list_for_mi$method_string
list_for_mi$method_string <- NULL
list_for_mi$method_string$simple <- tmps$mts
list_for_mi$method_string$intmed <- tmps$mts[-6]
list_for_mi$method_string$complex <- tmps$mts[-c(6, 10, 11)]

tmps$mtrx <- list_for_mi[["predictor_matrix"]]
list_for_mi$predictor_matrix$simple <- tmps$mtrx
list_for_mi$predictor_matrix$intmed <- tmps$mtrx[-6, -6]
list_for_mi$predictor_matrix$complex <- tmps$mtrx[-c(6, 10, 11), -c(6, 10, 11)]

list_for_mi$ps_formula <- NULL
list_for_mi$ps_formula$simple <- as.formula(migr ~ age + age_sb + sex + who + who_sb + viol + ses)
list_for_mi$ps_formula$intmed <- as.formula(migr ~ age + age_sb + sex + who + who_sb + viol + ses + age*sex)
list_for_mi$ps_formula$complex <- as.formula(migr ~ age + age_sb + sex + who + who_sb + viol + ses + age*sex + sex*who + age*who)
list_for_mi$truncate_at <- c(0, 0.99, 0.95)
```

# Plots

``` r
p <- map(c("simple", "intmed", "complex"), ~ BalancePlot(.x))

GridArrangeSharedLegend(p[[1]], p[[2]], p[[3]])
```

<img src="diagnostics_covar_balance201125_rcs_i_subst_model_files/figure-gfm/cont pred / categ responses-1.png" width="100%" />
<br> \*indicates variables for which the displayed value is the raw
(unstandardized) difference in means.
