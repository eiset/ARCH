Diagnostics, convergence of multiple imputation
================
Andreas Halgreen Eiset
04/11/2020

  - [Do the imputations](#do-the-imputations)
  - [Plot: convergence](#plot-convergence)

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

# Do the imputations

``` r
set.seed(451)
mi_diagnostics <- smcfcs(dta,
               smtype = "logistic",
               smformula = as.formula(migr ~ age + age_sb + sex + who + who_sb + viol + ses),
               method = list_for_mi$method_string,
               predictorMatrix = list_for_mi$predictor_matrix,
               m = 1,
               numit = 200,
               rjlimit = 1000,
               noisy = FALSE)
```

# Plot: convergence

``` r
covars_in_subst_model <- c("age", "age_sb", "sex_Female", "who", "who_sb", "viol_Yes", "ses_BelowAverage", "ses_Average", "ses_AboveOrRefuse")

map(1:length(covars_in_subst_model),
    function(x) plot(mi_diagnostics$smCoefIter[1, x, ],
                     main = paste0("Convergence plot for covariable:   ", covars_in_subst_model[x])
    )
)
```

<img src="diagnostics_convergence201125_files/figure-gfm/the plots-1.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-2.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-3.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-4.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-5.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-6.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-7.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-8.png" width="50%" /><img src="diagnostics_convergence201125_files/figure-gfm/the plots-9.png" width="50%" />
