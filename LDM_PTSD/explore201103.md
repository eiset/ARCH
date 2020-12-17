ARCH:PTSD, Explore
================
Andreas Halgreen Eiset
03/11/2020

<details>

<summary>*Click here for R and package version*</summary> R version and
loaded packages

``` r
version
```

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

``` r
print(sapply(tmps$pckg, packageVersion))
```

    ## $skimr
    ## [1] 2 1 2
    ## 
    ## $viridis
    ## [1] 0 5 1
    ## 
    ## $ggmosaic
    ## [1] 0 2 0
    ## 
    ## $shadowtext
    ## [1] 0 0 7

</details>

``` r
skim(dta)
```

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | dta  |
| Number of rows                                   | 712  |
| Number of columns                                | 17   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| factor                                           | 10   |
| numeric                                          | 7    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: factor**

| skim\_variable   | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                           |
| :--------------- | ---------: | -------------: | :------ | --------: | :------------------------------------ |
| sex              |         38 |           0.95 | FALSE   |         2 | Fem: 461, Mal: 213                    |
| marital\_status  |         34 |           0.95 | FALSE   |         2 | Mar: 534, Oth: 144                    |
| gov\_of\_orig    |        145 |           0.80 | FALSE   |         6 | Hom: 141, Dei: 119, Oth: 119, Ale: 78 |
| highest\_educ    |         39 |           0.95 | TRUE    |         4 | Pri: 275, No : 223, Hig: 137, Hig: 38 |
| ses              |        101 |           0.86 | TRUE    |         3 | On : 404, Bel: 155, Don: 52           |
| smoking          |         37 |           0.95 | FALSE   |         2 | nev: 465, cur: 210                    |
| expir\_violence  |         74 |           0.90 | FALSE   |         2 | No: 474, Yes: 164                     |
| long\_dist\_migr |          0 |           1.00 | FALSE   |         2 | no: 599, yes: 113                     |
| children         |         48 |           0.93 | TRUE    |         4 | 0-2: 267, 5+: 205, 4: 109, 3: 83      |
| ptsd             |         33 |           0.95 | FALSE   |         2 | yes: 380, no: 299                     |

**Variable type: numeric**

| skim\_variable      | n\_missing | complete\_rate |   mean |    sd |  p0 |    p25 |    p50 |    p75 | p100 | hist  |
| :------------------ | ---------: | -------------: | -----: | ----: | --: | -----: | -----: | -----: | ---: | :---- |
| age                 |         68 |           0.90 |  36.49 | 13.13 |  18 |  26.00 |  34.00 |  45.00 |   90 | ▇▆▃▁▁ |
| bp\_sys             |         55 |           0.92 | 121.87 | 17.10 |  75 | 110.00 | 120.00 | 130.00 |  190 | ▁▇▅▁▁ |
| hgt                 |         45 |           0.94 | 164.56 | 10.15 | 139 | 156.00 | 165.00 | 171.00 |  195 | ▂▆▇▃▁ |
| who5\_score         |         51 |           0.93 |  30.65 | 24.87 |   0 |   8.00 |  24.00 |  48.00 |  100 | ▇▅▂▂▁ |
| htq\_score          |         33 |           0.95 |   2.53 |  0.71 |   1 |   2.06 |   2.62 |   3.06 |    4 | ▃▅▇▇▂ |
| hscl\_anx\_score    |         60 |           0.92 |   2.37 |  0.74 |   1 |   1.80 |   2.40 |   2.90 |    4 | ▆▆▇▆▂ |
| hscl\_depres\_score |        128 |           0.82 |   2.34 |  0.68 |   1 |   1.87 |   2.33 |   2.80 |    4 | ▅▇▇▅▂ |

``` r
cont_vars <- dta[, purrr::map_lgl(dta, is.numeric)] %>% 
  pivot_longer(everything(), names_to = "vrbl", values_to = "vals")

ggplot(cont_vars) +
  facet_wrap(~ vrbl, scales = "free") +
  geom_bar(aes(vals))
```

### Functions to create plots

``` r
# For cont vs cat
PlotContCat <- function(data_set, cat_response) {
  dta_norm <- data_set[, purrr::map_lgl(data_set, is.numeric)] %>%
    dplyr::mutate_all(as.numeric)
  
  dta_mutate_miss <- dta_norm %>%
    dplyr::mutate_all(function(x)
      if_else(is.na(x),
                    min(x, na.rm = TRUE) - mad(x, low = TRUE, na.rm = TRUE),
                    x)) %>%
    dplyr::bind_cols(data_set[cat_response]) %>%
    tidyr::pivot_longer(-!!cat_response, names_to = "vrbls", values_to = "vals")
  
  dta_norm <- dta_norm %>%
    dplyr::bind_cols(data_set[cat_response]) %>%
    tidyr::pivot_longer(-cat_response, names_to = "vrbls", values_to = "vals")
  
  ggplot(dta_norm, aes(x = .data[[cat_response]], y = vals)) +
    geom_violin(scale = "count") +
    geom_count(data = dta_mutate_miss,
               aes(x = .data[[cat_response]], y = vals, colour = ..n..),
               alpha = 0.6,
               inherit.aes = FALSE) +
    geom_boxplot(width = 0.1, color = "black", alpha = 0.1) +
    #geom_jitter(data = dta_mutate_miss, height = 0.1, width = 0.1, shape = 1, alpha = 0.3, colour = "#440154FF") +
    facet_wrap( ~ vrbls, scales = "free", ncol = 2) +
    labs(title = NULL, y = NULL) +
    guides(x = guide_axis(angle = 45), colour = "legend") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 10), position = "top") +
    scale_color_viridis_c() +
    theme_bw() +
    theme(axis.text.x = element_text(size = rel(0.8)), #adjust text relative to default in theme_bw()(where size=11)
          legend.position = "none") 
}


# For cont vs cont
PlotContCont <- function(data_set, cont_response) {
    d <- data_set[, purrr::map_lgl(data_set, is.numeric)] %>%
        dplyr::mutate_all(as.numeric)#to loose possible annoyances e.g. labels etc.
    
    dta_mutate_miss <- d %>%
        dplyr::mutate_all(function(x)
            if_else(is.na(x),
                    min(x, na.rm = TRUE) - mad(x, low = TRUE, na.rm = TRUE),
                    x))
    
    ThePlot <- function(cont_predictor) {
        g <- ggplot(dta_mutate_miss, aes(x = .data[[cont_predictor]], 
                                        y = .data[[cont_response]])) +
          geom_count(aes(colour = ..n..), alpha = 0.6) +
          #facet_wrap(~ .data[[cont_predictor]]) +
          geom_smooth(data = d) +
          #guides(colour = "legend") +
          #labs(x = .data[[cont_response]], y = .data[[cont_predictor]]) +
          scale_colour_viridis_c() +
          theme_bw() +
          labs(title = paste0("Response: ", cont_response)) +
          theme(legend.position = "none")
    }
    
    map(colnames(d), ThePlot)
}


  


# For cat vs cat - måske kan loop laves om til pivot + bind_col som cont-cat ovenfor?
# Plots

#FOR THIS PLOT: POSSIBLE TO WRITE "Missing: n = XXXX" FOR EACH COLUMN AND ROW?
PlotCatCat <- function(catcat_data, response) {
  resp <- rlang::sym(response)
  for (n in 1:ncol(catcat_data)) {
    cat("\n")
    pred_name <- sym(colnames(catcat_data[n]))
    out <- ggplot(catcat_data) +
      geom_mosaic(aes(
        x = product(x = !!resp, y = !!pred_name),
        fill = !!resp), na.rm = TRUE) +
      labs(x = as.character(pred_name), y = response) +
      scale_fill_viridis_d(option = "D", na.value = "grey") +
      guides(x = guide_axis(angle = 45),
             colour = "legend") +
      theme_bw() +
    theme(axis.text.x = element_text(size = rel(0.8))) + #adjust text relative to default in theme_bw()(where size=11)
      geom_shadowtext(
        data = 
          ggplot2::layer_data(ggplot2::last_plot(), 1) %>%
          dplyr::group_by(.[2]) %>% 
          dplyr::select(xmin, xmax, ymin, ymax, .wt) %>%
          dplyr::mutate(mx = ((xmin + xmax) / 2), 
                        my = ((ymin + ymax) / 2),
                        prop = round(100 * .wt / sum(.wt), 2)) %>%
          dplyr::filter(.wt > 4) %>% 
          dplyr::select(mx, my, .wt, prop),
        aes(x = mx, y = my, label = paste0("n = ", .wt, "\n"," (", prop, "%)")), 
        size = 3, lineheight = 0.8) #possibly need to adjust size
    plot(out)
    cat("\n")
  }
}

# NOT USED since mosaic plot with n in each gives same and more info:
#Tables, OBS: muligvis bedre at bruge pakke "gt" i stedet for pander
FunTableCat <- function(df_only_cat_vars) {
  names_cat_vars <- colnames(df_only_cat_vars)
  for (n in names_cat_vars) {
    for (i in names_cat_vars) {
      out <- table(df_only_cat_vars[, n],
                   df_only_cat_vars[, i],
                   useNA = "always")
      cat(paste("Response:", n, "\n", "Predictor:", i, "\n", pander::pander(out), "\n"))
    }
  }
}
```

# Predictors to use in MI models

## Remove outcome of substantive model of interest

``` r
#this is not relevant in this version of the data
dta <- dta %>% 
  dplyr::select(-htq_score)
```

## Plots: Continuous response vs continuous predictor

<button class="btn btn-primary" data-toggle="collapse" data-target="#Continuous-continuous_plot">

Show/Hide

</button>

<div id="Continuous-continuous_plot" class="collapse">

NA values are plotted as (minimum value - MAD)

<img src="/home/ahe/Insync/a.halgreeneiset@gmail.com/Google Drive/projekt_manus_artikel/1_manus/ARCH.Mental.Health/colour_coding_scale.jpg" width="50%" style="display: block; margin: auto;" />

``` r
cont_vrbls <- dta[, purrr::map_lgl(dta, is.numeric)] %>%
  colnames() 

map(cont_vrbls, function(x) PlotContCont(dta, x))
```

<img src="explore201103_files/figure-gfm/cont pred / cont response-1.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-2.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-3.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-4.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-5.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-6.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-7.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-8.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-9.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-10.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-11.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-12.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-13.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-14.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-15.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-16.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-17.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-18.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-19.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-20.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-21.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-22.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-23.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-24.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-25.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-26.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-27.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-28.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-29.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-30.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-31.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-32.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-33.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-34.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-35.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-36.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-37.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-38.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-39.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-40.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-41.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-42.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-43.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-44.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-45.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-46.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-47.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-48.png" width="50%" /><img src="explore201103_files/figure-gfm/cont pred / cont response-49.png" width="50%" />

</div>

## Plots: Categorical response vs continuous predictor

<button class="btn btn-primary" data-toggle="collapse" data-target="#Categorical-continuous_plot">

Show/Hide

</button>

<div id="Categorical-continuous_plot" class="collapse">

6 plot for each response: sex, marital\_status, gov\_of\_orig,
highest\_educ, ses, smoking, expir\_violence<br/> NA values in
continuous predictors are plotted as (minimum value - MAD)

<img src="/home/ahe/Insync/a.halgreeneiset@gmail.com/Google Drive/projekt_manus_artikel/1_manus/ARCH.Mental.Health/colour_coding_scale.jpg" width="50%" style="display: block; margin: auto;" />

``` r
vars_to_x <- unlist(str_split("sex, marital_status, gov_of_orig, highest_educ, ses, smoking, expir_violence, children", pattern = ", "))

map(vars_to_x, function(x) PlotContCat(dta, x))
```

<img src="explore201103_files/figure-gfm/cont pred / categ responses-1.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-2.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-3.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-4.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-5.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-6.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-7.png" width="100%" /><img src="explore201103_files/figure-gfm/cont pred / categ responses-8.png" width="100%" />

</div>

## Plots: Categorical response vs. categorical predictor

<button class="btn btn-primary" data-toggle="collapse" data-target="#Categorical-ccategorical_plot">

Show/Hide

</button>

<div id="Categorical-ccategorical_plot" class="collapse">

Observations with cell proportion \< 0.5 are not labeled<br/>
Percentages are of *column* sums (predictors)

``` r
cat_df <- dta[, !purrr::map_lgl(dta, is.numeric)] %>%
      dplyr::select(-long_dist_migr)

d <-  cat_df %>% 
  mutate_all(function(x) str_wrap(x, 10)) #to wrap text

map(colnames(d), function(x) PlotCatCat(catcat_data = d, x))
```

<img src="explore201103_files/figure-gfm/cat response and cat predictors-1.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-2.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-3.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-4.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-5.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-6.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-7.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-8.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-9.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-10.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-11.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-12.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-13.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-14.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-15.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-16.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-17.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-18.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-19.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-20.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-21.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-22.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-23.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-24.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-25.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-26.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-27.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-28.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-29.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-30.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-31.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-32.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-33.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-34.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-35.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-36.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-37.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-38.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-39.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-40.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-41.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-42.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-43.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-44.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-45.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-46.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-47.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-48.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-49.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-50.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-51.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-52.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-53.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-54.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-55.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-56.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-57.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-58.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-59.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-60.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-61.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-62.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-63.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-64.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-65.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-66.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-67.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-68.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-69.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-70.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-71.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-72.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-73.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-74.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-75.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-76.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-77.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-78.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-79.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-80.png" width="100%" /><img src="explore201103_files/figure-gfm/cat response and cat predictors-81.png" width="100%" />

</div>

``` r
#No use for tables but here is to run the function:
FunTableCat(cat_df)
```
