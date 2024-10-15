# LOCATE

Analysis of the NAFLD-LOCATE randomised trial.

`data` folder contains synthetic data created using the [synthpop](https://cran.r-project.org/web/packages/synthpop/index.html) package.

`images` folder contains images used to generate letters to GPs.

The sample size calculation using simulation is in `NAFLD.sample.size.survival.R`.

### R and package versions

We used the following version of R and packages.

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8    LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                       LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] gridExtra_2.3   mitools_2.4     nimble_1.0.1    broom_1.0.5     survminer_0.4.9 ggpubr_0.6.0    ggplot2_3.4.2   survival_3.5-5  stringr_1.5.0   janitor_2.2.0   flextable_0.9.2 tidyr_1.3.0     dplyr_1.1.2     knitr_1.43     

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0        farver_2.1.1            fastmap_1.1.1           pracma_2.4.2            fontquiver_0.2.1        promises_1.2.0.1        digest_0.6.31           timechange_0.2.0        mime_0.12               lifecycle_1.0.3         ellipsis_0.3.2          gfonts_0.2.0            magrittr_2.0.3          compiler_4.3.1          rlang_1.1.1             tools_4.3.1             igraph_1.5.0            utf8_1.2.3              yaml_2.3.7              data.table_1.14.8       ggsignif_0.6.4          labeling_0.4.2          askpass_1.1             curl_5.0.1              xml2_1.3.4              abind_1.4-5             httpcode_0.3.0          numDeriv_2016.8-1.1     withr_2.5.0             purrr_1.0.1             grid_4.3.1              fansi_1.0.4             gdtools_0.3.3           xtable_1.8-4            colorspace_2.1-0        scales_1.2.1            crul_1.4.0              cli_3.6.1               rmarkdown_2.22          crayon_1.5.2            ragg_1.2.5             
[42] generics_0.1.3          rstudioapi_0.14         km.ci_0.5-6             commonmark_1.9.0        DBI_1.1.3               splines_4.3.1           parallel_4.3.1          survMisc_0.5.6          vctrs_0.6.3             Matrix_1.5-4.1          jsonlite_1.8.5          fontBitstreamVera_0.1.1 carData_3.0-5           car_3.1-2               rstatix_0.7.2           systemfonts_1.0.4       glue_1.6.2              codetools_0.2-19        ggtext_0.1.2            lubridate_1.9.2         stringi_1.7.12          gtable_0.3.3            later_1.3.1             munsell_0.5.0           tibble_3.2.1            pillar_1.9.0            htmltools_0.5.5         openssl_2.0.6           R6_2.5.1                KMsurv_0.1-5            textshaping_0.3.6       evaluate_0.21           shiny_1.7.4             lattice_0.21-8          highr_0.10              markdown_1.7            backports_1.4.1         gridtext_0.1.5          snakecase_0.11.0        fontLiberation_0.1.0    httpuv_1.6.11          
[83] TeachingDemos_2.12      Rcpp_1.0.10             zip_2.3.0               uuid_1.1-0              coda_0.19-4             officer_0.6.2           xfun_0.39               zoo_1.8-12              pkgconfig_2.0.3        
```
