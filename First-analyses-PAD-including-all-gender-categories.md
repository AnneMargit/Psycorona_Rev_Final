First analyses PAD including all gender categories
================
Anne Margit
4/25/2022

    ## [1] ""

``` r
load("data_analyse1_fcg.Rdata")

data_analyse1_fc <- data_analyse1_fcg
```

This dataset includes:

1.  Data from all weekly measurement waves (baseline through wave 11,
    Time 1 through 12)
2.  Participants who provided at least 3 measurements
3.  Participants who are residents of the country they currently live in
4.  Participants who provided info on age
5.  Participants who provided info on gender (either male or female)
6.  Data from countries with at least 20 participants
7.  Pooled age groups
8.  Imputed missing emotion scores
9.  Combined emotion scores (NAA, NAD, PAA, PAD)
10. An imputed Stringency index (StringencyIndex_imp)
11. A variable indicating the number of days before and after the day on
    which maximum stringency was reached for the respective country
    (DaysMax)
12. A variable indicating the number of weeks before and after the day
    on which maximum stringency was reached for the respective country
    (WeeksMax)
13. A variable indicating the date on which maximum Stringency was
    reached for that country (DateMaxStr)
14. A dummy Str_dummy with 0 = before the peaj, 1 = during peak, 2 =
    after peak
15. Observations during which there was a second peak are excluded
    (N=583)

> My comments are in block quotes such as this.

``` r
library(dplyr)
library(tidyverse)
library(papaja)
library(ggpubr)
library(ggplot2)
library(rockchalk)
library(effects)
library(nlme)
library(lattice)
library(broom)
library(broom.mixed)
library(purrr)
```

# Regression models

**Positive affect low arousal**

*Stringency Index x dummy interaction: random intercept for Country*

``` r
model_PAD1 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = ~1 | Country, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD1)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC    BIC    logLik
      164417.1 164489 -82200.53

    Random effects:
     Formula: ~1 | Country
            (Intercept)  Residual
    StdDev:   0.2089011 0.9721823

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8118957 0.04046825 59006 69.48399  0.0000
    StringencyIndex_dev             0.0046733 0.00212895 59006  2.19511  0.0282
    Str_dummy1                      0.1730276 0.02380333 59006  7.26905  0.0000
    Str_dummy2                      0.1953186 0.01892911 59006 10.31842  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0093279 0.00280896 59006 -3.32076  0.0009
    StringencyIndex_dev:Str_dummy2 -0.0058342 0.00223948 59006 -2.60514  0.0092
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.105                             
    Str_dummy1                     -0.277 -0.270                      
    Str_dummy2                     -0.383 -0.224  0.575               
    StringencyIndex_dev:Str_dummy1 -0.099 -0.660 -0.272  0.254        
    StringencyIndex_dev:Str_dummy2 -0.102 -0.967  0.270  0.256  0.617 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.53373352 -0.78998707  0.04245592  0.74751538  2.56580869 

    Number of Observations: 59044
    Number of Groups: 33 

*Stringency Index x dummy interaction: random intercept for ID*

``` r
model_PAD2 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | ID, 
                 data = data_analyse1_fc, 
                 na.action = na.omit)

summary(model_PAD2)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      134967.2 135039.1 -67475.59

    Random effects:
     Formula: ~1 | ID
            (Intercept)  Residual
    StdDev:    0.769269 0.6250041

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value   Std.Error    DF   t-value p-value
    (Intercept)                     2.8101804 0.013931946 48529 201.70768  0.0000
    StringencyIndex_dev             0.0057366 0.001469624 48529   3.90342  0.0001
    Str_dummy1                      0.1194215 0.015193594 48529   7.85999  0.0000
    Str_dummy2                      0.1721699 0.013072239 48529  13.17065  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0045377 0.001839224 48529  -2.46719  0.0136
    StringencyIndex_dev:Str_dummy2 -0.0070187 0.001534231 48529  -4.57473  0.0000
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.232                             
    Str_dummy1                     -0.690 -0.264                      
    Str_dummy2                     -0.759 -0.238  0.654               
    StringencyIndex_dev:Str_dummy1 -0.152 -0.736 -0.165  0.246        
    StringencyIndex_dev:Str_dummy2 -0.223 -0.967  0.259  0.270  0.700 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.24572164 -0.56119309  0.05790307  0.57778678  4.61339533 

    Number of Observations: 59044
    Number of Groups: 10510 

*Random intercept for Country and ID*

``` r
model_PAD3 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  control = list(maxIter = 100, opt = "optim"))

summary(model_PAD3)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      134390.6 134471.5 -67186.31

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2053184

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7437785 0.6247387

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8332766 0.03953144 48529 71.67148   0e+00
    StringencyIndex_dev             0.0051710 0.00148728 48529  3.47680   5e-04
    Str_dummy1                      0.1788716 0.01657789 48529 10.78977   0e+00
    Str_dummy2                      0.1523540 0.01330024 48529 11.45498   0e+00
    StringencyIndex_dev:Str_dummy1 -0.0124825 0.00195607 48529 -6.38140   0e+00
    StringencyIndex_dev:Str_dummy2 -0.0062404 0.00155630 48529 -4.00976   1e-04
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.078                             
    Str_dummy1                     -0.198 -0.280                      
    Str_dummy2                     -0.272 -0.229  0.574               
    StringencyIndex_dev:Str_dummy1 -0.072 -0.660 -0.265  0.261        
    StringencyIndex_dev:Str_dummy2 -0.076 -0.968  0.278  0.259  0.623 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.29411322 -0.56396941  0.05659175  0.57985848  4.54224515 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country*

``` r
model_PAD4 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = ~1), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD4)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      134353.7 134452.6 -67165.86

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.204849814 (Intr)
    StringencyIndex_dev 0.003158768 0.215 

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7437241 0.6243171

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8355199 0.03953148 48529 71.72815  0.0000
    StringencyIndex_dev             0.0045928 0.00179155 48529  2.56362  0.0104
    Str_dummy1                      0.1672765 0.01932602 48529  8.65551  0.0000
    Str_dummy2                      0.1574297 0.01378301 48529 11.42201  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0115482 0.00229446 48529 -5.03309  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0048854 0.00182838 48529 -2.67197  0.0075
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.130                             
    Str_dummy1                     -0.190 -0.299                      
    Str_dummy2                     -0.275 -0.180  0.475               
    StringencyIndex_dev:Str_dummy1 -0.050 -0.523 -0.382  0.236        
    StringencyIndex_dev:Str_dummy2 -0.066 -0.868  0.192  0.243  0.648 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.28772985 -0.56144795  0.05693797  0.57901827  4.53834163 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for ID*

``` r
model_PAD5 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD5)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133977.5 134076.3 -66977.73

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2060109

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.74487855 (Intr)
    StringencyIndex_dev 0.01413524 -0.047
    Residual            0.61120560       

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8312509 0.03961461 48529 71.46987  0.0000
    StringencyIndex_dev             0.0048340 0.00162759 48529  2.97001  0.0030
    Str_dummy1                      0.1765870 0.01693550 48529 10.42703  0.0000
    Str_dummy2                      0.1582168 0.01328877 48529 11.90605  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0117759 0.00206143 48529 -5.71253  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0052318 0.00171821 48529 -3.04494  0.0023
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.073                             
    Str_dummy1                     -0.192 -0.292                      
    Str_dummy2                     -0.268 -0.217  0.542               
    StringencyIndex_dev:Str_dummy1 -0.068 -0.659 -0.269  0.263        
    StringencyIndex_dev:Str_dummy2 -0.072 -0.950  0.257  0.260  0.653 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.38768457 -0.55143499  0.05511091  0.57483707  4.45285204 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country and ID*

``` r
model_PAD6 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~StringencyIndex_dev | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD6)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133963.7 134080.5 -66968.83

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.20531993 (Intr)
    StringencyIndex_dev 0.00306087 0.225 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.74478877 (Intr)
    StringencyIndex_dev 0.01391807 -0.046
    Residual            0.61122216       

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8329877 0.03956872 48529 71.59664  0.0000
    StringencyIndex_dev             0.0042349 0.00186791 48529  2.26716  0.0234
    Str_dummy1                      0.1717944 0.01920917 48529  8.94335  0.0000
    Str_dummy2                      0.1596946 0.01362492 48529 11.72078  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0114451 0.00230930 48529 -4.95610  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0045948 0.00188958 48529 -2.43164  0.0150
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.125                             
    Str_dummy1                     -0.186 -0.311                      
    Str_dummy2                     -0.271 -0.173  0.470               
    StringencyIndex_dev:Str_dummy1 -0.050 -0.533 -0.366  0.236        
    StringencyIndex_dev:Str_dummy2 -0.064 -0.872  0.204  0.235  0.656 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.38131024 -0.55154433  0.05568836  0.57381320  4.45774855 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country*

``` r
model_PAD7 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list(Country = pdDiag(~StringencyIndex_dev), ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD7)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133962.3 134070.1 -66969.14

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2051434         0.003150077

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.74478800 (Intr)
    StringencyIndex_dev 0.01391858 -0.046
    Residual            0.61121681       

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8341452 0.03955574 48529 71.64940  0.0000
    StringencyIndex_dev             0.0042898 0.00188336 48529  2.27777  0.0227
    Str_dummy1                      0.1698710 0.01923229 48529  8.83259  0.0000
    Str_dummy2                      0.1587030 0.01364167 48529 11.63369  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0112590 0.00231740 48529 -4.85845  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0045019 0.00190344 48529 -2.36514  0.0180
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.065                             
    Str_dummy1                     -0.188 -0.309                      
    Str_dummy2                     -0.272 -0.170  0.471               
    StringencyIndex_dev:Str_dummy1 -0.049 -0.534 -0.365  0.232        
    StringencyIndex_dev:Str_dummy2 -0.064 -0.869  0.200  0.230  0.660 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.38044358 -0.55174327  0.05526503  0.57415222  4.45928957 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for ID*

``` r
model_PAD8 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD8)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133964.1 134071.9 -66970.05

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.205267097 (Intr)
    StringencyIndex_dev 0.003093316 0.218 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7444141          0.01390691 0.6112072

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8329556 0.03956228 48529 71.60749  0.0000
    StringencyIndex_dev             0.0042523 0.00187024 48529  2.27369  0.0230
    Str_dummy1                      0.1718704 0.01923234 48529  8.93653  0.0000
    Str_dummy2                      0.1598284 0.01362689 48529 11.72889  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0114731 0.00231277 48529 -4.96077  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0046289 0.00189083 48529 -2.44805  0.0144
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.125                             
    Str_dummy1                     -0.187 -0.311                      
    Str_dummy2                     -0.271 -0.173  0.470               
    StringencyIndex_dev:Str_dummy1 -0.050 -0.532 -0.367  0.236        
    StringencyIndex_dev:Str_dummy2 -0.064 -0.871  0.203  0.235  0.657 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.38370665 -0.55184282  0.05601015  0.57358426  4.45927194 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country and ID*

``` r
model_PAD9 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAD9)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133962.7 134061.5 -66970.35

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:    0.205002         0.003170389

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7444147          0.01390889 0.6112012

    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8340789 0.03953349 48529 71.68804  0.0000
    StringencyIndex_dev             0.0043079 0.00188438 48529  2.28612  0.0223
    Str_dummy1                      0.1699999 0.01924641 48529  8.83281  0.0000
    Str_dummy2                      0.1588678 0.01364228 48529 11.64525  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0112916 0.00231976 48529 -4.86759  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0045395 0.00190388 48529 -2.38436  0.0171
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.067                             
    Str_dummy1                     -0.189 -0.309                      
    Str_dummy2                     -0.272 -0.170  0.471               
    StringencyIndex_dev:Str_dummy1 -0.049 -0.533 -0.366  0.233        
    StringencyIndex_dev:Str_dummy2 -0.064 -0.869  0.200  0.231  0.660 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.38286719 -0.55206979  0.05592551  0.57357421  4.46071983 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Autoregressive correlation structure*

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_PAD10 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAD10)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132875.2 132983.1 -66425.62

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2043816         0.002322395

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:    0.724393         0.008932987 0.6409951

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
        Phi1 
    0.258331 
    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8271207 0.03960143 48529 71.38935  0.0000
    StringencyIndex_dev             0.0048786 0.00180509 48529  2.70268  0.0069
    Str_dummy1                      0.1649833 0.01937147 48529  8.51682  0.0000
    Str_dummy2                      0.1631102 0.01459191 48529 11.17812  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0105723 0.00231704 48529 -4.56287  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0052557 0.00187982 48529 -2.79586  0.0052
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.075                             
    Str_dummy1                     -0.199 -0.308                      
    Str_dummy2                     -0.287 -0.185  0.489               
    StringencyIndex_dev:Str_dummy1 -0.063 -0.562 -0.340  0.251        
    StringencyIndex_dev:Str_dummy2 -0.073 -0.897  0.222  0.245  0.643 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.16703888 -0.55303768  0.06321431  0.58410276  4.25490412 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*AR without random slope for country*

``` r
model_PAD11 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAD11)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132887.9 132986.7 -66432.94

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2050954

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7244074          0.00914736 0.6410893

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2588189 
    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8253641 0.03964274 48529 71.27066  0.0000
    StringencyIndex_dev             0.0050892 0.00162238 48529  3.13689  0.0017
    Str_dummy1                      0.1731737 0.01757857 48529  9.85141  0.0000
    Str_dummy2                      0.1603664 0.01422835 48529 11.27090  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0112150 0.00210413 48529 -5.32999  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0059646 0.00171414 48529 -3.47966  0.0005
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.082                             
    Str_dummy1                     -0.202 -0.295                      
    Str_dummy2                     -0.283 -0.222  0.548               
    StringencyIndex_dev:Str_dummy1 -0.079 -0.652 -0.264  0.272        
    StringencyIndex_dev:Str_dummy2 -0.079 -0.956  0.275  0.261  0.629 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.17220174 -0.55372744  0.06408002  0.58350677  4.25436130 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

> Model PAD10 has the best fit (lowest BIC). This model has random
> slopes for Stringency at the ID and Country level, assumes no
> correlation between random slopes and intercepts, and assumes
> autoregressive correlation structure at the measurement level.

*QQ plot of residuals*

``` r
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
hist(resid(model_PAD10, type = "normalized"),
freq = FALSE, xlim = lims, ylim =  c(0,.7),main = "Histogram of Standardized Residuals")
lines(density(scale(resid(model_PAD10))))
qqnorm(resid(model_PAD10, type = "normalized"),
xlim = lims, ylim = lims,main = "QQ plot")
abline(0,1, col = "red", lty = 2)
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

*Residuals vs fitted*

``` r
plot(fitted(model_PAD10, level=2), residuals(model_PAD10, level=2), 
     main="residuals vs fitted at ID level")
abline(a=0, b=0,col="red")
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(fitted(model_PAD10, level=1), residuals(model_PAD10, level=1), 
    main="residuals vs fitted at Country level")
abline(a=0, b=0,col="red")
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

*Plot random intercepts and slopes*

``` r
plot(ranef(model_PAD10, level = 1))
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(ranef(model_PAD10, level = 2))
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

*Confidence intervals*

``` r
intervals(model_PAD10)
```

    Approximate 95% confidence intervals

     Fixed effects:
                                          lower         est.        upper
    (Intercept)                     2.749501349  2.827120671  2.904739993
    StringencyIndex_dev             0.001340574  0.004878576  0.008416578
    Str_dummy1                      0.127015006  0.164983337  0.202951668
    Str_dummy2                      0.134509831  0.163110171  0.191710511
    StringencyIndex_dev:Str_dummy1 -0.015113742 -0.010572322 -0.006030903
    StringencyIndex_dev:Str_dummy2 -0.008940202 -0.005255722 -0.001571242

     Random Effects:
      Level: Country 
                                  lower        est.       upper
    sd((Intercept))         0.155936162 0.204381647 0.267877937
    sd(StringencyIndex_dev) 0.001102675 0.002322395 0.004891303
      Level: ID 
                                  lower        est.     upper
    sd((Intercept))         0.712568687 0.724393038 0.7364136
    sd(StringencyIndex_dev) 0.007728942 0.008932987 0.0103246

     Correlation structure:
             lower     est.     upper
    Phi1 0.2436383 0.258331 0.2729051

     Within-group standard error:
        lower      est.     upper 
    0.6358425 0.6409951 0.6461895 

*Plot of predicted values*

``` r
ef_PAD <- effect("StringencyIndex_dev:Str_dummy", model_PAD10)

plot_PAD <- ggplot(as.data.frame(ef_PAD), 
       aes(StringencyIndex_dev, fit, color=Str_dummy)) + geom_line() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + theme_bw(base_size=12) + scale_color_discrete(name="Maximum stringency", labels = c("Before the peak", "During the peak", "After the peak")) + expand_limits(y=c(1, 5))
```

``` r
plot_PAD
```

![](First-analyses-PAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
coef_PAD = broom.mixed::tidy(model_PAD10, 
               effects = "fixed")
```

*Effect sizes* **Within person SD and average within person SD**

``` r
load("ISDs_av.Rdata")
```

> Effect size = (regression coefficient \* average ISD of X) / average
> ISD of Y)

> For the intercept and the dummy variables (+ interaction) I only
> standardized Y, so the effect size = (regression coefficient / average
> ISD of Y)

``` r
coef_PAD <- coef_PAD %>%
 left_join(., ISDs_av, by=c("term"="ind"))

coef_PAD <- coef_PAD %>%
  mutate(sd = ifelse(is.na(sd), 1, sd))

coef_PAD <- coef_PAD %>%
  mutate(sd = ifelse(row_number()== 5 | row_number()== 6, 6.12091, sd))

coef_PAD <- coef_PAD %>%
 mutate(e_size = (estimate * sd)/0.5614224)

coef_PAD <- coef_PAD %>%
  rename(isd = sd)
```

``` r
coef_PAD
```

    ## # A tibble: 6 × 9
    ##   effect term                           estimate std.error    df statistic  p.value   isd  e_size
    ##   <chr>  <chr>                             <dbl>     <dbl> <dbl>     <dbl>    <dbl> <dbl>   <dbl>
    ## 1 fixed  (Intercept)                     2.83      0.0396  48529     71.4  0         1     5.04  
    ## 2 fixed  StringencyIndex_dev             0.00488   0.00181 48529      2.70 6.88e- 3  6.13  0.0533
    ## 3 fixed  Str_dummy1                      0.165     0.0194  48529      8.52 1.69e-17  1     0.294 
    ## 4 fixed  Str_dummy2                      0.163     0.0146  48529     11.2  5.66e-29  1     0.291 
    ## 5 fixed  StringencyIndex_dev:Str_dummy1 -0.0106    0.00232 48529     -4.56 5.06e- 6  6.12 -0.115 
    ## 6 fixed  StringencyIndex_dev:Str_dummy2 -0.00526   0.00188 48529     -2.80 5.18e- 3  6.12 -0.0573
