First analyses NAD including all gender categories
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

**Negative affect low arousal**

*Stringency Index x dummy interaction: random intercept for Country*

``` r
model_NAD1 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = ~1 | Country, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD1)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      167288.5 167360.4 -83636.25

    Random effects:
     Formula: ~1 | Country
            (Intercept)  Residual
    StdDev:    0.236905 0.9960614

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2882779 0.04508601 59006 50.75362  0.0000
    StringencyIndex_dev             0.0058575 0.00218194 59006  2.68453  0.0073
    Str_dummy1                     -0.0218433 0.02440702 59006 -0.89496  0.3708
    Str_dummy2                     -0.0818875 0.01940622 59006 -4.21965  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0032408 0.00287975 59006 -1.12538  0.2604
    StringencyIndex_dev:Str_dummy2 -0.0042076 0.00229533 59006 -1.83311  0.0668
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.097                             
    Str_dummy1                     -0.254 -0.270                      
    Str_dummy2                     -0.353 -0.224  0.574               
    StringencyIndex_dev:Str_dummy1 -0.091 -0.660 -0.273  0.254        
    StringencyIndex_dev:Str_dummy2 -0.094 -0.967  0.270  0.256  0.616 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -1.8911782 -0.8177294 -0.1639015  0.6882018  3.1785207 

    Number of Observations: 59044
    Number of Groups: 33 

*Stringency Index x dummy interaction: random intercept for ID*

``` r
model_NAD2 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | ID, 
                 data = data_analyse1_fc, 
                 na.action = na.omit,
                 control = list(maxIter = 100, opt = "optim"))

summary(model_NAD2)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
         AIC      BIC    logLik
      133472 133543.9 -66728.01

    Random effects:
     Formula: ~1 | ID
            (Intercept)  Residual
    StdDev:   0.8100595 0.6097593

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value   Std.Error    DF   t-value p-value
    (Intercept)                     2.1917542 0.013948392 48529 157.13311  0.0000
    StringencyIndex_dev             0.0013204 0.001438638 48529   0.91785  0.3587
    Str_dummy1                      0.0361706 0.014980995 48529   2.41443  0.0158
    Str_dummy2                      0.0037289 0.012806837 48529   0.29117  0.7709
    StringencyIndex_dev:Str_dummy1 -0.0016359 0.001808686 48529  -0.90447  0.3658
    StringencyIndex_dev:Str_dummy2 -0.0002730 0.001502093 48529  -0.18172  0.8558
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.228                             
    Str_dummy1                     -0.672 -0.266                      
    Str_dummy2                     -0.742 -0.238  0.647               
    StringencyIndex_dev:Str_dummy1 -0.147 -0.729 -0.175  0.248        
    StringencyIndex_dev:Str_dummy2 -0.218 -0.967  0.261  0.269  0.693 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4089568 -0.5498087 -0.1192645  0.5235816  5.3705229 

    Number of Observations: 59044
    Number of Groups: 10510 

> Bij NAD2 en NAD3 wilde hij niet convergen dus heb ik een max iteration
> toegevoegd

*Random intercept for Country and ID*

``` r
model_NAD3 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  control = list(maxIter = 100, opt = "optim"))

summary(model_NAD3)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133177.5 133258.4 -66579.75

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2151183

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7944236 0.6096741

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2303140 0.04123264 48529 54.09098  0.0000
    StringencyIndex_dev             0.0028740 0.00145493 48529  1.97536  0.0482
    Str_dummy1                      0.0030874 0.01621667 48529  0.19038  0.8490
    Str_dummy2                      0.0154646 0.01301248 48529  1.18844  0.2347
    StringencyIndex_dev:Str_dummy1  0.0016398 0.00191349 48529  0.85696  0.3915
    StringencyIndex_dev:Str_dummy2 -0.0020290 0.00152222 48529 -1.33290  0.1826
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.074                             
    Str_dummy1                     -0.186 -0.281                      
    Str_dummy2                     -0.255 -0.229  0.574               
    StringencyIndex_dev:Str_dummy1 -0.068 -0.660 -0.265  0.261        
    StringencyIndex_dev:Str_dummy2 -0.072 -0.968  0.278  0.259  0.623 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4200169 -0.5513080 -0.1170756  0.5203456  5.3883794 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country*

``` r
model_NAD4 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = ~1), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD4)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      133144.2 133243.1 -66561.11

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.214802823 (Intr)
    StringencyIndex_dev 0.005321202 -0.031

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7946569 0.6091198

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2286800 0.04131885 48529 53.93857  0.0000
    StringencyIndex_dev             0.0026880 0.00202304 48529  1.32867  0.1840
    Str_dummy1                      0.0100849 0.02036826 48529  0.49513  0.6205
    Str_dummy2                      0.0173755 0.01364549 48529  1.27335  0.2029
    StringencyIndex_dev:Str_dummy1  0.0001801 0.00242717 48529  0.07420  0.9409
    StringencyIndex_dev:Str_dummy2 -0.0026754 0.00192379 48529 -1.39069  0.1643
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.045                             
    Str_dummy1                     -0.182 -0.277                      
    Str_dummy2                     -0.261 -0.151  0.454               
    StringencyIndex_dev:Str_dummy1 -0.032 -0.458 -0.431  0.202        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.791  0.144  0.213  0.666 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4673387 -0.5508797 -0.1172757  0.5211750  5.4582650 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for ID*

``` r
model_NAD5 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD5)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC    BIC    logLik
      132766.2 132865 -66372.08

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2168108

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.79512798 (Intr)
    StringencyIndex_dev 0.01408366 -0.001
    Residual            0.59609109       

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2331384 0.04147978 48529 53.83680  0.0000
    StringencyIndex_dev             0.0041627 0.00159499 48529  2.60987  0.0091
    Str_dummy1                     -0.0033596 0.01657438 48529 -0.20270  0.8394
    Str_dummy2                      0.0152127 0.01299889 48529  1.17031  0.2419
    StringencyIndex_dev:Str_dummy1  0.0007015 0.00201925 48529  0.34742  0.7283
    StringencyIndex_dev:Str_dummy2 -0.0029261 0.00168342 48529 -1.73817  0.0822
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.069                             
    Str_dummy1                     -0.179 -0.293                      
    Str_dummy2                     -0.250 -0.217  0.541               
    StringencyIndex_dev:Str_dummy1 -0.064 -0.659 -0.269  0.264        
    StringencyIndex_dev:Str_dummy2 -0.068 -0.950  0.257  0.260  0.653 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.3941180 -0.5432540 -0.1140702  0.5115800  5.3800625 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country and ID*

``` r
model_NAD6 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~StringencyIndex_dev | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD6)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132750.7 132867.5 -66362.35

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.215515858 (Intr)
    StringencyIndex_dev 0.004877827 0.007 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.79526245 (Intr)
    StringencyIndex_dev 0.01384156 0.001 
    Residual            0.59600321       

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2325083 0.04138782 48529 53.94119  0.0000
    StringencyIndex_dev             0.0032796 0.00203970 48529  1.60788  0.1079
    Str_dummy1                      0.0053023 0.02011588 48529  0.26359  0.7921
    Str_dummy2                      0.0138905 0.01346483 48529  1.03161  0.3023
    StringencyIndex_dev:Str_dummy1 -0.0002508 0.00240852 48529 -0.10414  0.9171
    StringencyIndex_dev:Str_dummy2 -0.0031004 0.00195012 48529 -1.58986  0.1119
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.061                             
    Str_dummy1                     -0.178 -0.298                      
    Str_dummy2                     -0.257 -0.149  0.449               
    StringencyIndex_dev:Str_dummy1 -0.034 -0.470 -0.413  0.207        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.809  0.166  0.210  0.665 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4406406 -0.5430133 -0.1152268  0.5132693  5.3742702 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country*

``` r
model_NAD7 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list(Country = pdDiag(~StringencyIndex_dev), ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD7)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132748.7 132856.5 -66362.35

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:    0.215615          0.00488407

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.79526161 (Intr)
    StringencyIndex_dev 0.01384133 0.001 
    Residual            0.59600313       

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2325197 0.04140416 48529 53.92017  0.0000
    StringencyIndex_dev             0.0032836 0.00204045 48529  1.60925  0.1076
    Str_dummy1                      0.0053245 0.02011616 48529  0.26469  0.7913
    Str_dummy2                      0.0139098 0.01346538 48529  1.03300  0.3016
    StringencyIndex_dev:Str_dummy1 -0.0002614 0.00240871 48529 -0.10854  0.9136
    StringencyIndex_dev:Str_dummy2 -0.0031114 0.00195051 48529 -1.59517  0.1107
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.058                             
    Str_dummy1                     -0.178 -0.298                      
    Str_dummy2                     -0.257 -0.149  0.449               
    StringencyIndex_dev:Str_dummy1 -0.034 -0.470 -0.413  0.207        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.809  0.166  0.210  0.665 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4407678 -0.5429988 -0.1151268  0.5132537  5.3743875 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for ID*

``` r
model_NAD8 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD8)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132748.7 132856.5 -66362.35

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.215494647 (Intr)
    StringencyIndex_dev 0.004876488 0.008 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7952704          0.01384192 0.5960031

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2325040 0.04138435 48529 53.94561  0.0000
    StringencyIndex_dev             0.0032794 0.00203956 48529  1.60788  0.1079
    Str_dummy1                      0.0052966 0.02011563 48529  0.26331  0.7923
    Str_dummy2                      0.0138906 0.01346474 48529  1.03163  0.3023
    StringencyIndex_dev:Str_dummy1 -0.0002491 0.00240846 48529 -0.10342  0.9176
    StringencyIndex_dev:Str_dummy2 -0.0030996 0.00195006 48529 -1.58949  0.1120
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.061                             
    Str_dummy1                     -0.178 -0.298                      
    Str_dummy2                     -0.257 -0.149  0.449               
    StringencyIndex_dev:Str_dummy1 -0.034 -0.470 -0.413  0.207        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.809  0.166  0.210  0.665 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4408736 -0.5430120 -0.1150621  0.5132107  5.3742495 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country and ID*

``` r
model_NAD9 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_NAD9)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      132746.7 132845.5 -66362.35

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2156115         0.004883683

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev Residual
    StdDev:   0.7952706          0.01384156 0.596003

    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2325173 0.04140361 48529 53.92083  0.0000
    StringencyIndex_dev             0.0032840 0.00204043 48529  1.60948  0.1075
    Str_dummy1                      0.0053223 0.02011593 48529  0.26458  0.7913
    Str_dummy2                      0.0139132 0.01346537 48529  1.03326  0.3015
    StringencyIndex_dev:Str_dummy1 -0.0002614 0.00240867 48529 -0.10854  0.9136
    StringencyIndex_dev:Str_dummy2 -0.0031124 0.00195050 48529 -1.59568  0.1106
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.058                             
    Str_dummy1                     -0.178 -0.298                      
    Str_dummy2                     -0.257 -0.149  0.449               
    StringencyIndex_dev:Str_dummy1 -0.034 -0.470 -0.413  0.207        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.809  0.166  0.210  0.665 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -5.4410282 -0.5430245 -0.1150610  0.5132141  5.3743876 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Autoregressive correlation structure*

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_NAD10 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_NAD10)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC    BIC    logLik
      131367.2 131475 -65671.58

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2130174         0.004342124

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7746187         0.008821821 0.6290455

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2876511 
    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2383326 0.04117351 48529 54.36342  0.0000
    StringencyIndex_dev             0.0025707 0.00201234 48529  1.27744  0.2015
    Str_dummy1                     -0.0027728 0.02061231 48529 -0.13452  0.8930
    Str_dummy2                      0.0085440 0.01456378 48529  0.58666  0.5574
    StringencyIndex_dev:Str_dummy1  0.0008463 0.00247895 48529  0.34140  0.7328
    StringencyIndex_dev:Str_dummy2 -0.0023064 0.00199392 48529 -1.15671  0.2474
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.063                             
    Str_dummy1                     -0.189 -0.295                      
    Str_dummy2                     -0.274 -0.154  0.458               
    StringencyIndex_dev:Str_dummy1 -0.044 -0.493 -0.397  0.220        
    StringencyIndex_dev:Str_dummy2 -0.060 -0.832  0.175  0.215  0.659 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9668082 -0.5533047 -0.1422778  0.5037572  4.9666244 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*AR without random slope for country*

``` r
model_NAD11 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_NAD11)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      131379.8 131478.7 -65678.91

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2143447

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:    0.774371         0.009016562 0.6293623

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2884034 
    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2371580 0.04125810 48529 54.22348  0.0000
    StringencyIndex_dev             0.0031733 0.00159488 48529  1.98966  0.0466
    Str_dummy1                     -0.0068187 0.01726876 48529 -0.39486  0.6929
    Str_dummy2                      0.0108166 0.01401811 48529  0.77161  0.4403
    StringencyIndex_dev:Str_dummy1  0.0016450 0.00206774 48529  0.79555  0.4263
    StringencyIndex_dev:Str_dummy2 -0.0021273 0.00168486 48529 -1.26258  0.2067
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.077                             
    Str_dummy1                     -0.191 -0.297                      
    Str_dummy2                     -0.267 -0.222  0.546               
    StringencyIndex_dev:Str_dummy1 -0.075 -0.651 -0.262  0.273        
    StringencyIndex_dev:Str_dummy2 -0.075 -0.955  0.277  0.261  0.629 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9276890 -0.5519021 -0.1442885  0.5050771  4.9678712 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

> Model NAD10 has the best fit (lowest BIC). This model has random
> slopes fo Stringency at the Country and ID level, assumes no
> correlation between random slope and intercept, and assumes
> autoregressive correlation structure at the measurement level.

*QQ plot of residuals*

``` r
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
hist(resid(model_NAD10, type = "normalized"),
freq = FALSE, xlim = lims, ylim =  c(0,.7),main = "Histogram of Standardized Residuals")
lines(density(scale(resid(model_NAD10))))
qqnorm(resid(model_NAD10, type = "normalized"),
xlim = lims, ylim = lims,main = "QQ plot")
abline(0,1, col = "red", lty = 2)
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

*Residuals vs fitted*

``` r
plot(fitted(model_NAD10, level=2), residuals(model_NAD10, level=2), 
     main="residuals vs fitted at ID level")
abline(a=0, b=0,col="red")
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(fitted(model_NAD10, level=1), residuals(model_NAD10, level=1), 
    main="residuals vs fitted at Country level")
abline(a=0, b=0,col="red")
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

*Plot random intercepts and slopes*

``` r
plot(ranef(model_NAD10, level = 1))
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(ranef(model_NAD10, level = 2))
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

*Confidence intervals*

``` r
intervals(model_NAD10)
```

    Approximate 95% confidence intervals

     Fixed effects:
                                          lower          est.       upper
    (Intercept)                     2.157632037  2.2383326406 2.319033244
    StringencyIndex_dev            -0.001373570  0.0025706514 0.006514873
    Str_dummy1                     -0.043173214 -0.0027728217 0.037627571
    Str_dummy2                     -0.020001200  0.0085440030 0.037089207
    StringencyIndex_dev:Str_dummy1 -0.004012451  0.0008463179 0.005705087
    StringencyIndex_dev:Str_dummy2 -0.006214471 -0.0023063718 0.001601728

     Random Effects:
      Level: Country 
                                  lower        est.       upper
    sd((Intercept))         0.152912291 0.213017442 0.296748091
    sd(StringencyIndex_dev) 0.002594597 0.004342124 0.007266653
      Level: ID 
                                  lower        est.      upper
    sd((Intercept))         0.762407899 0.774618684 0.78702504
    sd(StringencyIndex_dev) 0.007635859 0.008821821 0.01019198

     Correlation structure:
             lower      est.     upper
    Phi1 0.2735086 0.2876511 0.3016693

     Within-group standard error:
        lower      est.     upper 
    0.6239032 0.6290455 0.6342302 

*Plot of predicted values*

``` r
ef_NAD <- effect("StringencyIndex_dev:Str_dummy", model_NAD10)

plot_NAD <- ggplot(as.data.frame(ef_NAD), 
       aes(StringencyIndex_dev, fit, color=Str_dummy)) + geom_line() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + theme_bw(base_size=12) + scale_color_discrete(name="Maximum stringency", labels = c("Before the peak", "During the peak", "After the peak")) + expand_limits(y=c(1, 5))
```

``` r
plot_NAD
```

![](First-analyses-NAD-including-all-gender-categories_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
coef_NAD <- broom.mixed::tidy(model_NAD10, 
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
coef_NAD <- coef_NAD %>%
 left_join(., ISDs_av, by=c("term"="ind"))

coef_NAD <- coef_NAD %>%
  mutate(sd = ifelse(is.na(sd), 1, sd))

coef_NAD <- coef_NAD %>%
  mutate(sd = ifelse(row_number()== 5 | row_number()== 6, 6.12091, sd))

coef_NAD <- coef_NAD %>%
 mutate(e_size = (estimate * sd)/0.5319464)

coef_NAD <- coef_NAD %>%
  rename(isd = sd)
```

``` r
coef_NAD
```

    ## # A tibble: 6 × 9
    ##   effect term                            estimate std.error    df statistic p.value   isd   e_size
    ##   <chr>  <chr>                              <dbl>     <dbl> <dbl>     <dbl>   <dbl> <dbl>    <dbl>
    ## 1 fixed  (Intercept)                     2.24       0.0412  48529    54.4     0      1     4.21   
    ## 2 fixed  StringencyIndex_dev             0.00257    0.00201 48529     1.28    0.201  6.13  0.0296 
    ## 3 fixed  Str_dummy1                     -0.00277    0.0206  48529    -0.135   0.893  1    -0.00521
    ## 4 fixed  Str_dummy2                      0.00854    0.0146  48529     0.587   0.557  1     0.0161 
    ## 5 fixed  StringencyIndex_dev:Str_dummy1  0.000846   0.00248 48529     0.341   0.733  6.12  0.00974
    ## 6 fixed  StringencyIndex_dev:Str_dummy2 -0.00231    0.00199 48529    -1.16    0.247  6.12 -0.0265
