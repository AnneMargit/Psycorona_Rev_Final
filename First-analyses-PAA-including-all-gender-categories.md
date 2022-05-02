First analyses PAA including all gender categories
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

**Positive affect high arousal**

*Stringency Index x dummy interaction: random intercept for Country*

``` r
model_PAA1 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = ~1 | Country, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA1)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      160620.4 160692.3 -80302.22

    Random effects:
     Formula: ~1 | Country
            (Intercept)  Residual
    StdDev:   0.2384716 0.9413389

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4712597 0.04494845 59006 54.97986       0
    StringencyIndex_dev             0.0100853 0.00206242 59006  4.89004       0
    Str_dummy1                      0.1092990 0.02307567 59006  4.73655       0
    Str_dummy2                      0.2119435 0.01834621 59006 11.55244       0
    StringencyIndex_dev:Str_dummy1 -0.0129595 0.00272244 59006 -4.76025       0
    StringencyIndex_dev:Str_dummy2 -0.0140247 0.00216965 59006 -6.46402       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.092                             
    Str_dummy1                     -0.241 -0.270                      
    Str_dummy2                     -0.335 -0.224  0.574               
    StringencyIndex_dev:Str_dummy1 -0.086 -0.659 -0.273  0.254        
    StringencyIndex_dev:Str_dummy2 -0.089 -0.967  0.271  0.256  0.616 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.34448210 -0.82904378  0.02769964  0.68314941  3.23530229 

    Number of Observations: 59044
    Number of Groups: 33 

*Stringency Index x dummy interaction: random intercept for ID*

``` r
model_PAA2 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | ID, 
                 data = data_analyse1_fc, 
                 na.action = na.omit)

summary(model_PAA2)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      131845.4 131917.3 -65914.71

    Random effects:
     Formula: ~1 | ID
            (Intercept) Residual
    StdDev:   0.7539343 0.607986

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value   Std.Error    DF   t-value p-value
    (Intercept)                     2.4075474 0.013585768 48529 177.21099  0.0000
    StringencyIndex_dev             0.0094559 0.001430101 48529   6.61208  0.0000
    Str_dummy1                      0.0656571 0.014795678 48529   4.43759  0.0000
    Str_dummy2                      0.2310942 0.012721733 48529  18.16531  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0044525 0.001790576 48529  -2.48665  0.0129
    StringencyIndex_dev:Str_dummy2 -0.0137878 0.001492992 48529  -9.23503  0.0000
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.232                             
    Str_dummy1                     -0.688 -0.264                      
    Str_dummy2                     -0.758 -0.238  0.654               
    StringencyIndex_dev:Str_dummy1 -0.152 -0.735 -0.166  0.246        
    StringencyIndex_dev:Str_dummy2 -0.222 -0.967  0.259  0.269  0.699 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.205031315 -0.577817126 -0.004891665  0.581544453  5.219078809 

    Number of Observations: 59044
    Number of Groups: 10510 

*Random intercept for Country and ID*

``` r
model_PAA3 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~1 | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA3)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130951.7 131032.5 -65466.84

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2337885

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7160322 0.6077355

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4625604 0.04391718 48529 56.07282       0
    StringencyIndex_dev             0.0087391 0.00144676 48529  6.04050       0
    Str_dummy1                      0.1238077 0.01613211 48529  7.67462       0
    Str_dummy2                      0.2142444 0.01294033 48529 16.55634       0
    StringencyIndex_dev:Str_dummy1 -0.0124767 0.00190320 48529 -6.55565       0
    StringencyIndex_dev:Str_dummy2 -0.0126557 0.00151396 48529 -8.35936       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.069                             
    Str_dummy1                     -0.173 -0.281                      
    Str_dummy2                     -0.238 -0.229  0.573               
    StringencyIndex_dev:Str_dummy1 -0.064 -0.660 -0.265  0.261        
    StringencyIndex_dev:Str_dummy2 -0.067 -0.968  0.278  0.259  0.622 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.248862116 -0.578689435 -0.007348997  0.580198801  5.173264654 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country*

``` r
model_PAA4 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = ~1), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA4)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130916.3 131015.1 -65447.13

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.234683521 (Intr)
    StringencyIndex_dev 0.006216821 0.112 

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.7158403 0.6071932

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4511766 0.04419987 48529 55.45665       0
    StringencyIndex_dev             0.0092168 0.00211672 48529  4.35429       0
    Str_dummy1                      0.1465679 0.02086496 48529  7.02459       0
    Str_dummy2                      0.2272302 0.01361498 48529 16.68972       0
    StringencyIndex_dev:Str_dummy1 -0.0164203 0.00247399 48529 -6.63720       0
    StringencyIndex_dev:Str_dummy2 -0.0151535 0.00194056 48529 -7.80886       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.106                             
    Str_dummy1                     -0.169 -0.267                      
    Str_dummy2                     -0.244 -0.143  0.447               
    StringencyIndex_dev:Str_dummy1 -0.025 -0.428 -0.453  0.190        
    StringencyIndex_dev:Str_dummy2 -0.048 -0.758  0.128  0.206  0.666 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.255981188 -0.580083485 -0.007504934  0.579764285  5.187663376 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for ID*

``` r
model_PAA5 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA5)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC    BIC    logLik
      130577.2 130676 -65277.59

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2348597

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.71687806 (Intr)
    StringencyIndex_dev 0.01351992 -0.053
    Residual            0.59512938       

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4606081 0.04406891 48529 55.83547       0
    StringencyIndex_dev             0.0089946 0.00158051 48529  5.69094       0
    Str_dummy1                      0.1330633 0.01648014 48529  8.07416       0
    Str_dummy2                      0.2156716 0.01293558 48529 16.67274       0
    StringencyIndex_dev:Str_dummy1 -0.0138052 0.00200413 48529 -6.88838       0
    StringencyIndex_dev:Str_dummy2 -0.0133922 0.00166863 48529 -8.02585       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.063                             
    Str_dummy1                     -0.167 -0.292                      
    Str_dummy2                     -0.234 -0.217  0.542               
    StringencyIndex_dev:Str_dummy1 -0.060 -0.659 -0.270  0.263        
    StringencyIndex_dev:Str_dummy2 -0.063 -0.951  0.257  0.260  0.652 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.365868956 -0.567890318 -0.009527372  0.570498017  5.256841947 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Random slope for Country and ID*

``` r
model_PAA6 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = ~StringencyIndex_dev | Country/ID, 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA6)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130558.4 130675.2 -65266.21

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.235226210 (Intr)
    StringencyIndex_dev 0.005804566 0.037 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.71665533 (Intr)
    StringencyIndex_dev 0.01333648 -0.049
    Residual            0.59494830       

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4507744 0.04425601 48529 55.37721       0
    StringencyIndex_dev             0.0094778 0.00213189 48529  4.44572       0
    Str_dummy1                      0.1450345 0.02064523 48529  7.02509       0
    Str_dummy2                      0.2290249 0.01345621 48529 17.02001       0
    StringencyIndex_dev:Str_dummy1 -0.0162912 0.00245553 48529 -6.63449       0
    StringencyIndex_dev:Str_dummy2 -0.0152229 0.00196970 48529 -7.72855       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.067                             
    Str_dummy1                     -0.166 -0.288                      
    Str_dummy2                     -0.240 -0.140  0.443               
    StringencyIndex_dev:Str_dummy1 -0.026 -0.439 -0.436  0.193        
    StringencyIndex_dev:Str_dummy2 -0.048 -0.776  0.149  0.201  0.665 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.371329922 -0.567870536 -0.009520735  0.570595697  5.270501516 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country*

``` r
model_PAA7 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list(Country = pdDiag(~StringencyIndex_dev), ID = ~StringencyIndex_dev), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA7)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130556.5 130664.3 -65266.25

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2353675         0.005748984

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev     Corr  
    (Intercept)         0.71665241 (Intr)
    StringencyIndex_dev 0.01333851 -0.049
    Residual            0.59494893       

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4510761 0.04427938 48529 55.35480       0
    StringencyIndex_dev             0.0095194 0.00212607 48529  4.47745       0
    Str_dummy1                      0.1443820 0.02059403 48529  7.01086       0
    Str_dummy2                      0.2288893 0.01345438 48529 17.01225       0
    StringencyIndex_dev:Str_dummy1 -0.0162367 0.00245126 48529 -6.62382       0
    StringencyIndex_dev:Str_dummy2 -0.0152193 0.00196885 48529 -7.73006       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.051                             
    Str_dummy1                     -0.166 -0.289                      
    Str_dummy2                     -0.240 -0.140  0.444               
    StringencyIndex_dev:Str_dummy1 -0.027 -0.441 -0.434  0.194        
    StringencyIndex_dev:Str_dummy2 -0.048 -0.778  0.150  0.201  0.665 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.371314835 -0.567873321 -0.009456157  0.570594400  5.270567951 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for ID*

``` r
model_PAA8 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~StringencyIndex_dev, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA8)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130559.1 130666.9 -65267.56

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: General positive-definite, Log-Cholesky parametrization
                        StdDev      Corr  
    (Intercept)         0.235392086 (Intr)
    StringencyIndex_dev 0.005860707 0.071 

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7162763          0.01333462 0.5949223

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4503834 0.04428195 48529 55.33594       0
    StringencyIndex_dev             0.0094173 0.00213675 48529  4.40731       0
    Str_dummy1                      0.1459629 0.02068953 48529  7.05492       0
    Str_dummy2                      0.2294200 0.01345648 48529 17.04903       0
    StringencyIndex_dev:Str_dummy1 -0.0163384 0.00246038 48529 -6.64060       0
    StringencyIndex_dev:Str_dummy2 -0.0152102 0.00197002 48529 -7.72084       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.083                             
    Str_dummy1                     -0.166 -0.288                      
    Str_dummy2                     -0.240 -0.140  0.442               
    StringencyIndex_dev:Str_dummy1 -0.026 -0.437 -0.438  0.194        
    StringencyIndex_dev:Str_dummy2 -0.048 -0.774  0.149  0.202  0.665 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.372779190 -0.567957029 -0.009310933  0.570772154  5.270659311 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*No correlation between intercept and slope for Country and ID*

``` r
model_PAA9 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit)

summary(model_PAA9)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC    BIC   logLik
      130557.2 130656 -65267.6

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2353554         0.005769461

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:    0.716282          0.01333717 0.5949224

    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4509392 0.04427720 48529 55.35443       0
    StringencyIndex_dev             0.0094950 0.00212749 48529  4.46300       0
    Str_dummy1                      0.1447311 0.02060467 48529  7.02419       0
    Str_dummy2                      0.2291859 0.01345410 48529 17.03466       0
    StringencyIndex_dev:Str_dummy1 -0.0162385 0.00245348 48529 -6.61859       0
    StringencyIndex_dev:Str_dummy2 -0.0152076 0.00196906 48529 -7.72330       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.052                             
    Str_dummy1                     -0.166 -0.288                      
    Str_dummy2                     -0.240 -0.141  0.443               
    StringencyIndex_dev:Str_dummy1 -0.027 -0.441 -0.435  0.194        
    StringencyIndex_dev:Str_dummy2 -0.048 -0.777  0.150  0.202  0.665 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.372770872 -0.567959918 -0.009154033  0.570691631  5.270819430 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*Autoregressive correlation structure*

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_PAA10 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAA10)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC   logLik
      129349.8 129457.6 -64662.9

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:    0.234699         0.004566278

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.6936078         0.007930761 0.6267445

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2739828 
    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4527981 0.04431729 48529 55.34630       0
    StringencyIndex_dev             0.0083087 0.00202151 48529  4.11014       0
    Str_dummy1                      0.1447460 0.02067638 48529  7.00055       0
    Str_dummy2                      0.2195725 0.01447612 48529 15.16791       0
    StringencyIndex_dev:Str_dummy1 -0.0148339 0.00248208 48529 -5.97639       0
    StringencyIndex_dev:Str_dummy2 -0.0137700 0.00198947 48529 -6.92145       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.057                             
    Str_dummy1                     -0.174 -0.292                      
    Str_dummy2                     -0.254 -0.152  0.456               
    StringencyIndex_dev:Str_dummy1 -0.039 -0.484 -0.406  0.216        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.823  0.168  0.213  0.659 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.114634968 -0.571117264  0.001935938  0.578690454  4.777477795 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*AR without random slope for country*

``` r
model_PAA11 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (Country = ~1, ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAA11)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      129363.3 129462.1 -64670.65

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2338371

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.6936431         0.008059687 0.6271498

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2750402 
    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4622445 0.04405585 48529 55.88916       0
    StringencyIndex_dev             0.0079885 0.00157199 48529  5.08176       0
    Str_dummy1                      0.1253903 0.01713514 48529  7.31773       0
    Str_dummy2                      0.2085666 0.01390742 48529 14.99679       0
    StringencyIndex_dev:Str_dummy1 -0.0120494 0.00204537 48529 -5.89105       0
    StringencyIndex_dev:Str_dummy2 -0.0122820 0.00166024 48529 -7.39772       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.072                             
    Str_dummy1                     -0.178 -0.295                      
    Str_dummy2                     -0.249 -0.223  0.548               
    StringencyIndex_dev:Str_dummy1 -0.070 -0.651 -0.263  0.273        
    StringencyIndex_dev:Str_dummy2 -0.070 -0.957  0.278  0.261  0.626 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.108395106 -0.571539269  0.001623915  0.579701741  4.766632862 

    Number of Observations: 59044
    Number of Groups: 
            Country ID %in% Country 
                 33           10510 

*AR without random intercept and slope for country*

``` r
model_PAA12 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                  random = list (ID = pdDiag(~StringencyIndex_dev)), 
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | ID))

summary(model_PAA12)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
           AIC      BIC    logLik
      130263.6 130353.5 -65121.82

    Random effects:
     Formula: ~StringencyIndex_dev | ID
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7324246         0.007960403 0.6276288

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | ID 
     Parameter estimate(s):
         Phi1 
    0.2756274 
    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value   Std.Error    DF   t-value p-value
    (Intercept)                     2.4043705 0.014002532 48529 171.70970  0.0000
    StringencyIndex_dev             0.0088374 0.001544824 48529   5.72063  0.0000
    Str_dummy1                      0.0618128 0.015494063 48529   3.98945  0.0001
    Str_dummy2                      0.2293229 0.013604347 48529  16.85659  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0029801 0.001909361 48529  -1.56076  0.1186
    StringencyIndex_dev:Str_dummy2 -0.0134448 0.001628381 48529  -8.25656  0.0000
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.239                             
    Str_dummy1                     -0.697 -0.272                      
    Str_dummy2                     -0.766 -0.236  0.644               
    StringencyIndex_dev:Str_dummy1 -0.164 -0.739 -0.152  0.252        
    StringencyIndex_dev:Str_dummy2 -0.226 -0.956  0.256  0.274  0.711 

    Standardized Within-Group Residuals:
              Min            Q1           Med            Q3           Max 
    -5.0474266710 -0.5715741351  0.0008051551  0.5771002691  4.8128864057 

    Number of Observations: 59044
    Number of Groups: 10510 

> Model PAA10 has the best fit (lowest BIC). This model has random
> slopes fo Stringency at the Country and ID level, assumes no
> correlation between random slopes and intercepts, and assumes
> autoregressive correlation structure at the measurement level.

*QQ plot of residuals*

``` r
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
hist(resid(model_PAA10, type = "normalized"),
freq = FALSE, xlim = lims, ylim =  c(0,.7),main = "Histogram of Standardized Residuals")
lines(density(scale(resid(model_PAA10))))
qqnorm(resid(model_PAA10, type = "normalized"),
xlim = lims, ylim = lims,main = "QQ plot")
abline(0,1, col = "red", lty = 2)
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

*Residuals vs fitted*

``` r
plot(fitted(model_PAA10, level=2), residuals(model_PAA10, level=2), 
     main="residuals vs fitted at ID level")
abline(a=0, b=0,col="red")
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(fitted(model_PAA10, level=1), residuals(model_PAA10, level=1), 
    main="residuals vs fitted at Country level")
abline(a=0, b=0,col="red")
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

*Plot random intercepts and slopes*

``` r
plot(ranef(model_PAA10, level = 1))
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
plot(ranef(model_PAA10, level = 2))
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

*Confidence intervals*

``` r
intervals(model_PAA10)
```

    Approximate 95% confidence intervals

     Fixed effects:
                                         lower         est.        upper
    (Intercept)                     2.36593563  2.452798090  2.539660550
    StringencyIndex_dev             0.00434649  0.008308667  0.012270844
    Str_dummy1                      0.10422004  0.144746002  0.185271968
    Str_dummy2                      0.19119911  0.219572493  0.247945877
    StringencyIndex_dev:Str_dummy1 -0.01969878 -0.014833877 -0.009968970
    StringencyIndex_dev:Str_dummy2 -0.01766938 -0.013769997 -0.009870617

     Random Effects:
      Level: Country 
                                  lower        est.       upper
    sd((Intercept))         0.181142551 0.234698997 0.304089894
    sd(StringencyIndex_dev) 0.002403247 0.004566278 0.008676134
      Level: ID 
                                 lower        est.       upper
    sd((Intercept))         0.68251490 0.693607830 0.704881052
    sd(StringencyIndex_dev) 0.00672238 0.007930761 0.009356356

     Correlation structure:
             lower      est.     upper
    Phi1 0.2602453 0.2739828 0.2876094

     Within-group standard error:
        lower      est.     upper 
    0.6217507 0.6267445 0.6317784 

*Plot of predicted values*

``` r
ef_PAA <- effect("StringencyIndex_dev:Str_dummy", model_PAA10)

plot_PAA <- ggplot(as.data.frame(ef_PAA), 
       aes(StringencyIndex_dev, fit, color=Str_dummy)) + geom_line() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + theme_bw(base_size=12) + scale_color_discrete(name="Maximum stringency", labels = c("Before the peak", "During the peak", "After the peak")) + expand_limits(y=c(1, 5))
```

``` r
plot_PAA
```

![](First-analyses-PAA-including-all-gender-categories_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
coef_PAA = broom.mixed::tidy(model_PAA10, 
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
coef_PAA <- coef_PAA %>%
 left_join(., ISDs_av, by=c("term"="ind"))

coef_PAA <- coef_PAA %>%
  mutate(sd = ifelse(is.na(sd), 1, sd))

coef_PAA <- coef_PAA %>%
  mutate(sd = ifelse(row_number()== 5 | row_number()== 6, 6.12091, sd))

coef_PAA <- coef_PAA %>%
 mutate(e_size = (estimate * sd)/0.5469834)

coef_PAA <- coef_PAA %>%
  rename(isd = sd)
```

``` r
coef_PAA
```

    ## # A tibble: 6 × 9
    ##   effect term                           estimate std.error    df statistic  p.value   isd  e_size
    ##   <chr>  <chr>                             <dbl>     <dbl> <dbl>     <dbl>    <dbl> <dbl>   <dbl>
    ## 1 fixed  (Intercept)                     2.45      0.0443  48529     55.3  0         1     4.48  
    ## 2 fixed  StringencyIndex_dev             0.00831   0.00202 48529      4.11 3.96e- 5  6.13  0.0931
    ## 3 fixed  Str_dummy1                      0.145     0.0207  48529      7.00 2.58e-12  1     0.265 
    ## 4 fixed  Str_dummy2                      0.220     0.0145  48529     15.2  7.59e-52  1     0.401 
    ## 5 fixed  StringencyIndex_dev:Str_dummy1 -0.0148    0.00248 48529     -5.98 2.30e- 9  6.12 -0.166 
    ## 6 fixed  StringencyIndex_dev:Str_dummy2 -0.0138    0.00199 48529     -6.92 4.53e-12  6.12 -0.154
