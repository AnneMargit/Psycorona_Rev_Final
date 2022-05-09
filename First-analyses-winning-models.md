First analyses winning models
================
Anne Margit
05/09/2022

    ## [1] ""

``` r
load("data_analyse1_fc.Rdata")
```

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
library(purrr)
library(stargazer)
```

**Negative affect high arousal**

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_NAA10 <- lme(fixed = NAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_NAA10)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse1_fc 
         AIC      BIC   logLik
      122849 122956.8 -61412.5

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2191325         0.001909749

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7321647         0.006763433 0.5948761

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.3395076 
    Fixed effects:  NAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF   t-value p-value
    (Intercept)                     2.4697659 0.04170321 48523  59.22244  0.0000
    StringencyIndex_dev            -0.0038154 0.00162309 48523  -2.35072  0.0187
    Str_dummy1                     -0.2081843 0.01764989 48523 -11.79522  0.0000
    Str_dummy2                     -0.2135786 0.01355169 48523 -15.76029  0.0000
    StringencyIndex_dev:Str_dummy1  0.0127375 0.00210471 48523   6.05187  0.0000
    StringencyIndex_dev:Str_dummy2  0.0071582 0.00169937 48523   4.21224  0.0000
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.067                             
    Str_dummy1                     -0.175 -0.309                      
    Str_dummy2                     -0.252 -0.189  0.495               
    StringencyIndex_dev:Str_dummy1 -0.059 -0.567 -0.332  0.257        
    StringencyIndex_dev:Str_dummy2 -0.065 -0.905  0.231  0.248  0.636 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9169579 -0.5659618 -0.1337071  0.5002523  4.9967859 

    Number of Observations: 59037
    Number of Groups: 
            Country ID %in% Country 
                 33           10509 

``` r
VarCorr(model_NAA10)
```

                        Variance                    StdDev     
    Country =           pdDiag(StringencyIndex_dev)            
    (Intercept)         0.048019046602              0.219132486
    StringencyIndex_dev 0.000003647139              0.001909749
    ID =                pdDiag(StringencyIndex_dev)            
    (Intercept)         0.536065159316              0.732164708
    StringencyIndex_dev 0.000045744032              0.006763433
    Residual            0.353877599239              0.594876121

> Random ICs and slopes for country and ID, no correlation between ICS
> and slopes, AR structure

**Negative affect low arousal**

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
           AIC      BIC    logLik
      131352.6 131460.5 -65664.32

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2130175         0.004342649

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7746693         0.008823744 0.6290218

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2875478 
    Fixed effects:  NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2383310 0.04117360 48523 54.36326  0.0000
    StringencyIndex_dev             0.0025710 0.00201239 48523  1.27758  0.2014
    Str_dummy1                     -0.0027654 0.02061235 48523 -0.13416  0.8933
    Str_dummy2                      0.0085472 0.01456330 48523  0.58690  0.5573
    StringencyIndex_dev:Str_dummy1  0.0008454 0.00247895 48523  0.34105  0.7331
    StringencyIndex_dev:Str_dummy2 -0.0023069 0.00199391 48523 -1.15698  0.2473
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.063                             
    Str_dummy1                     -0.189 -0.295                      
    Str_dummy2                     -0.274 -0.154  0.458               
    StringencyIndex_dev:Str_dummy1 -0.044 -0.493 -0.397  0.220        
    StringencyIndex_dev:Str_dummy2 -0.060 -0.832  0.175  0.215  0.659 

    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9671405 -0.5532257 -0.1422207  0.5037598  4.9668796 

    Number of Observations: 59037
    Number of Groups: 
            Country ID %in% Country 
                 33           10509 

``` r
VarCorr(model_NAD10)
```

                        Variance                    StdDev     
    Country =           pdDiag(StringencyIndex_dev)            
    (Intercept)         0.04537646928               0.213017533
    StringencyIndex_dev 0.00001885860               0.004342649
    ID =                pdDiag(StringencyIndex_dev)            
    (Intercept)         0.60011250589               0.774669288
    StringencyIndex_dev 0.00007785845               0.008823744
    Residual            0.39566838127               0.629021765

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

**Positive affect high arousal**

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
           AIC      BIC    logLik
      129336.2 129444.1 -64656.12

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2347207         0.004565836

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev Residual
    StdDev:   0.6935049         0.007928129  0.62678

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2740265 
    Fixed effects:  PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4527878 0.04432060 48523 55.34193       0
    StringencyIndex_dev             0.0083082 0.00202150 48523  4.10994       0
    Str_dummy1                      0.1447147 0.02067688 48523  6.99887       0
    Str_dummy2                      0.2195653 0.01447680 48523 15.16670       0
    StringencyIndex_dev:Str_dummy1 -0.0148308 0.00248214 48523 -5.97501       0
    StringencyIndex_dev:Str_dummy2 -0.0137693 0.00198951 48523 -6.92095       0
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.057                             
    Str_dummy1                     -0.174 -0.292                      
    Str_dummy2                     -0.254 -0.152  0.456               
    StringencyIndex_dev:Str_dummy1 -0.039 -0.484 -0.406  0.216        
    StringencyIndex_dev:Str_dummy2 -0.054 -0.823  0.168  0.213  0.659 

    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.114214248 -0.571156245  0.002021254  0.578596264  4.777130132 

    Number of Observations: 59037
    Number of Groups: 
            Country ID %in% Country 
                 33           10509 

``` r
VarCorr(model_PAA10)
```

                        Variance                    StdDev     
    Country =           pdDiag(StringencyIndex_dev)            
    (Intercept)         0.05509380782               0.234720702
    StringencyIndex_dev 0.00002084686               0.004565836
    ID =                pdDiag(StringencyIndex_dev)            
    (Intercept)         0.48094905010               0.693504903
    StringencyIndex_dev 0.00006285523               0.007928129
    Residual            0.39285314665               0.626779983

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

**Positive affect low arousal**

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
      132851.5 132959.3 -66413.74

    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2043951         0.002323949

     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev Residual
    StdDev:   0.7244669         0.008942382 0.640881

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2580925 
    Fixed effects:  PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8271233 0.03960319 48523 71.38625  0.0000
    StringencyIndex_dev             0.0048776 0.00180516 48523  2.70203  0.0069
    Str_dummy1                      0.1649837 0.01937035 48523  8.51733  0.0000
    Str_dummy2                      0.1631073 0.01458961 48523 11.17969  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0105726 0.00231697 48523 -4.56311  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0052545 0.00187981 48523 -2.79522  0.0052
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.075                             
    Str_dummy1                     -0.199 -0.308                      
    Str_dummy2                     -0.287 -0.185  0.489               
    StringencyIndex_dev:Str_dummy1 -0.063 -0.562 -0.340  0.251        
    StringencyIndex_dev:Str_dummy2 -0.073 -0.897  0.222  0.245  0.643 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.16803914 -0.55322135  0.06315432  0.58396858  4.25570527 

    Number of Observations: 59037
    Number of Groups: 
            Country ID %in% Country 
                 33           10509 

``` r
VarCorr(model_PAD10)
```

                        Variance                    StdDev     
    Country =           pdDiag(StringencyIndex_dev)            
    (Intercept)         0.04177736834               0.204395128
    StringencyIndex_dev 0.00000540074               0.002323949
    ID =                pdDiag(StringencyIndex_dev)            
    (Intercept)         0.52485227504               0.724466890
    StringencyIndex_dev 0.00007996620               0.008942382
    Residual            0.41072840397               0.640880959

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

``` r
stargazer(model_NAA10, model_NAD10, model_PAA10, model_PAD10,
type="html",
out="firstanalyses.doc",  single.row=TRUE, digits = 2, align = TRUE, 
order = c("Intercept","SI", "Max SI (during)", "Max SI (after)", "SI x Max SI (during)", "SI x Max SI (after)"), 
covariate.labels = c("Intercept","SI", "Max SI (during)", "Max SI (after)", 
"SI x Max SI (during)", "SI x Max SI (after)"))
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td>NAA</td><td>NAD</td><td>PAA</td><td>PAD</td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Intercept</td><td>-0.004<sup>**</sup> (0.002)</td><td>0.003 (0.002)</td><td>0.01<sup>***</sup> (0.002)</td><td>0.005<sup>***</sup> (0.002)</td></tr>
    ## <tr><td style="text-align:left">SI</td><td>-0.21<sup>***</sup> (0.02)</td><td>-0.003 (0.02)</td><td>0.14<sup>***</sup> (0.02)</td><td>0.16<sup>***</sup> (0.02)</td></tr>
    ## <tr><td style="text-align:left">Max SI (during)</td><td>-0.21<sup>***</sup> (0.01)</td><td>0.01 (0.01)</td><td>0.22<sup>***</sup> (0.01)</td><td>0.16<sup>***</sup> (0.01)</td></tr>
    ## <tr><td style="text-align:left">Max SI (after)</td><td>0.01<sup>***</sup> (0.002)</td><td>0.001 (0.002)</td><td>-0.01<sup>***</sup> (0.002)</td><td>-0.01<sup>***</sup> (0.002)</td></tr>
    ## <tr><td style="text-align:left">SI x Max SI (during)</td><td>0.01<sup>***</sup> (0.002)</td><td>-0.002 (0.002)</td><td>-0.01<sup>***</sup> (0.002)</td><td>-0.01<sup>***</sup> (0.002)</td></tr>
    ## <tr><td style="text-align:left">SI x Max SI (after)</td><td>2.47<sup>***</sup> (0.04)</td><td>2.24<sup>***</sup> (0.04)</td><td>2.45<sup>***</sup> (0.04)</td><td>2.83<sup>***</sup> (0.04)</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>59,037</td><td>59,037</td><td>59,037</td><td>59,037</td></tr>
    ## <tr><td style="text-align:left">Log Likelihood</td><td>-61,412.50</td><td>-65,664.32</td><td>-64,656.12</td><td>-66,413.74</td></tr>
    ## <tr><td style="text-align:left">Akaike Inf. Crit.</td><td>122,849.00</td><td>131,352.60</td><td>129,336.20</td><td>132,851.50</td></tr>
    ## <tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>122,956.80</td><td>131,460.50</td><td>129,444.10</td><td>132,959.30</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    ## </table>
