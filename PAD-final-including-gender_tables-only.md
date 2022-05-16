PAD final including gender_tables only
================
Anne Margit
05/16/2022

    ## [1] ""

``` r
load("data_analyse2_p1.Rdata")
load("data_analyse2_p2.Rdata")
load("data_analyse2_p3.Rdata")

levels(data_analyse2_p1$Gender) <- c("M", "F", "O")
levels(data_analyse2_p2$Gender) <- c("M", "F", "O")
levels(data_analyse2_p3$Gender) <- c("M", "F", "O")
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
11. A dummy Str_dummy with 0 = before the peak, 1 = during peak, 2 =
    after peak
12. A variable indicating the number of days before maximum stringency
    was reached (DaysMax_p1), during (DaysMax_p2), and after
    (DaysPhase3)
13. A variable indicating the number of weeks before maximum stringency
    was reached (WeeksMax_p1), during (WeeksMax_p2), and after
    (WeeksPhase3)
14. A variable indicating the date on which maximum Stringency was
    reached for that country (DateMaxStr) and the max level reached
    (MaxStr) across the entire measurement period
15. A variable indicating the date on which minimum Stringency was
    reached for that country (DateMinStr) and the min level reached
    (MinStr) across the entire measurement period
16. Observations during which there was a second peak are excluded
    (N=583)

> My comments are in block quotes such as this.

``` r
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rockchalk)
library(effects)
library(nlme)
library(lattice)
library(broom.mixed)
library(purrr)
library(stargazer)
library(viridis)
```

# Phase 1

*Random: IC for ID and Country + Covariates Gender and Education*

> Edu: 0= Primary education, 1= General secondary education, 2=
> Vocational education, 3= Higher education, 4= Bachelors degree, 5=
> Masters degree, 6= PhD degree

``` r
data_analyse2_p1$Edu <- as.numeric(data_analyse2_p1$Edu)
model_PADp1 <- lme(fixed = PAD ~ Gender + Edu + Age_new,
                  random = ~1 | Country/ID, 
                  data = data_analyse2_p1, 
                  na.action = na.omit)

summary(model_PADp1)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse2_p1 
           AIC      BIC    logLik
      10977.41 11040.76 -5478.703

    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2801719

     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.6830884 0.6561505

    Fixed effects:  PAD ~ Gender + Edu + Age_new 
                     Value  Std.Error   DF   t-value p-value
    (Intercept)  2.9387366 0.10198030 2730 28.816710  0.0000
    GenderF     -0.2743735 0.03743682 2730 -7.328976  0.0000
    GenderO     -0.3847021 0.23201439 2730 -1.658096  0.0974
    Edu          0.0175868 0.01358553 2730  1.294525  0.1956
    Age_new1    -0.1579400 0.05191957 2730 -3.042013  0.0024
    Age_new2    -0.0052883 0.05523257 2730 -0.095747  0.9237
    Age_new3     0.2243155 0.07838023 2730  2.861889  0.0042
     Correlation: 
             (Intr) GendrF GendrO Edu    Ag_nw1 Ag_nw2
    GenderF  -0.277                                   
    GenderO  -0.078  0.119                            
    Edu      -0.550 -0.044  0.031                     
    Age_new1 -0.237  0.078  0.020 -0.246              
    Age_new2 -0.281  0.129  0.058 -0.162  0.712       
    Age_new3 -0.239  0.184  0.049 -0.098  0.492  0.497

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.90197572 -0.52520043  0.01295692  0.55265314  3.05429778 

    Number of Observations: 4177
    Number of Groups: 
            Country ID %in% Country 
                 26            2762 

``` r
VarCorr(model_PADp1)
```

                Variance     StdDev   
    Country =   pdLogChol(1)          
    (Intercept) 0.07849632   0.2801719
    ID =        pdLogChol(1)          
    (Intercept) 0.46660982   0.6830884
    Residual    0.43053351   0.6561505

*Confidence intervals*

``` r
intervals(model_PADp1)
```

    Approximate 95% confidence intervals

     Fixed effects:
                       lower        est.       upper
    (Intercept)  2.738770200  2.93873656  3.13870293
    GenderF     -0.347780913 -0.27437355 -0.20096618
    GenderO     -0.839643620 -0.38470208  0.07023947
    Edu         -0.009052152  0.01758680  0.04422575
    Age_new1    -0.259745623 -0.15794000 -0.05613439
    Age_new2    -0.113590209 -0.00528834  0.10301353
    Age_new3     0.070624930  0.22431549  0.37800606

     Random Effects:
      Level: Country 
                        lower      est.     upper
    sd((Intercept)) 0.1917558 0.2801719 0.4093556
      Level: ID 
                        lower      est.     upper
    sd((Intercept)) 0.6498095 0.6830884 0.7180717

     Within-group standard error:
        lower      est.     upper 
    0.6329526 0.6561505 0.6801986 

*Plot of predicted values*

``` r
ef_PADp1 <- effect("Age_new", model_PADp1)

plot_PADp1 <- ggplot(as.data.frame(ef_PADp1), 
  aes(Age_new, fit, color=Age_new)) + geom_line() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + theme_minimal(base_size=10) + 
  labs(title="PAD during tightening of restrictions", y = "PAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(name ="Age", labels=c("18-24", "25-44", "45-64", "65+")) +
  theme(legend.position = "none") +                 
  scale_color_discrete() + 
  expand_limits(y=c(1, 5))
```

``` r
plot_PADp1
```

![](PAD-final-including-gender_tables-only_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

*Effect sizes*

``` r
ISDs <- data_analyse2_p1 %>% 
  group_by(ID) %>%
  summarize_at(c("PAD"), sd, na.rm=TRUE) %>%
  ungroup()

ISDs_av <- ISDs %>%
  summarize_at(c("PAD"), mean, na.rm=TRUE) %>%
  stack() %>%
  rename(sd=values) 
```

> Effect size = regression coefficient / average ISD of PAD

``` r
coef_PADp1 = broom.mixed::tidy(model_PADp1, 
               effects = "fixed")

coef_PADp1 <- coef_PADp1 %>%
  mutate (e_size = estimate/0.507317) %>% 
  mutate(across(3:8, round, 2)) 
```

``` r
coef_PADp1
```

    ## # A tibble: 7 × 8
    ##   effect term        estimate std.error    df statistic p.value e_size
    ##   <chr>  <chr>          <dbl>     <dbl> <dbl>     <dbl>   <dbl>  <dbl>
    ## 1 fixed  (Intercept)     2.94      0.1   2730     28.8     0      5.79
    ## 2 fixed  GenderF        -0.27      0.04  2730     -7.33    0     -0.54
    ## 3 fixed  GenderO        -0.38      0.23  2730     -1.66    0.1   -0.76
    ## 4 fixed  Edu             0.02      0.01  2730      1.29    0.2    0.03
    ## 5 fixed  Age_new1       -0.16      0.05  2730     -3.04    0     -0.31
    ## 6 fixed  Age_new2       -0.01      0.06  2730     -0.1     0.92  -0.01
    ## 7 fixed  Age_new3        0.22      0.08  2730      2.86    0      0.44

``` r
coef_PADp1 <- as.matrix(coef_PADp1)
```

# Phase 2

*Best model*

> Random intercept for ID and Country, random slope for ID and country,
> no correlation between IC and S for ID and country + AR correlation
> structure at Measurement level

``` r
data_analyse2_p2$Edu <- as.numeric(data_analyse2_p2$Edu)
data_analyse2_p2 <- data_analyse2_p2[with(data_analyse2_p2, order(Country, ID, Time)),]
data_analyse2_p2$Time <- as.numeric(data_analyse2_p2$Time)

model_PADp2 <- lme(fixed = PAD ~ Gender + Edu + DaysMax_p2 + Age_new + DaysMax_p2*Age_new,
                  random = list(Country= pdDiag(~DaysMax_p2), ID = pdDiag(~DaysMax_p2)), 
                  data = data_analyse2_p2, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PADp2)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse2_p2 
           AIC      BIC    logLik
      68893.73 69034.85 -34429.86

    Random effects:
     Formula: ~DaysMax_p2 | Country
     Structure: Diagonal
            (Intercept)  DaysMax_p2
    StdDev:   0.2215429 0.004748276

     Formula: ~DaysMax_p2 | ID %in% Country
     Structure: Diagonal
            (Intercept) DaysMax_p2  Residual
    StdDev:   0.7105237 0.00569683 0.6086598

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2177511 
    Fixed effects:  PAD ~ Gender + Edu + DaysMax_p2 + Age_new + DaysMax_p2 * Age_new 
                             Value  Std.Error    DF   t-value p-value
    (Intercept)          2.9890108 0.05899848 20515  50.66250  0.0000
    GenderF             -0.2700984 0.01911583  9221 -14.12957  0.0000
    GenderO             -0.4652503 0.12163318  9221  -3.82503  0.0001
    Edu                  0.0305627 0.00626111  9221   4.88135  0.0000
    DaysMax_p2           0.0031981 0.00143944 20515   2.22175  0.0263
    Age_new1            -0.1962504 0.03638335  9221  -5.39396  0.0000
    Age_new2            -0.1320575 0.03746536  9221  -3.52479  0.0004
    Age_new3             0.0550860 0.04537270  9221   1.21408  0.2247
    DaysMax_p2:Age_new1  0.0025139 0.00104217 20515   2.41219  0.0159
    DaysMax_p2:Age_new2  0.0031294 0.00106397 20515   2.94123  0.0033
    DaysMax_p2:Age_new3  0.0031393 0.00121595 20515   2.58176  0.0098
     Correlation: 
                        (Intr) GendrF GendrO Edu    DysM_2 Ag_nw1 Ag_nw2 Ag_nw3 DM_2:A_1 DM_2:A_2
    GenderF             -0.244                                                                   
    GenderO             -0.054  0.117                                                            
    Edu                 -0.400 -0.030  0.003                                                     
    DaysMax_p2          -0.219  0.004 -0.008 -0.004                                              
    Age_new1            -0.332  0.044  0.017 -0.220  0.292                                       
    Age_new2            -0.362  0.075  0.042 -0.137  0.280  0.752                                
    Age_new3            -0.329  0.142  0.044 -0.095  0.232  0.624  0.630                         
    DaysMax_p2:Age_new1  0.238  0.001  0.012  0.024 -0.530 -0.571 -0.450 -0.374                  
    DaysMax_p2:Age_new2  0.230  0.001  0.013  0.028 -0.516 -0.454 -0.590 -0.376  0.809           
    DaysMax_p2:Age_new3  0.206 -0.007  0.011  0.019 -0.454 -0.398 -0.399 -0.607  0.710    0.709  

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -4.75070950 -0.52905385  0.04282003  0.55527970  4.51500062 

    Number of Observations: 29779
    Number of Groups: 
            Country ID %in% Country 
                 33            9260 

``` r
VarCorr(model_PADp2)
```

                Variance           StdDev     
    Country =   pdDiag(DaysMax_p2)            
    (Intercept) 4.908124e-02       0.221542869
    DaysMax_p2  2.254612e-05       0.004748276
    ID =        pdDiag(DaysMax_p2)            
    (Intercept) 5.048440e-01       0.710523725
    DaysMax_p2  3.245387e-05       0.005696830
    Residual    3.704667e-01       0.608659763

*Confidence intervals*

``` r
intervals(model_PADp2, which = 'fixed')
```

    Approximate 95% confidence intervals

     Fixed effects:
                                lower         est.        upper
    (Intercept)          2.8733690506  2.989010771  3.104652492
    GenderF             -0.3075697087 -0.270098445 -0.232627181
    GenderO             -0.7036782563 -0.465250315 -0.226822373
    Edu                  0.0182895049  0.030562662  0.042835819
    DaysMax_p2           0.0003766637  0.003198074  0.006019485
    Age_new1            -0.2675698666 -0.196250449 -0.124931032
    Age_new2            -0.2054978597 -0.132057455 -0.058617051
    Age_new3            -0.0338544875  0.055086047  0.144026581
    DaysMax_p2:Age_new1  0.0004711801  0.002513913  0.004556645
    DaysMax_p2:Age_new2  0.0010439165  0.003129380  0.005214843
    DaysMax_p2:Age_new3  0.0007559296  0.003139295  0.005522660

*Plot of predicted values*

``` r
ef_PADp2 <- effect("DaysMax_p2:Age_new", model_PADp2)

plot_PADp2 <- ggplot(as.data.frame(ef_PADp2), aes(DaysMax_p2, fit, color=Age_new)) + 
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + 
  theme_minimal(base_size=10) + 
  labs(title="A",
       x="Days", y = "PAD") +
  theme(plot.title = element_text(size=10)) +
  scale_color_discrete(name="Age", labels = c("18-24", "25-44", "45-64", "65+")) + 
  expand_limits(y=c(1, 5))
```

``` r
plot_PADp2
```

![](PAD-final-including-gender_tables-only_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

*Effect sizes* **Within person SD and average within person SD**

``` r
ISDs <- data_analyse2_p2 %>% 
  group_by(ID) %>%
  summarize_at(c("DaysMax_p2", "PAD"), sd, na.rm=TRUE) %>%
  ungroup()

ISDs_av <- ISDs %>%
  summarize_at(c("DaysMax_p2", "PAD"), mean, na.rm=TRUE) %>%
  stack() %>%
  rename(sd=values) 
```

> Effect sizes for intercept and main effect of age and covariates =
> regression coefficient / average ISD of PAD Effect size for main
> effect of DaysMax = (regression coefficient \* 28)/ average ISD of PAD
> Effect sizes for interaction effects = (regression coefficient \* 28)/
> average ISD of PAD

> The effect sizes for main effect of DaysMax and the interaction
> effects reflect the increase in SD of PAD over 4 weeks (28 days)

``` r
coef_PADp2 = broom.mixed::tidy(model_PADp2, 
               effects = "fixed")

coef_PADp2 <- coef_PADp2 %>%
  mutate(e_size = ifelse(row_number()== 1 | row_number()== 2 |  row_number()== 3 |  row_number()== 4 |  row_number()== 6 |  row_number()== 7 |  row_number()== 8,  estimate/0.5053069, (estimate*28)/0.5053069)) %>%
  mutate(across(3:8, round, 2)) 
```

``` r
coef_PADp2
```

    ## # A tibble: 11 × 8
    ##    effect term                estimate std.error    df statistic p.value e_size
    ##    <chr>  <chr>                  <dbl>     <dbl> <dbl>     <dbl>   <dbl>  <dbl>
    ##  1 fixed  (Intercept)             2.99      0.06 20515     50.7     0      5.92
    ##  2 fixed  GenderF                -0.27      0.02  9221    -14.1     0     -0.53
    ##  3 fixed  GenderO                -0.47      0.12  9221     -3.83    0     -0.92
    ##  4 fixed  Edu                     0.03      0.01  9221      4.88    0      0.06
    ##  5 fixed  DaysMax_p2              0         0    20515      2.22    0.03   0.18
    ##  6 fixed  Age_new1               -0.2       0.04  9221     -5.39    0     -0.39
    ##  7 fixed  Age_new2               -0.13      0.04  9221     -3.52    0     -0.26
    ##  8 fixed  Age_new3                0.06      0.05  9221      1.21    0.22   0.11
    ##  9 fixed  DaysMax_p2:Age_new1     0         0    20515      2.41    0.02   0.14
    ## 10 fixed  DaysMax_p2:Age_new2     0         0    20515      2.94    0      0.17
    ## 11 fixed  DaysMax_p2:Age_new3     0         0    20515      2.58    0.01   0.17

``` r
coef_PADp2 <- as.matrix(coef_PADp2)
```

> PAD increases over time in the youngest age group (main effect), and
> also increases in the older groups but faster (interaction effect).
> Compared to the youngest age group, older groups report lower PAD on
> the first day of max stringency, except for the oldest age group (main
> effect). Women report lower PAD compared to men, and higher educated
> people report higher PAD compared to lower educated people.

# Quadratic term

The interaction between age and daysmax was significant so these are
kept in the current model. The time variable needs to be centered at the
midpoint (mean) to reduce collinearity between the linear and the
quadratic components.

``` r
data_analyse2_p2q <- gmc(data_analyse2_p2, "DaysMax_p2", "ID", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)
```

``` r
data_analyse2_p2q$Edu <- as.numeric(data_analyse2_p2q$Edu)

data_analyse2_p2q <- data_analyse2_p2q[with(data_analyse2_p2q, order(Country, ID, Time)),]
data_analyse2_p2q$Time <- as.numeric(data_analyse2_p2q$Time)

model_PADp2q <- lme(fixed = PAD ~ Gender + Edu + DaysMax_p2_dev + Age_new + DaysMax_p2_dev*Age_new +  
                    + I(DaysMax_p2_dev^2) + I(DaysMax_p2_dev^2)*Age_new,
                  random = list(Country = pdDiag(~ DaysMax_p2_dev), 
                  ID = ~DaysMax_p2_dev),
                  data = data_analyse2_p2q, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PADp2q)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse2_p2q 
           AIC      BIC    logLik
      68886.06 69068.68 -34421.03

    Random effects:
     Formula: ~DaysMax_p2_dev | Country
     Structure: Diagonal
            (Intercept) DaysMax_p2_dev
    StdDev:   0.2071315    0.005714577

     Formula: ~DaysMax_p2_dev | ID %in% Country
     Structure: General positive-definite, Log-Cholesky parametrization
                   StdDev      Corr  
    (Intercept)    0.732387891 (Intr)
    DaysMax_p2_dev 0.007694281 0.101 
    Residual       0.598358955       

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.1923009 
    Fixed effects:  PAD ~ Gender + Edu + DaysMax_p2_dev + Age_new + DaysMax_p2_dev *      Age_new + +I(DaysMax_p2_dev^2) + I(DaysMax_p2_dev^2) * Age_new 
                                      Value  Std.Error    DF   t-value p-value
    (Intercept)                   3.0325570 0.05471545 20511  55.42414  0.0000
    GenderF                      -0.2640508 0.01911217  9221 -13.81585  0.0000
    GenderO                      -0.4658934 0.12106510  9221  -3.84829  0.0001
    Edu                           0.0302065 0.00624718  9221   4.83522  0.0000
    DaysMax_p2_dev                0.0031763 0.00164428 20511   1.93171  0.0534
    Age_new1                     -0.1402693 0.03067173  9221  -4.57324  0.0000
    Age_new2                     -0.0499836 0.03092751  9221  -1.61615  0.1061
    Age_new3                      0.1450250 0.03658189  9221   3.96439  0.0001
    I(DaysMax_p2_dev^2)          -0.0001248 0.00005311 20511  -2.34913  0.0188
    DaysMax_p2_dev:Age_new1       0.0029840 0.00113378 20511   2.63195  0.0085
    DaysMax_p2_dev:Age_new2       0.0036336 0.00115927 20511   3.13440  0.0017
    DaysMax_p2_dev:Age_new3       0.0033629 0.00133182 20511   2.52504  0.0116
    Age_new1:I(DaysMax_p2_dev^2) -0.0000180 0.00005740 20511  -0.31354  0.7539
    Age_new2:I(DaysMax_p2_dev^2)  0.0000064 0.00005772 20511   0.11067  0.9119
    Age_new3:I(DaysMax_p2_dev^2)  0.0000396 0.00006473 20511   0.61139  0.5409
     Correlation: 
                                 (Intr) GendrF GendrO Edu    DyM_2_ Ag_nw1 Ag_nw2 Ag_nw3 I(DM_2 DM_2_:A_1 DM_2_:A_2 DM_2_:A_3 A_1:I( A_2:I(
    GenderF                      -0.262                                                                                                    
    GenderO                      -0.061  0.118                                                                                             
    Edu                          -0.431 -0.031  0.003                                                                                      
    DaysMax_p2_dev                0.007  0.000  0.001  0.000                                                                               
    Age_new1                     -0.280  0.054  0.027 -0.244 -0.011                                                                        
    Age_new2                     -0.334  0.094  0.058 -0.143 -0.011  0.757                                                                 
    Age_new3                     -0.319  0.172  0.060 -0.102 -0.010  0.644  0.661                                                          
    I(DaysMax_p2_dev^2)          -0.094  0.003 -0.013 -0.010  0.023  0.194  0.192  0.163                                                   
    DaysMax_p2_dev:Age_new1      -0.010  0.000 -0.001  0.000 -0.503  0.024  0.019  0.016 -0.025                                            
    DaysMax_p2_dev:Age_new2      -0.009  0.000 -0.002  0.000 -0.488  0.019  0.024  0.016 -0.024  0.810                                     
    DaysMax_p2_dev:Age_new3      -0.008  0.001 -0.001  0.000 -0.429  0.016  0.017  0.020 -0.022  0.707     0.705                           
    Age_new1:I(DaysMax_p2_dev^2)  0.091 -0.005  0.010  0.006 -0.020 -0.214 -0.171 -0.146 -0.922  0.029     0.024     0.021                 
    Age_new2:I(DaysMax_p2_dev^2)  0.091 -0.006  0.012  0.004 -0.020 -0.172 -0.214 -0.147 -0.917  0.024     0.031     0.019     0.848       
    Age_new3:I(DaysMax_p2_dev^2)  0.084 -0.013  0.011  0.002 -0.019 -0.154 -0.155 -0.211 -0.818  0.021     0.019     0.055     0.756  0.752

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -4.72815523 -0.52777629  0.04115468  0.54910411  4.54269032 

    Number of Observations: 29779
    Number of Groups: 
            Country ID %in% Country 
                 33            9260 

``` r
VarCorr(model_PADp2q)
```

                   Variance                  StdDev      Corr  
    Country =      pdDiag(DaysMax_p2_dev)                      
    (Intercept)    4.290345e-02              0.207131471       
    DaysMax_p2_dev 3.265639e-05              0.005714577       
    ID =           pdLogChol(DaysMax_p2_dev)                   
    (Intercept)    5.363920e-01              0.732387891 (Intr)
    DaysMax_p2_dev 5.920196e-05              0.007694281 0.101 
    Residual       3.580334e-01              0.598358955       

Results suggest that there is a linear increase of PAA over time in the
youngest age group and a faster increase in the older age groups. The
quadratic effect was significant, showing a decellarating rate of
increase in the youngest age group, and no differences between age
groups.

*Confidence intervals*

``` r
intervals(model_PADp2q, which = 'fixed')
```

    Approximate 95% confidence intervals

     Fixed effects:
                                         lower          est.         upper
    (Intercept)                   2.925310e+00  3.032557e+00  3.139804e+00
    GenderF                      -3.015148e-01 -2.640508e-01 -2.265867e-01
    GenderO                      -7.032077e-01 -4.658934e-01 -2.285790e-01
    Edu                           1.796063e-02  3.020649e-02  4.245236e-02
    DaysMax_p2_dev               -4.664783e-05  3.176268e-03  6.399184e-03
    Age_new1                     -2.003927e-01 -1.402693e-01 -8.014590e-02
    Age_new2                     -1.106083e-01 -4.998357e-02  1.064120e-02
    Age_new3                      7.331637e-02  1.450250e-01  2.167336e-01
    I(DaysMax_p2_dev^2)          -2.288532e-04 -1.247574e-04 -2.066157e-05
    DaysMax_p2_dev:Age_new1       7.617490e-04  2.984043e-03  5.206337e-03
    DaysMax_p2_dev:Age_new2       1.361346e-03  3.633602e-03  5.905858e-03
    DaysMax_p2_dev:Age_new3       7.524203e-04  3.362890e-03  5.973360e-03
    Age_new1:I(DaysMax_p2_dev^2) -1.305014e-04 -1.799653e-05  9.450833e-05
    Age_new2:I(DaysMax_p2_dev^2) -1.067555e-04  6.388221e-06  1.195319e-04
    Age_new3:I(DaysMax_p2_dev^2) -8.729927e-05  3.957440e-05  1.664481e-04

*Plot of predicted values*

``` r
ef_PADp2q <- effect("Age_new:I(DaysMax_p2_dev^2)", model_PADp2q)

plot_PADp2q <- ggplot(as.data.frame(ef_PADp2q), aes(DaysMax_p2_dev, fit, color=Age_new)) + 
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + 
  theme_minimal(base_size=10) + 
  labs(title="Quadratic PAD trajectories during peak restrictions",
       x="Days (centered)", y = "PAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name="Age", labels = c("18-24", "25-44", "45-64", "65+")) + 
  expand_limits(y=c(1, 5))
```

``` r
plot_PADp2q
```

![](PAD-final-including-gender_tables-only_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

# Phase 3

> Random: IC for ID and Country, S for ID and country, no correlation
> between IC and S for ID and country + AR

``` r
data_analyse2_p3$Edu <- as.numeric(data_analyse2_p3$Edu)
data_analyse2_p3 <- data_analyse2_p3[with(data_analyse2_p3, order(Country, ID, Time)),]
data_analyse2_p3$Time <- as.numeric(data_analyse2_p3$Time)

model_PADp3 <- lme(fixed = PAD ~ Gender + Edu + DaysPhase3 + Age_new + DaysPhase3*Age_new,
                  random = list(Country = pdDiag(~DaysPhase3), ID = pdDiag(~DaysPhase3)),
                  data = data_analyse2_p3, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PADp3)
```

    Linear mixed-effects model fit by REML
      Data: data_analyse2_p3 
           AIC      BIC    logLik
      57239.65 57377.85 -28602.83

    Random effects:
     Formula: ~DaysPhase3 | Country
     Structure: Diagonal
            (Intercept)  DaysPhase3
    StdDev:   0.2121068 0.002289937

     Formula: ~DaysPhase3 | ID %in% Country
     Structure: Diagonal
            (Intercept)  DaysPhase3  Residual
    StdDev:   0.7280485 0.006425761 0.6010516

    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
        Phi1 
    0.160957 
    Fixed effects:  PAD ~ Gender + Edu + DaysPhase3 + Age_new + DaysPhase3 * Age_new 
                             Value  Std.Error    DF   t-value p-value
    (Intercept)          3.0236071 0.06056431 18008  49.92391  0.0000
    GenderF             -0.2525287 0.02188678  7031 -11.53795  0.0000
    GenderO             -0.2476696 0.17610583  7031  -1.40637  0.1597
    Edu                  0.0284462 0.00726909  7031   3.91330  0.0001
    DaysPhase3          -0.0024101 0.00113607 18008  -2.12140  0.0339
    Age_new1            -0.0627716 0.04070587  7031  -1.54208  0.1231
    Age_new2             0.0130850 0.04019384  7031   0.32555  0.7448
    Age_new3             0.1942526 0.04620033  7031   4.20457  0.0000
    DaysPhase3:Age_new1  0.0030707 0.00119012 18008   2.58020  0.0099
    DaysPhase3:Age_new2  0.0039254 0.00116609 18008   3.36626  0.0008
    DaysPhase3:Age_new3  0.0049377 0.00130191 18008   3.79264  0.0001
     Correlation: 
                        (Intr) GendrF GendrO Edu    DysPh3 Ag_nw1 Ag_nw2 Ag_nw3 DP3:A_1 DP3:A_2
    GenderF             -0.281                                                                 
    GenderO             -0.054  0.095                                                          
    Edu                 -0.452 -0.027  0.003                                                   
    DaysPhase3          -0.263 -0.004 -0.007 -0.005                                            
    Age_new1            -0.366  0.050  0.015 -0.222  0.377                                     
    Age_new2            -0.440  0.106  0.047 -0.115  0.383  0.751                              
    Age_new3            -0.427  0.184  0.049 -0.074  0.337  0.658  0.702                       
    DaysPhase3:Age_new1  0.242  0.000  0.004  0.002 -0.772 -0.489 -0.374 -0.327                
    DaysPhase3:Age_new2  0.247  0.002  0.008  0.004 -0.791 -0.377 -0.489 -0.342  0.773         
    DaysPhase3:Age_new3  0.226 -0.001  0.008  0.000 -0.718 -0.339 -0.352 -0.483  0.696   0.729 

    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -4.98869678 -0.51414557  0.06348494  0.54004414  3.75467309 

    Number of Observations: 25081
    Number of Groups: 
            Country ID %in% Country 
                 32            7069 

``` r
VarCorr(model_PADp3)
```

                Variance           StdDev     
    Country =   pdDiag(DaysPhase3)            
    (Intercept) 4.498928e-02       0.212106755
    DaysPhase3  5.243811e-06       0.002289937
    ID =        pdDiag(DaysPhase3)            
    (Intercept) 5.300547e-01       0.728048537
    DaysPhase3  4.129041e-05       0.006425761
    Residual    3.612631e-01       0.601051623

*Confidence intervals*

``` r
intervals(model_PADp3, which = 'fixed')
```

    Approximate 95% confidence intervals

     Fixed effects:
                                lower         est.         upper
    (Intercept)          2.9048952933  3.023607142  3.1423189902
    GenderF             -0.2954333545 -0.252528665 -0.2096239762
    GenderO             -0.5928901206 -0.247669599  0.0975509217
    Edu                  0.0141965500  0.028446163  0.0426957750
    DaysPhase3          -0.0046368633 -0.002410056 -0.0001832478
    Age_new1            -0.1425673613 -0.062771595  0.0170241717
    Age_new2            -0.0657069895  0.013085044  0.0918770782
    Age_new3             0.1036859813  0.194252562  0.2848191417
    DaysPhase3:Age_new1  0.0007379952  0.003070737  0.0054034787
    DaysPhase3:Age_new2  0.0016397109  0.003925353  0.0062109944
    DaysPhase3:Age_new3  0.0023858114  0.004937682  0.0074895520

*Plot of predicted values*

``` r
ef_PADp3 <- effect("DaysPhase3:Age_new", model_PADp3)

plot_PADp3 <- ggplot(as.data.frame(ef_PADp3), aes(DaysPhase3, fit, color=Age_new)) + 
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + 
  theme_minimal(base_size=10) + 
  labs(title="B",
       x="Days", y = "PAD") +
  theme(plot.title = element_text(size=10)) +
  scale_color_discrete(name="Age", labels = c("18-24", "25-44", "45-64", "65+")) + 
  expand_limits(y=c(1, 5))
```

``` r
plot_PADp3
```

![](PAD-final-including-gender_tables-only_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
plot_PADp2and3 <- ggarrange(plot_PADp2, plot_PADp3 , 
          ncol = 2, nrow = 1, common.legend=TRUE, legend= "bottom")

plot_PADp2and3 <- annotate_figure(plot_PADp2and3,top = text_grob("PAD trajectories during peak (A) and easing (B) of restrictions", size = 12))
```

``` r
plot_PADp2and3
```

![](PAD-final-including-gender_tables-only_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

*Effect sizes* **Within person SD and average within person SD for PAD**

``` r
ISDs <- data_analyse2_p3 %>% 
  group_by(ID) %>%
  summarize_at(c("PAD"), sd, na.rm=TRUE) %>%
  ungroup()

ISDs_av <- ISDs %>%
  summarize_at(c("PAD"), mean, na.rm=TRUE) %>%
  stack() %>%
  rename(sd=values) 
```

> Effect sizes for intercept and main effect of age = regression
> coefficient / average ISD of PAD Effect size for main effect of
> DaysMax = (regression coefficient \* 28)/ average ISD of PAD Effect
> sizes for interaction effects = (regression coefficient \* 28)/
> average ISD of PAD

> The effect sizes for main effect of DaysMax and the interaction
> effects reflect the increase in SD of PAD over 4 weeks (28 days)

``` r
coef_PADp3 = broom.mixed::tidy(model_PADp3, 
               effects = "fixed")

coef_PADp3 <- coef_PADp3 %>%
 mutate(e_size = ifelse(row_number()== 1 | row_number()== 2 |  row_number()== 3 |  row_number()== 4 |  row_number()== 6 |  row_number()== 7 |  row_number()== 8, estimate/0.493984, (estimate*28)/0.493984)) %>%
  mutate(across(3:8, round, 2)) 
```

``` r
coef_PADp3
```

    ## # A tibble: 11 × 8
    ##    effect term                estimate std.error    df statistic p.value e_size
    ##    <chr>  <chr>                  <dbl>     <dbl> <dbl>     <dbl>   <dbl>  <dbl>
    ##  1 fixed  (Intercept)             3.02      0.06 18008     49.9     0      6.12
    ##  2 fixed  GenderF                -0.25      0.02  7031    -11.5     0     -0.51
    ##  3 fixed  GenderO                -0.25      0.18  7031     -1.41    0.16  -0.5 
    ##  4 fixed  Edu                     0.03      0.01  7031      3.91    0      0.06
    ##  5 fixed  DaysPhase3              0         0    18008     -2.12    0.03  -0.14
    ##  6 fixed  Age_new1               -0.06      0.04  7031     -1.54    0.12  -0.13
    ##  7 fixed  Age_new2                0.01      0.04  7031      0.33    0.74   0.03
    ##  8 fixed  Age_new3                0.19      0.05  7031      4.2     0      0.39
    ##  9 fixed  DaysPhase3:Age_new1     0         0    18008      2.58    0.01   0.17
    ## 10 fixed  DaysPhase3:Age_new2     0         0    18008      3.37    0      0.22
    ## 11 fixed  DaysPhase3:Age_new3     0         0    18008      3.79    0      0.28

``` r
coef_PADp3 <- as.matrix(coef_PADp3)
```

> PAD seems to decrease over time in the youngest group but not
> significant (main effect), whereas it increases over time in the older
> age groups (interaction effect) very slightly. There are no age
> differences in PAD on the first day stringency reduces again after the
> peak, except for the oldest age group which reports higher PAD (main
> effect). Women report lower PAD, higher educated groups report higher
> PAD.

``` r
stargazer(coef_PADp1, coef_PADp2, coef_PADp3,
type="html", df = TRUE, out="star_coefallphase_PAD.doc",  single.row=TRUE, digits = 3, align = TRUE)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>effect</td><td>term</td><td>estimate</td><td>std.error</td><td>df</td><td>statistic</td><td>p.value</td><td>e_size</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>fixed</td><td>(Intercept)</td><td>2.94</td><td>0.10</td><td>2730</td><td>28.82</td><td>0.00</td><td>5.79</td></tr>
    ## <tr><td>fixed</td><td>GenderF</td><td>-0.27</td><td>0.04</td><td>2730</td><td>-7.33</td><td>0.00</td><td>-0.54</td></tr>
    ## <tr><td>fixed</td><td>GenderO</td><td>-0.38</td><td>0.23</td><td>2730</td><td>-1.66</td><td>0.10</td><td>-0.76</td></tr>
    ## <tr><td>fixed</td><td>Edu</td><td>0.02</td><td>0.01</td><td>2730</td><td>1.29</td><td>0.20</td><td>0.03</td></tr>
    ## <tr><td>fixed</td><td>Age_new1</td><td>-0.16</td><td>0.05</td><td>2730</td><td>-3.04</td><td>0.00</td><td>-0.31</td></tr>
    ## <tr><td>fixed</td><td>Age_new2</td><td>-0.01</td><td>0.06</td><td>2730</td><td>-0.10</td><td>0.92</td><td>-0.01</td></tr>
    ## <tr><td>fixed</td><td>Age_new3</td><td>0.22</td><td>0.08</td><td>2730</td><td>2.86</td><td>0.00</td><td>0.44</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>effect</td><td>term</td><td>estimate</td><td>std.error</td><td>df</td><td>statistic</td><td>p.value</td><td>e_size</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>fixed</td><td>(Intercept)</td><td>2.99</td><td>0.06</td><td>20515</td><td>50.66</td><td>0.00</td><td>5.92</td></tr>
    ## <tr><td>fixed</td><td>GenderF</td><td>-0.27</td><td>0.02</td><td>9221</td><td>-14.13</td><td>0.00</td><td>-0.53</td></tr>
    ## <tr><td>fixed</td><td>GenderO</td><td>-0.47</td><td>0.12</td><td>9221</td><td>-3.83</td><td>0.00</td><td>-0.92</td></tr>
    ## <tr><td>fixed</td><td>Edu</td><td>0.03</td><td>0.01</td><td>9221</td><td>4.88</td><td>0.00</td><td>0.06</td></tr>
    ## <tr><td>fixed</td><td>DaysMax_p2</td><td>0.00</td><td>0.00</td><td>20515</td><td>2.22</td><td>0.03</td><td>0.18</td></tr>
    ## <tr><td>fixed</td><td>Age_new1</td><td>-0.20</td><td>0.04</td><td>9221</td><td>-5.39</td><td>0.00</td><td>-0.39</td></tr>
    ## <tr><td>fixed</td><td>Age_new2</td><td>-0.13</td><td>0.04</td><td>9221</td><td>-3.52</td><td>0.00</td><td>-0.26</td></tr>
    ## <tr><td>fixed</td><td>Age_new3</td><td>0.06</td><td>0.05</td><td>9221</td><td>1.21</td><td>0.22</td><td>0.11</td></tr>
    ## <tr><td>fixed</td><td>DaysMax_p2:Age_new1</td><td>0.00</td><td>0.00</td><td>20515</td><td>2.41</td><td>0.02</td><td>0.14</td></tr>
    ## <tr><td>fixed</td><td>DaysMax_p2:Age_new2</td><td>0.00</td><td>0.00</td><td>20515</td><td>2.94</td><td>0.00</td><td>0.17</td></tr>
    ## <tr><td>fixed</td><td>DaysMax_p2:Age_new3</td><td>0.00</td><td>0.00</td><td>20515</td><td>2.58</td><td>0.01</td><td>0.17</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>effect</td><td>term</td><td>estimate</td><td>std.error</td><td>df</td><td>statistic</td><td>p.value</td><td>e_size</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td>fixed</td><td>(Intercept)</td><td>3.02</td><td>0.06</td><td>18008</td><td>49.92</td><td>0.00</td><td>6.12</td></tr>
    ## <tr><td>fixed</td><td>GenderF</td><td>-0.25</td><td>0.02</td><td>7031</td><td>-11.54</td><td>0.00</td><td>-0.51</td></tr>
    ## <tr><td>fixed</td><td>GenderO</td><td>-0.25</td><td>0.18</td><td>7031</td><td>-1.41</td><td>0.16</td><td>-0.50</td></tr>
    ## <tr><td>fixed</td><td>Edu</td><td>0.03</td><td>0.01</td><td>7031</td><td>3.91</td><td>0.00</td><td>0.06</td></tr>
    ## <tr><td>fixed</td><td>DaysPhase3</td><td>0.00</td><td>0.00</td><td>18008</td><td>-2.12</td><td>0.03</td><td>-0.14</td></tr>
    ## <tr><td>fixed</td><td>Age_new1</td><td>-0.06</td><td>0.04</td><td>7031</td><td>-1.54</td><td>0.12</td><td>-0.13</td></tr>
    ## <tr><td>fixed</td><td>Age_new2</td><td>0.01</td><td>0.04</td><td>7031</td><td>0.33</td><td>0.74</td><td>0.03</td></tr>
    ## <tr><td>fixed</td><td>Age_new3</td><td>0.19</td><td>0.05</td><td>7031</td><td>4.20</td><td>0.00</td><td>0.39</td></tr>
    ## <tr><td>fixed</td><td>DaysPhase3:Age_new1</td><td>0.00</td><td>0.00</td><td>18008</td><td>2.58</td><td>0.01</td><td>0.17</td></tr>
    ## <tr><td>fixed</td><td>DaysPhase3:Age_new2</td><td>0.00</td><td>0.00</td><td>18008</td><td>3.37</td><td>0.00</td><td>0.22</td></tr>
    ## <tr><td>fixed</td><td>DaysPhase3:Age_new3</td><td>0.00</td><td>0.00</td><td>18008</td><td>3.79</td><td>0.00</td><td>0.28</td></tr>
    ## <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>

``` r
stargazer(model_PADp1, model_PADp2, model_PADp2q, model_PADp3,
type="html", df = TRUE, out="starallphasesPAD.doc",  single.row=TRUE, digits = 3, align = TRUE,
intercept.top = TRUE, intercept.bottom = FALSE)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td colspan="4">PAD</td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Constant</td><td>2.939<sup>***</sup> (0.102)</td><td>2.989<sup>***</sup> (0.059)</td><td>3.033<sup>***</sup> (0.055)</td><td>3.024<sup>***</sup> (0.061)</td></tr>
    ## <tr><td style="text-align:left">GenderF</td><td>-0.274<sup>***</sup> (0.037)</td><td>-0.270<sup>***</sup> (0.019)</td><td>-0.264<sup>***</sup> (0.019)</td><td>-0.253<sup>***</sup> (0.022)</td></tr>
    ## <tr><td style="text-align:left">GenderO</td><td>-0.385<sup>*</sup> (0.232)</td><td>-0.465<sup>***</sup> (0.122)</td><td>-0.466<sup>***</sup> (0.121)</td><td>-0.248 (0.176)</td></tr>
    ## <tr><td style="text-align:left">Edu</td><td>0.018 (0.014)</td><td>0.031<sup>***</sup> (0.006)</td><td>0.030<sup>***</sup> (0.006)</td><td>0.028<sup>***</sup> (0.007)</td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2</td><td></td><td>0.003<sup>**</sup> (0.001)</td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2_dev</td><td></td><td></td><td>0.003<sup>*</sup> (0.002)</td><td></td></tr>
    ## <tr><td style="text-align:left">DaysPhase3</td><td></td><td></td><td></td><td>-0.002<sup>**</sup> (0.001)</td></tr>
    ## <tr><td style="text-align:left">Age_new1</td><td>-0.158<sup>***</sup> (0.052)</td><td>-0.196<sup>***</sup> (0.036)</td><td>-0.140<sup>***</sup> (0.031)</td><td>-0.063 (0.041)</td></tr>
    ## <tr><td style="text-align:left">Age_new2</td><td>-0.005 (0.055)</td><td>-0.132<sup>***</sup> (0.037)</td><td>-0.050 (0.031)</td><td>0.013 (0.040)</td></tr>
    ## <tr><td style="text-align:left">Age_new3</td><td>0.224<sup>***</sup> (0.078)</td><td>0.055 (0.045)</td><td>0.145<sup>***</sup> (0.037)</td><td>0.194<sup>***</sup> (0.046)</td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2:Age_new1</td><td></td><td>0.003<sup>**</sup> (0.001)</td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2:Age_new2</td><td></td><td>0.003<sup>***</sup> (0.001)</td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2:Age_new3</td><td></td><td>0.003<sup>***</sup> (0.001)</td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">I(DaysMax_p2_dev2)</td><td></td><td></td><td>-0.0001<sup>**</sup> (0.0001)</td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2_dev:Age_new1</td><td></td><td></td><td>0.003<sup>***</sup> (0.001)</td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2_dev:Age_new2</td><td></td><td></td><td>0.004<sup>***</sup> (0.001)</td><td></td></tr>
    ## <tr><td style="text-align:left">DaysMax_p2_dev:Age_new3</td><td></td><td></td><td>0.003<sup>**</sup> (0.001)</td><td></td></tr>
    ## <tr><td style="text-align:left">Age_new1:I(DaysMax_p2_dev2)</td><td></td><td></td><td>-0.00002 (0.0001)</td><td></td></tr>
    ## <tr><td style="text-align:left">Age_new2:I(DaysMax_p2_dev2)</td><td></td><td></td><td>0.00001 (0.0001)</td><td></td></tr>
    ## <tr><td style="text-align:left">Age_new3:I(DaysMax_p2_dev2)</td><td></td><td></td><td>0.00004 (0.0001)</td><td></td></tr>
    ## <tr><td style="text-align:left">DaysPhase3:Age_new1</td><td></td><td></td><td></td><td>0.003<sup>***</sup> (0.001)</td></tr>
    ## <tr><td style="text-align:left">DaysPhase3:Age_new2</td><td></td><td></td><td></td><td>0.004<sup>***</sup> (0.001)</td></tr>
    ## <tr><td style="text-align:left">DaysPhase3:Age_new3</td><td></td><td></td><td></td><td>0.005<sup>***</sup> (0.001)</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>4,177</td><td>29,779</td><td>29,779</td><td>25,081</td></tr>
    ## <tr><td style="text-align:left">Log Likelihood</td><td>-5,478.703</td><td>-34,429.860</td><td>-34,421.030</td><td>-28,602.830</td></tr>
    ## <tr><td style="text-align:left">Akaike Inf. Crit.</td><td>10,977.410</td><td>68,893.730</td><td>68,886.060</td><td>57,239.650</td></tr>
    ## <tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>11,040.760</td><td>69,034.850</td><td>69,068.680</td><td>57,377.860</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    ## </table>
