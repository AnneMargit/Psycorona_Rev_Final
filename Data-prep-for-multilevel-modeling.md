Data preparation including all gender categories
================
Anne Margit
04/25/2020

> Final dataset including all gender categories

``` r
library(ggplot2)
library(dplyr)
library(knitr)
library(arsenal)
library(tidyverse)
library(anytime)
library(rockchalk)
```

``` r
load("data_long.Rdata")
oxforddata <- read.csv("OxCGRT_latest.csv", header=TRUE)
```

To check whether country names are different in both datasets:

``` r
data_long$coded_country <- as.factor(data_long$coded_country)
oxforddata$CountryName <- as.factor(oxforddata$CountryName)

levels1 <- levels(data_long$coded_country)
levels2 <- levels(oxforddata$CountryName)

levels1<- as.data.frame(levels1)
levels2<- as.data.frame(levels2)

levels1$levels1 <-as.character(levels1$levels1)
levels2$levels2 <-as.character(levels2$levels2)

check <- comparedf(levels1, levels2, by.x="levels1", by.y="levels2")
summary(check)
```

Some data preparation

``` r
data_long$coded_country <- 
  plyr::revalue(data_long$coded_country , c("Hong Kong S.A.R."="Hong Kong", 
                                       "Kyrgyzstan" = "Kyrgyz Republic",
                                       "United Republic of Tanzania" = "Tanzania", 
                                       "United States of America" = "United States",
                                       "Republic of Serbia" = "Serbia",
                                       "Slovakia" = "Slovak Republic"))
```

Renaming and recoding variables:

``` r
data_long$age <- as.factor(data_long$age)
names(data_long)[names(data_long) == "age"] <- "Age"
names(data_long)[names(data_long) == "coded_country"] <- "Country"
names(data_long)[names(data_long) == "X"] <- "ID"
names(data_long)[names(data_long) == "RecordedDate"] <- "Date"
names(data_long)[names(data_long) == "gender"] <- "Gender"
names(data_long)[names(data_long) == "edu"] <- "Edu"
```

Recoding NA into zero’s:

``` r
data_long$Close1[is.na(data_long$Close1)] <- 0
data_long$Close2[is.na(data_long$Close2)] <- 0
data_long$Close3[is.na(data_long$Close3)] <- 0
data_long$Close4[is.na(data_long$Close4)] <- 0
data_long$Close5[is.na(data_long$Close5)] <- 0
data_long$Close6[is.na(data_long$Close6)] <- 0
```

Delete participants with missing data on which country they currently
live:

``` r
data_long$Country[data_long$Country == ""] <- NA

data_long <- data_long %>%
  filter(!is.na(Country))
```

Delete participants that are not citizens of the country they currently
live in: Note: Citizen = Have you been a citizen of this country since
birth? countryCitizen = Are you a citizen of this country?

``` r
data_long <- data_long %>%
  filter(countryCitizen == 1)
```

Delete participants with missing data on age

``` r
data_long <-data_long %>%
filter(!is.na(Age))
```

Delete participants with missing data on gender

``` r
data_long <-data_long %>%
filter(!is.na(Gender))
```

Recode gender into male = 0 and female = 1 and other = 2

``` r
data_long$Gender[data_long$Gender == "2"] <- 0
```

Delete participants with missing data on education:

``` r
data_long <- data_long %>%
  filter(!is.na(Edu))
```

Delete emotion variables we don’t study (Bored, Loved, Content, Excited)

``` r
data_long <- data_long %>%
  select(-c(Bored, Lov, Content, Exc))
```

Add number of measurements

``` r
data_long$Date <- as.Date(data_long$Date)
data_long <- data_long %>% group_by(ID) %>% add_tally(wt = !is.na(Date))
```

Filter participants with at least 3 measurements

``` r
data_long_min3 <- data_long %>% filter(n > 2)
```

Rename Wave column into Time ordered 1-12

``` r
data_long_min3 <- data_long_min3 %>%
  rename(Time = Wave)

data_long_min3$Time <- data_long_min3$Time %>%
  plyr::revalue(c("w0"="1","w1"="2","w2"="3","w3"="4","w4"="5","w5"="6","w6"="7","w7"="8","w8"="9","w9"="10","w10"="11","w11"="12"))
```

``` r
data_long_min3_g <- data_long_min3

save(data_long_min3_g, file="data_long_min3_g.Rdata")
```

Calculate number of participants per country & select countries with
\>20 participants:

``` r
data_long_min3_g$Country <- as.factor(data_long_min3_g$Country)
data_long_min3_g <- as_tibble(data_long_min3_g)

Country_N <- data_long_min3_g %>%
  filter(Time == "1") %>%
  group_by(Country) %>%
  dplyr::summarise(N = n())
```

``` r
data_long_min3_gn <- left_join(data_long_min3_g, Country_N, by="Country")
```

``` r
data_long_min3_g20 <- data_long_min3_gn %>%
  filter(N>20)
```

``` r
data_long_min3_g20 <- data_long_min3_g20 %>%
  select(-c(Citizen, countryCitizen))

summary(data_long_min3_g20)
```

    ##        ID             Time            Date                 Age       
    ##  Min.   :   13   1      :10509   Min.   :2020-03-19   2      :23688  
    ##  1st Qu.:14112   2      :10509   1st Qu.:2020-04-18   3      :22944  
    ##  Median :30267   3      :10509   Median :2020-05-04   4      :22440  
    ##  Mean   :29516   4      :10509   Mean   :2020-05-05   5      :21660  
    ##  3rd Qu.:43299   5      :10509   3rd Qu.:2020-05-23   6      :16644  
    ##  Max.   :62491   6      :10509   Max.   :2020-07-07   1      :16380  
    ##                  (Other):63054   NA's   :66416        (Other): 2352  
    ##      Gender            Edu                Country           Ang       
    ##  Min.   :0.0000   Min.   :1.00   United States:28584   Min.   :1.00   
    ##  1st Qu.:0.0000   1st Qu.:4.00   Spain        :12372   1st Qu.:1.00   
    ##  Median :1.0000   Median :5.00   Netherlands  : 8184   Median :2.00   
    ##  Mean   :0.6808   Mean   :4.51   Greece       : 8148   Mean   :1.98   
    ##  3rd Qu.:1.0000   3rd Qu.:6.00   Serbia       : 6120   3rd Qu.:3.00   
    ##  Max.   :3.0000   Max.   :7.00   Italy        : 5760   Max.   :5.00   
    ##                                  (Other)      :56940   NA's   :77105  
    ##     Anxiety           Calm            Depr           Energ      
    ##  Min.   :1.00    Min.   :1.00    Min.   :1       Min.   :1.00   
    ##  1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1       1st Qu.:2.00   
    ##  Median :2.00    Median :3.00    Median :2       Median :3.00   
    ##  Mean   :2.38    Mean   :3.06    Mean   :2       Mean   :2.65   
    ##  3rd Qu.:3.00    3rd Qu.:4.00    3rd Qu.:3       3rd Qu.:3.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5       Max.   :5.00   
    ##  NA's   :66634   NA's   :66624   NA's   :66664   NA's   :66691  
    ##       Exh             Insp            Nerv            Rel       
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1.00   
    ##  1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1.00    1st Qu.:2.00   
    ##  Median :2.00    Median :2.00    Median :2.00    Median :3.00   
    ##  Mean   :2.35    Mean   :2.48    Mean   :2.26    Mean   :2.88   
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:4.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00   
    ##  NA's   :66687   NA's   :66717   NA's   :66664   NA's   :66669  
    ##      Close1             Close2            Close3            Close4       
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.000000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.002458   Mean   :0.01685   Mean   :0.01994   Mean   :0.06181  
    ##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.000000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##                                                                          
    ##      Close5            Close6             n               N         
    ##  Min.   :0.00000   Min.   :0.0000   Min.   : 3.00   Min.   :  22.0  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.: 3.00   1st Qu.: 262.0  
    ##  Median :0.00000   Median :0.0000   Median : 5.00   Median : 510.0  
    ##  Mean   :0.04345   Mean   :0.3563   Mean   : 5.68   Mean   : 890.8  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.: 8.00   3rd Qu.:1031.0  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :12.00   Max.   :2382.0  
    ## 

``` r
summary(data_long_min3_g20$Country)
```

    ##          United States                  Spain            Netherlands 
    ##                  28584                  12372                   8184 
    ##                 Greece                 Serbia                  Italy 
    ##                   8148                   6120                   5760 
    ##         United Kingdom                Germany                 France 
    ##                   5628                   4836                   4320 
    ##                 Canada                Romania                Ukraine 
    ##                   3756                   3744                   3144 
    ##           South Africa                 Brazil              Argentina 
    ##                   2664                   2640                   2568 
    ##              Australia                 Russia                Hungary 
    ##                   2496                   2424                   2352 
    ##              Indonesia                 Turkey                Croatia 
    ##                   2244                   2208                   1704 
    ##                 Poland            Philippines               Malaysia 
    ##                   1596                   1572                   1296 
    ##                  Chile             Kazakhstan                  Japan 
    ##                   1044                    996                    924 
    ##           Saudi Arabia              Singapore                   Peru 
    ##                    852                    528                    432 
    ##                Vietnam                 Kosovo            South Korea 
    ##                    360                    348                    264 
    ##                                       Albania                Algeria 
    ##                      0                      0                      0 
    ##                Andorra                Armenia                Austria 
    ##                      0                      0                      0 
    ##             Azerbaijan                Bahrain             Bangladesh 
    ##                      0                      0                      0 
    ##                Belarus                Belgium                  Benin 
    ##                      0                      0                      0 
    ## Bosnia and Herzegovina               Botswana                 Brunei 
    ##                      0                      0                      0 
    ##               Bulgaria               Cambodia               Cameroon 
    ##                      0                      0                      0 
    ##                  China               Colombia             Costa Rica 
    ##                      0                      0                      0 
    ##                 Cyprus         Czech Republic                Denmark 
    ##                      0                      0                      0 
    ##     Dominican Republic                Ecuador                  Egypt 
    ##                      0                      0                      0 
    ##            El Salvador                Estonia               Ethiopia 
    ##                      0                      0                      0 
    ##                Finland                Georgia              Guatemala 
    ##                      0                      0                      0 
    ##              Hong Kong                Iceland                  India 
    ##                      0                      0                      0 
    ##                   Iran                   Iraq                Ireland 
    ##                      0                      0                      0 
    ##                 Israel                Jamaica                 Jordan 
    ##                      0                      0                      0 
    ##                  Kenya                 Kuwait        Kyrgyz Republic 
    ##                      0                      0                      0 
    ##                   Laos                 Latvia                Lebanon 
    ##                      0                      0                      0 
    ##                  Libya              Lithuania             Luxembourg 
    ##                      0                      0                      0 
    ##                   Mali                  Malta              Mauritius 
    ##                      0                      0                      0 
    ##                 Mexico                Moldova               Mongolia 
    ##                      0                      0                      0 
    ##             Montenegro                Morocco                Myanmar 
    ##                      0                      0                      0 
    ##                  Nepal            New Zealand                Nigeria 
    ##                      0                      0                      0 
    ##                 Norway                   Oman               Pakistan 
    ##                      0                      0                      0 
    ##                (Other) 
    ##                      0

Save

``` r
save(data_long_min3_g20, file="data_long_min3_g20.Rdata")
```

Recode oxford data:

``` r
oxforddata$Date <-anydate(oxforddata$Date)
oxforddata$Country <- oxforddata$CountryName
```

Drop not used variables:

``` r
Stringency_data <- oxforddata %>% select(Date, Country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Impute Stringency Index, fill empty rows with last previously reported
index score: Create new Stringency Index variable first

``` r
Stringency_data$StringencyIndex_imp <- Stringency_data$StringencyIndex
```

First join, then impute

``` r
data_long_min3_gstr <- left_join(data_long_min3_g20, Stringency_data, by=c("Country", "Date"))
```

Impute

``` r
data_long_min3_gstr<-data_long_min3_gstr[with(data_long_min3_gstr, order(Country, Date)),]

data_long_min3_gstr2 <- data_long_min3_gstr %>%
  group_by(Country) %>%
  fill(StringencyIndex_imp, .direction = "down")

data_long_min3_gstr <- data_long_min3_gstr2
```

Check if StringencyIndex and StringencyIndex_imp are identical (they
shouldnt be)

``` r
identical(data_long_min3_gstr[["StringencyIndex"]],data_long_min3_gstr[["StringencyIndex_imp"]])
```

    ## [1] FALSE

Save: This dataset includes measurements from participants that (1)
provided at least 3 measurements, (2) that are residents of the country
they currently live in, (3) from countries with at least 20
participants, (4) provided data on age, and (5) with imputed Stringency
index values, (6) for baseline through wave 11

``` r
save(data_long_min3_gstr, file ="data_long_min3_gstr.Rdata")
```

Centering Stringency Index

``` r
data_long_min3_gstrc <- gmc(data_long_min3_gstr, "StringencyIndex_imp", "Country", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)
```

This dataset also adds the country mean centered stringency index

``` r
save(data_long_min3_gstrc, file="data_long_min3_gstrc.Rdata")
```

Make new age groups: Youth: aged 18-24 coded as 1 \<- 0 Young adults:
aged 25-44 coded as 2 or 3 \<- 1 Middle-aged adults: aged 45-64 coded as
4 or 5 \<- 2 Older-aged adults: aged 65+ coded as 6, 7, or 8 \<- 3

``` r
data_long_min3_gstr_age <- data_long_min3_gstr 
data_long_min3_gstr_age$Age_new <- data_long_min3_gstr_age$Age
```

``` r
data_long_min3_gstr_age$Age_new <- data_long_min3_gstr_age$Age_new %>%
  plyr::revalue(c("1"="0", "2"= "1", "3"="1", "4"="2", "5"="2", "6"="3", "7"="3", "8"="3"))
```

Create new variable that indicates sum of missings:

``` r
data_long_min3_gstr_age <- data_long_min3_gstr_age %>%
  group_by(ID, Time) %>%
mutate(Nmiss = sum(is.na(Ang)) + sum(is.na(Anxiety)) + sum(is.na(Nerv)) + sum(is.na(Depr)) + sum(is.na(Exh)) + 
               sum(is.na(Energ)) + sum(is.na(Insp)) + sum(is.na(Calm)) + sum(is.na(Rel))) %>%
  ungroup()
```

save

``` r
save(data_long_min3_gstr_age, file="data_long_min3_gstr_age.Rdata")
```

``` r
data_means_g <- data_long_min3_gstr %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise_each(funs(mean(., na.rm=TRUE)), 
                          Ang, Anxiety, Calm, Depr, Energ, Exh, Insp, Nerv, Rel)
```

    ## Warning: `summarise_each_()` was deprecated in dplyr 0.7.0.
    ## Please use `across()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
save(data_means_g, file="data_means_g.Rdata")
```

Next = data imputation
