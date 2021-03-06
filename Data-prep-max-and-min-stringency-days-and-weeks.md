Data prep max and min stringency days and weeks
================
Anne Margit
05/02/2022

``` r
load("data_imputed_emomeans.Rdata")
```

``` r
library(pracma)
library(dplyr)
library(tidyverse)
library(anytime)
library(lubridate)
```

Order by Stringency and Date and select rows with first day of maximum
Stringency for each country

``` r
data_imputed_emomeans <- as_tibble(data_imputed_emomeans)

data_imputed_emomeans$Date <- as.Date(data_imputed_emomeans$Date)

data_imputed_emomeans$Country <- as.factor(data_imputed_emomeans$Country)

data_imputed_emomeans <- data_imputed_emomeans[with(data_imputed_emomeans, order(StringencyIndex, Date)),]

MaxStr <- data_imputed_emomeans %>% 
dplyr::group_by(Country) %>%
arrange(desc(c(StringencyIndex))) %>%
slice(1) %>%
ungroup()
```

Rename, select columns and merge with original data

``` r
MaxStr$MaxStr <- MaxStr$StringencyIndex

MaxStr <- as_tibble(MaxStr)

MaxStr2 <- MaxStr %>%
dplyr::mutate(DateMaxStr = date(Date))

MaxStr2 <- MaxStr2 %>%
dplyr::select(c("ID", "MaxStr", "Date", "DateMaxStr"))

data2 <- left_join(data_imputed_emomeans, MaxStr2, by=c("ID", "Date"))
```

Fill other rows of NewDate with date on which max Stringency was reached
for respective country

``` r
data3 <- data2 %>%
group_by(Country) %>%
fill(DateMaxStr, .direction = "downup")
```

Fill other rows with max stringency

``` r
data3 <- data3 %>%
group_by(Country) %>%
fill(MaxStr, .direction = "downup")
```

Calculate difference in DAYS between date of maximum Stringency and
current date

``` r
data4 <- data3 %>%
group_by(Country) %>%
arrange(Date) %>%
mutate(DaysMax = round(difftime(Date, DateMaxStr, units = "days"), digits=0))
```

Calculate difference in WEEKS between date of maximum Stringency and
current date

``` r
data5 <- data4 %>%
group_by(Country) %>%
arrange(Date) %>%
mutate(WeeksMax = round(difftime(Date, DateMaxStr, units = "weeks"), digits=0))
```

NEW: Add week number

``` r
data_imputed_emomeans_maxweeks <- data5 %>%
  mutate(Week = week(Date))
```

This file contains the new X axis on which week of maximum Stringency of
the country is coded as 0

``` r
data_imputed_emomeans_maxweeks$WeeksMax <- as.numeric(data_imputed_emomeans_maxweeks$WeeksMax)

save(data_imputed_emomeans_maxweeks, file = "data_imputed_emomeans_maxweeks.Rdata")
```

NEW: Add minimum Stringency Index

``` r
data_imputed_emomeans_maxweeks <- data_imputed_emomeans_maxweeks[with(data_imputed_emomeans_maxweeks, order(StringencyIndex, Date)),]

MinStr <- data_imputed_emomeans_maxweeks %>% 
group_by(Country) %>%
arrange(StringencyIndex) %>%
slice(1) %>%
ungroup()
```

``` r
MinStr$MinStr <- MinStr$StringencyIndex

MinStr2 <- MinStr %>%
dplyr::mutate(DateMinStr = date(Date))

MinStr2 <- MinStr2 %>%
dplyr::select(c("ID", "MinStr", "Date", "DateMinStr"))

data5 <- left_join(data_imputed_emomeans_maxweeks, MinStr2, by=c("ID", "Date"))
```

Fill other rows of NewDate with date on which min Stringency was reached
for respective country

``` r
data6 <- data5 %>%
group_by(Country) %>%
fill(DateMinStr, .direction = "downup")
```

Fill other rows with min stringency

``` r
data_imputed_emomeans_maxmin <- data6 %>%
group_by(Country) %>%
fill(MinStr, .direction = "downup")
```

This file contains the new X axis on which week of maximum Stringency of
the country is coded as 0

``` r
save(data_imputed_emomeans_maxmin, file = "data_imputed_emomeans_maxmin.Rdata")
```
