---
title: "EMD: Projekt z analizy danych"
author: "Zuzanna Kocur, Tomasz Supłat"
date: "30 November, 2019"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: yes
---



###  R Markdown

Zapewniono powtarzalność wyników oraz zaimportowano niezbędne biblioteki.


```r
library(knitr)
library(rmarkdown)
library(ggplot2)
library(corrplot)
```

```
## corrplot 0.84 loaded
```

```r
library(shiny)
library(caret)
```

```
## Loading required package: lattice
```

```r
set.seed(42)
```
### Wczytanie i wyczyszczenie danych

W pliku z danymi, brakujące informacje zastąpiono znakiem '?', który przy wczytaniu zastąpiono symbolem **NA**, który reprezentuje w języku *R* brakujące wartości. Wiersze z brakującymi wartościami usunięto i pozostało około 80% wszystkich obserwacji.


```r
data <- read.csv('sledzie.csv', na.strings = '?')
len_all <- nrow(data)
data_clean <- na.omit(data)
len_not_na <- nrow(data_clean)
print(paste0("Liczba wszystkich obserwacji: ", len_all))
```

```
## [1] "Liczba wszystkich obserwacji: 52582"
```

```r
print(paste0("Liczba obserwacji bez brakujacyhc wartości: ", len_not_na))
```

```
## [1] "Liczba obserwacji bez brakujacyhc wartości: 42488"
```

```r
knitr::kable(summary(data_clean))
```

           X             length         cfin1             cfin2             chel1            chel2            lcop1              lcop2             fbar             recr              cumf             totaln             sst             sal            xmonth            nao         
---  --------------  -------------  ----------------  ----------------  ---------------  ---------------  -----------------  ---------------  ---------------  ----------------  ----------------  ----------------  --------------  --------------  ---------------  -----------------
     Min.   :    1   Min.   :19.0   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000   Min.   : 5.238   Min.   :  0.3074   Min.   : 7.849   Min.   :0.0680   Min.   : 140515   Min.   :0.06833   Min.   : 144137   Min.   :12.77   Min.   :35.40   Min.   : 1.000   Min.   :-4.89000 
     1st Qu.:13233   1st Qu.:24.0   1st Qu.: 0.0000   1st Qu.: 0.2778   1st Qu.: 2.469   1st Qu.:13.427   1st Qu.:  2.5479   1st Qu.:17.808   1st Qu.:0.2270   1st Qu.: 360061   1st Qu.:0.14809   1st Qu.: 306068   1st Qu.:13.60   1st Qu.:35.51   1st Qu.: 5.000   1st Qu.:-1.90000 
     Median :26308   Median :25.5   Median : 0.1111   Median : 0.7012   Median : 5.750   Median :21.435   Median :  7.0000   Median :24.859   Median :0.3320   Median : 421391   Median :0.23191   Median : 539558   Median :13.86   Median :35.51   Median : 8.000   Median : 0.20000 
     Mean   :26316   Mean   :25.3   Mean   : 0.4457   Mean   : 2.0269   Mean   :10.016   Mean   :21.197   Mean   : 12.8386   Mean   :28.396   Mean   :0.3306   Mean   : 519877   Mean   :0.22987   Mean   : 515082   Mean   :13.87   Mean   :35.51   Mean   : 7.252   Mean   :-0.09642 
     3rd Qu.:39447   3rd Qu.:26.5   3rd Qu.: 0.3333   3rd Qu.: 1.7936   3rd Qu.:11.500   3rd Qu.:27.193   3rd Qu.: 21.2315   3rd Qu.:37.232   3rd Qu.:0.4650   3rd Qu.: 724151   3rd Qu.:0.29803   3rd Qu.: 730351   3rd Qu.:14.16   3rd Qu.:35.52   3rd Qu.: 9.000   3rd Qu.: 1.63000 
     Max.   :52580   Max.   :32.5   Max.   :37.6667   Max.   :19.3958   Max.   :75.000   Max.   :57.706   Max.   :115.5833   Max.   :68.736   Max.   :0.8490   Max.   :1565890   Max.   :0.39801   Max.   :1015595   Max.   :14.73   Max.   :35.61   Max.   :12.000   Max.   : 5.08000 

### Szczegółowa analiza wartości atrybutów

Dla katego atrybutu dokonano analizy na podstawie histogramów przedstawionych poniej.

![](projekt_sledzie_raport_files/figure-html/attrAnalysis-1.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-2.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-3.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-4.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-5.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-6.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-7.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-8.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-9.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-10.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-11.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-12.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-13.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-14.png)<!-- -->

## Korelacje atrybutów
A tutaj macierz korelacji. Ladna.

![](projekt_sledzie_raport_files/figure-html/correlation-1.png)<!-- -->

###  Animowane sledzie!
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

<!--html_preserve--><div style="width: 100% ; height: 500px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div><!--/html_preserve-->

## Regres osobisty


```r
train_indices <- createDataPartition(data_clean$length, p=0.9, list = FALSE, times=1)
test_indices <- -train_indices

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model <- train(length ~ ., data = data_clean[train_indices,], method="ridge", trControl=fitControl, preProcess = c('scale', 'center'))
```
