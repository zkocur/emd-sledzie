---
title: "EMD: Projekt z analizy danych"
author: "Zuzanna Kocur, Tomasz Supłat"
date: "06 grudnia, 2019"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: yes
---



###  Wstępne ustawienia

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
print(paste0("Liczba obserwacji bez brakujacyhc wartości: ", len_not_na))
knitr::kable(summary(data_clean))
```

```
## [1] "Liczba wszystkich obserwacji: 52582"
## [1] "Liczba obserwacji bez brakujacyhc wartości: 42488"
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

Dla katego atrybutu dokonano analizy na podstawie histogramów przedstawionych ponizej. Dostępność planktonu *Calanus helgolandicus gat. 2* oraz *widłonogów gat. 2* nie przypomina konkretnego rozkladu. Wykresy dla pozostałych planktonów mają wysokie słupki w okolicach zera i długi ogon. Dla niektórych z nich, obserwacji odległych od zera było tak niewiele, ze w ogóle nie widać odpowiadających im słupków.

Natężenie połowów w regionie (**fbar**) i roczny narybek (**recr**) mają zblione rozkłady. Łączne roczne natężenie połowów w regionie (**cumf**) i łączna liczba ryb złowionych w ramach połowu (**totaln**) mają losowe rozkłady, a temperatura przy powierzchni wody (**sst**) oraz poziom zasolenia wody (**sal**) przypominają rozkłady normalne. Rozkład normalny przyjął także atrybut **xmonth**, oznaczający miesiąc połowu, z którego można odczytać że najwięcej obserwacji w zbiorze to obserwacje z cieplejszych miesięcy (lipiec, sierpień, październik). Ostatni atrybut, oscylacja północnoatlantycka (**nao**) przypomina rozkład dwumodalny.

![](projekt_sledzie_raport_files/figure-html/attrAnalysis-1.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-2.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-3.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-4.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-5.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-6.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-7.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-8.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-9.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-10.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-11.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-12.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-13.png)<!-- -->![](projekt_sledzie_raport_files/figure-html/attrAnalysis-14.png)<!-- -->

## Korelacje atrybutów
Przeanalizujmy teraz czy parametry wejsciowe sa ze soba skorelowane.

![](projekt_sledzie_raport_files/figure-html/correlation-1.png)<!-- -->

Jak widac, niektore parametry sa ze soba silnie skorelowane:

* 'lcop1' oraz 'chel1'
* 'lcop2' oraz 'chel2'
* 'lcop2' oraz 'cfin2'
* 'fbar' oraz 'cumf'
* 'totaln' oraz 'cumf'

Parametry 'lcop', 'chel' oraz 'cfin' odpowiadaja poszczegolnym gatunkom planktonu. Prawdopodobnie maja one podobne wymagania co do temperatury i innych czynnikow srodowiskowych, przez co rozwijaja sie podobnie. Nie dziwi rowniez silna korelacja pomiedzy parametrami 'fbar', 'cumf' - oba opisuja natezenie polowow w regionie. Tak samo mozna wytlumaczyc silna korelacje pomiedzy 'totaln' i 'cumf' - odsetek zostawionych ryb i zlowionych ryb. Poniewaz takie parametry moga zaburzac analize danych, usuniemy czesc z nich: 'lcop1', 'lcop2', 'fbar', 'totaln'.


## Animowane sledzie
Interaktywny wykres przedstawiający zmianę rozmiaru śledzi w czasie.

<!--html_preserve--><div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div><!--/html_preserve-->

## Regresja

Sprobujemy teraz odpowiedziec na pytanie co jest przyczyna zmian dlugosci sledzia. Do tego celu wytrenujemy model regresyjny.  Dane wejsciowe zostaly podzielone na zbior treningowy (90% danych) i testowy (10% danych). Dodatkowo do trenowania zastosujemy 5-krotna walidacje.


```r
train_indices <- createDataPartition(data_clean$length, p=0.9, list = FALSE, times=1)
test_indices <- -train_indices

train_set <- data.matrix(data_clean[train_indices, -1])
test_set <- data.matrix(data_clean[test_indices, -1])

cv <- trainControl(method='cv', number=5)
```

Na poczatek sprawdzimy dzialanie najprostszego modelu, regresji liniowej. W celu polepszenia wynikow, dane wejsciowe zostaly przeskalowane oraz wycentrowane.


```r
model <- train(length ~ cfin1+cfin2+chel1+chel2+recr+cumf+sst+sal+xmonth+nao, data = train_set, method="lm", preProcess = c('scale', 'center'), trControl=cv)
```

Nastepnie sprawdzmy jak nasz model radzi sobie na zbiorze testowym:


```r
predicted <- predict(model, test_set)
postResample(predicted, data_clean[test_indices, 2])
```

```
##      RMSE  Rsquared       MAE 
## 1.4150780 0.2460649 1.1383371
```

Na koniec zobaczmy jakie zmienne sa najwazniejsze dla naszego modelu:


```r
varImp(model)
```

```
## lm variable importance
## 
##        Overall
## sst    100.000
## chel1   33.213
## recr    30.034
## cfin1   20.692
## sal     14.543
## nao     12.547
## chel2    8.744
## cfin2    3.157
## xmonth   1.307
## cumf     0.000
```

Jak widzimy, najwazniejsza jest temperatura wody przy powierzchni.

Zobaczmy jak zachowaja sie inne modele. Przetestujemy teraz random forest. Poniewaz trenowanie na pelnym zestawie danych trwa zdecydowanie za dlugo, do trenowania zastosujemy 10% oryginalnego zbioru (wykorzystamy do tego zbior 'test_set'):


```r
model_rf <- train(length ~ ., data = test_set, method="rf", ntree=25, importance= TRUE, trControl=cv)
```

Zobaczmy teraz jak nasz model radzi sobie ze zbiorem testowym (tutaj pozostale 90% zbioru: 'train_set'):


```r
predicted <- predict(model_rf, train_set)
postResample(predicted, data_clean[train_indices, 2])
```

```
##      RMSE  Rsquared       MAE 
## 1.1774627 0.4923352 0.9283122
```

Na koniec zobaczmy ktore zmienne maja najwiekszy wplyw na wyniki:


```r
varImp(model_rf)
```

```
## rf variable importance
## 
##        Overall
## xmonth 100.000
## sst     63.187
## lcop1   34.999
## totaln  29.002
## lcop2   28.278
## recr    27.505
## fbar    19.506
## cfin2   16.615
## chel2   16.533
## chel1   14.716
## cumf    11.425
## cfin1    4.112
## sal      3.936
## nao      0.000
```

Co zaskakujace, model uznal zmienna 'xmonth' za najbardziej znaczaca przy przewidywaniu dlugosci sledzia. Nastepne sa parametry 'recr' oraz 'sst'. Poprzedni model rowniez uwglednial te zmienne. Mozemy 
