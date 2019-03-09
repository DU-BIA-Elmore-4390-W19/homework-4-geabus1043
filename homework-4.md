Homework 4: Bags, Forests, Boosts, oh my
================
Jason Carlson
2/28/2019

``` r
library(e1071)
library(gbm)
```

    ## Loaded gbm 2.1.5

``` r
library(MASS)
library(tidyverse)
```

    ## -- Attaching packages ---- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x dplyr::select() masks MASS::select()

``` r
library(broom)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(ISLR)
library(janitor)
library(plotROC)
library(kernlab)
```

    ## 
    ## Attaching package: 'kernlab'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     alpha

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loaded glmnet 2.0-16

``` r
library(stringr)
library(rpart)
library(rpart.plot)
library(partykit) 
```

    ## Loading required package: grid

    ## Loading required package: libcoin

    ## Loading required package: mvtnorm

``` r
library(tree)
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
theme_set(theme_bw())
```

Problem 1
---------

Problem 7 from Chapter 8 in the text. To be specific, please use a sequence of `ntree` from 25 to 500 in steps of 25 and `mtry` from 3 to 9 for by 1.

Answer 1
--------

``` r
set.seed(1234)
df <- tbl_df(Boston)

for (k in 1:20){
  inTraining <- createDataPartition(df$medv, p = .75, list = F)
  training <- df[inTraining, ]
  testing <- df[-inTraining, ]
  mtry <- c(3:9)
  ntree <- seq(25, 500, len = 20)
  results <- tibble(trial = rep(NA, 140),
  mtry = rep(NA, 140),
  ntree = rep(NA, 140),
  mse = rep(NA, 140)) 
  for(i in 1:7){
    cat(sprintf('Trial: %s, mtry: %s --- %s\n', k, mtry[i], Sys.time()))
    for(j in 1:20){ 
      rf_train <- randomForest(medv ~ .,
                               data = training,
                               mtry = mtry[i],
                               ntree = ntree[j])
      mse <- mean((predict(rf_train, newdata = testing) - testing$medv)^2)
      results[(i-1)*20 + j, ] <- c(k, mtry[i], ntree[j], mse)
    }
  }
  if(exists("results_total")){
  results_total <- bind_rows(results_total, results)
  }
  else(
  results_total <- results
  )
}
```

    ## Trial: 1, mtry: 3 --- 2019-03-09 14:41:52
    ## Trial: 1, mtry: 4 --- 2019-03-09 14:41:55
    ## Trial: 1, mtry: 5 --- 2019-03-09 14:41:59
    ## Trial: 1, mtry: 6 --- 2019-03-09 14:42:03
    ## Trial: 1, mtry: 7 --- 2019-03-09 14:42:09
    ## Trial: 1, mtry: 8 --- 2019-03-09 14:42:14
    ## Trial: 1, mtry: 9 --- 2019-03-09 14:42:21
    ## Trial: 2, mtry: 3 --- 2019-03-09 14:42:28
    ## Trial: 2, mtry: 4 --- 2019-03-09 14:42:31
    ## Trial: 2, mtry: 5 --- 2019-03-09 14:42:35
    ## Trial: 2, mtry: 6 --- 2019-03-09 14:42:40
    ## Trial: 2, mtry: 7 --- 2019-03-09 14:42:45
    ## Trial: 2, mtry: 8 --- 2019-03-09 14:42:51
    ## Trial: 2, mtry: 9 --- 2019-03-09 14:42:58
    ## Trial: 3, mtry: 3 --- 2019-03-09 14:43:05
    ## Trial: 3, mtry: 4 --- 2019-03-09 14:43:08
    ## Trial: 3, mtry: 5 --- 2019-03-09 14:43:12
    ## Trial: 3, mtry: 6 --- 2019-03-09 14:43:16
    ## Trial: 3, mtry: 7 --- 2019-03-09 14:43:21
    ## Trial: 3, mtry: 8 --- 2019-03-09 14:43:27
    ## Trial: 3, mtry: 9 --- 2019-03-09 14:43:34
    ## Trial: 4, mtry: 3 --- 2019-03-09 14:43:41
    ## Trial: 4, mtry: 4 --- 2019-03-09 14:43:44
    ## Trial: 4, mtry: 5 --- 2019-03-09 14:43:48
    ## Trial: 4, mtry: 6 --- 2019-03-09 14:43:52
    ## Trial: 4, mtry: 7 --- 2019-03-09 14:43:57
    ## Trial: 4, mtry: 8 --- 2019-03-09 14:44:03
    ## Trial: 4, mtry: 9 --- 2019-03-09 14:44:10
    ## Trial: 5, mtry: 3 --- 2019-03-09 14:44:17
    ## Trial: 5, mtry: 4 --- 2019-03-09 14:44:20
    ## Trial: 5, mtry: 5 --- 2019-03-09 14:44:24
    ## Trial: 5, mtry: 6 --- 2019-03-09 14:44:29
    ## Trial: 5, mtry: 7 --- 2019-03-09 14:44:35
    ## Trial: 5, mtry: 8 --- 2019-03-09 14:44:41
    ## Trial: 5, mtry: 9 --- 2019-03-09 14:44:48
    ## Trial: 6, mtry: 3 --- 2019-03-09 14:44:55
    ## Trial: 6, mtry: 4 --- 2019-03-09 14:44:59
    ## Trial: 6, mtry: 5 --- 2019-03-09 14:45:03
    ## Trial: 6, mtry: 6 --- 2019-03-09 14:45:07
    ## Trial: 6, mtry: 7 --- 2019-03-09 14:45:12
    ## Trial: 6, mtry: 8 --- 2019-03-09 14:45:18
    ## Trial: 6, mtry: 9 --- 2019-03-09 14:45:24
    ## Trial: 7, mtry: 3 --- 2019-03-09 14:45:31
    ## Trial: 7, mtry: 4 --- 2019-03-09 14:45:34
    ## Trial: 7, mtry: 5 --- 2019-03-09 14:45:38
    ## Trial: 7, mtry: 6 --- 2019-03-09 14:45:42
    ## Trial: 7, mtry: 7 --- 2019-03-09 14:45:47
    ## Trial: 7, mtry: 8 --- 2019-03-09 14:45:52
    ## Trial: 7, mtry: 9 --- 2019-03-09 14:45:58
    ## Trial: 8, mtry: 3 --- 2019-03-09 14:46:05
    ## Trial: 8, mtry: 4 --- 2019-03-09 14:46:08
    ## Trial: 8, mtry: 5 --- 2019-03-09 14:46:11
    ## Trial: 8, mtry: 6 --- 2019-03-09 14:46:15
    ## Trial: 8, mtry: 7 --- 2019-03-09 14:46:20
    ## Trial: 8, mtry: 8 --- 2019-03-09 14:46:26
    ## Trial: 8, mtry: 9 --- 2019-03-09 14:46:32
    ## Trial: 9, mtry: 3 --- 2019-03-09 14:46:38
    ## Trial: 9, mtry: 4 --- 2019-03-09 14:46:41
    ## Trial: 9, mtry: 5 --- 2019-03-09 14:46:45
    ## Trial: 9, mtry: 6 --- 2019-03-09 14:46:49
    ## Trial: 9, mtry: 7 --- 2019-03-09 14:46:54
    ## Trial: 9, mtry: 8 --- 2019-03-09 14:46:59
    ## Trial: 9, mtry: 9 --- 2019-03-09 14:47:05
    ## Trial: 10, mtry: 3 --- 2019-03-09 14:47:12
    ## Trial: 10, mtry: 4 --- 2019-03-09 14:47:15
    ## Trial: 10, mtry: 5 --- 2019-03-09 14:47:18
    ## Trial: 10, mtry: 6 --- 2019-03-09 14:47:23
    ## Trial: 10, mtry: 7 --- 2019-03-09 14:47:28
    ## Trial: 10, mtry: 8 --- 2019-03-09 14:47:33
    ## Trial: 10, mtry: 9 --- 2019-03-09 14:47:39
    ## Trial: 11, mtry: 3 --- 2019-03-09 14:47:46
    ## Trial: 11, mtry: 4 --- 2019-03-09 14:47:49
    ## Trial: 11, mtry: 5 --- 2019-03-09 14:47:53
    ## Trial: 11, mtry: 6 --- 2019-03-09 14:47:58
    ## Trial: 11, mtry: 7 --- 2019-03-09 14:48:03
    ## Trial: 11, mtry: 8 --- 2019-03-09 14:48:09
    ## Trial: 11, mtry: 9 --- 2019-03-09 14:48:16
    ## Trial: 12, mtry: 3 --- 2019-03-09 14:48:23
    ## Trial: 12, mtry: 4 --- 2019-03-09 14:48:26
    ## Trial: 12, mtry: 5 --- 2019-03-09 14:48:30
    ## Trial: 12, mtry: 6 --- 2019-03-09 14:48:34
    ## Trial: 12, mtry: 7 --- 2019-03-09 14:48:39
    ## Trial: 12, mtry: 8 --- 2019-03-09 14:48:45
    ## Trial: 12, mtry: 9 --- 2019-03-09 14:48:52
    ## Trial: 13, mtry: 3 --- 2019-03-09 14:48:59
    ## Trial: 13, mtry: 4 --- 2019-03-09 14:49:02
    ## Trial: 13, mtry: 5 --- 2019-03-09 14:49:06
    ## Trial: 13, mtry: 6 --- 2019-03-09 14:49:10
    ## Trial: 13, mtry: 7 --- 2019-03-09 14:49:15
    ## Trial: 13, mtry: 8 --- 2019-03-09 14:49:21
    ## Trial: 13, mtry: 9 --- 2019-03-09 14:49:27
    ## Trial: 14, mtry: 3 --- 2019-03-09 14:49:34
    ## Trial: 14, mtry: 4 --- 2019-03-09 14:49:37
    ## Trial: 14, mtry: 5 --- 2019-03-09 14:49:41
    ## Trial: 14, mtry: 6 --- 2019-03-09 14:49:45
    ## Trial: 14, mtry: 7 --- 2019-03-09 14:49:50
    ## Trial: 14, mtry: 8 --- 2019-03-09 14:49:56
    ## Trial: 14, mtry: 9 --- 2019-03-09 14:50:03
    ## Trial: 15, mtry: 3 --- 2019-03-09 14:50:09
    ## Trial: 15, mtry: 4 --- 2019-03-09 14:50:12
    ## Trial: 15, mtry: 5 --- 2019-03-09 14:50:16
    ## Trial: 15, mtry: 6 --- 2019-03-09 14:50:21
    ## Trial: 15, mtry: 7 --- 2019-03-09 14:50:26
    ## Trial: 15, mtry: 8 --- 2019-03-09 14:50:31
    ## Trial: 15, mtry: 9 --- 2019-03-09 14:50:38
    ## Trial: 16, mtry: 3 --- 2019-03-09 14:50:45
    ## Trial: 16, mtry: 4 --- 2019-03-09 14:50:48
    ## Trial: 16, mtry: 5 --- 2019-03-09 14:50:52
    ## Trial: 16, mtry: 6 --- 2019-03-09 14:50:56
    ## Trial: 16, mtry: 7 --- 2019-03-09 14:51:01
    ## Trial: 16, mtry: 8 --- 2019-03-09 14:51:08
    ## Trial: 16, mtry: 9 --- 2019-03-09 14:51:15
    ## Trial: 17, mtry: 3 --- 2019-03-09 14:51:22
    ## Trial: 17, mtry: 4 --- 2019-03-09 14:51:26
    ## Trial: 17, mtry: 5 --- 2019-03-09 14:51:30
    ## Trial: 17, mtry: 6 --- 2019-03-09 14:51:35
    ## Trial: 17, mtry: 7 --- 2019-03-09 14:51:40
    ## Trial: 17, mtry: 8 --- 2019-03-09 14:51:45
    ## Trial: 17, mtry: 9 --- 2019-03-09 14:51:52
    ## Trial: 18, mtry: 3 --- 2019-03-09 14:51:58
    ## Trial: 18, mtry: 4 --- 2019-03-09 14:52:01
    ## Trial: 18, mtry: 5 --- 2019-03-09 14:52:05
    ## Trial: 18, mtry: 6 --- 2019-03-09 14:52:10
    ## Trial: 18, mtry: 7 --- 2019-03-09 14:52:14
    ## Trial: 18, mtry: 8 --- 2019-03-09 14:52:20
    ## Trial: 18, mtry: 9 --- 2019-03-09 14:52:27
    ## Trial: 19, mtry: 3 --- 2019-03-09 14:52:34
    ## Trial: 19, mtry: 4 --- 2019-03-09 14:52:37
    ## Trial: 19, mtry: 5 --- 2019-03-09 14:52:41
    ## Trial: 19, mtry: 6 --- 2019-03-09 14:52:45
    ## Trial: 19, mtry: 7 --- 2019-03-09 14:52:50
    ## Trial: 19, mtry: 8 --- 2019-03-09 14:52:56
    ## Trial: 19, mtry: 9 --- 2019-03-09 14:53:02
    ## Trial: 20, mtry: 3 --- 2019-03-09 14:53:09
    ## Trial: 20, mtry: 4 --- 2019-03-09 14:53:12
    ## Trial: 20, mtry: 5 --- 2019-03-09 14:53:16
    ## Trial: 20, mtry: 6 --- 2019-03-09 14:53:21
    ## Trial: 20, mtry: 7 --- 2019-03-09 14:53:26
    ## Trial: 20, mtry: 8 --- 2019-03-09 14:53:31
    ## Trial: 20, mtry: 9 --- 2019-03-09 14:53:38

Problem 2
---------

I Dont remember some of this stuff (like the tune() stuff) so I got a lot from the internet.
--------------------------------------------------------------------------------------------

Problem 8 from Chapter 8 in the text. Set your seed with 9823 and split into train/test using 50% of your data in each split. In addition to parts (a) - (e), do the following: \#A

``` r
inTraining <- sample(nrow(OJ), 800)
training <- OJ[inTraining, ]
testing <- OJ[-inTraining, ]
```

B
=

``` r
purchase.svm <- svm(Purchase ~ ., data = training, kernel = "linear", cost = 0.01)
summary(purchase.svm)
```

    ## 
    ## Call:
    ## svm(formula = Purchase ~ ., data = training, kernel = "linear", 
    ##     cost = 0.01)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  linear 
    ##        cost:  0.01 
    ##       gamma:  0.05555556 
    ## 
    ## Number of Support Vectors:  443
    ## 
    ##  ( 220 223 )
    ## 
    ## 
    ## Number of Classes:  2 
    ## 
    ## Levels: 
    ##  CH MM

C
=

``` r
purchase.train.pred<- predict(purchase.svm, training)
table(training$Purchase, purchase.train.pred)
```

    ##     purchase.train.pred
    ##       CH  MM
    ##   CH 433  53
    ##   MM  85 229

``` r
(74+57)/(436+57+74+233)
```

    ## [1] 0.16375

``` r
purchase.test.pred<- predict(purchase.svm, testing)
table(testing$Purchase, purchase.test.pred)
```

    ##     purchase.test.pred
    ##       CH  MM
    ##   CH 150  17
    ##   MM  24  79

``` r
(24+26)/(134+26+24+86)
```

    ## [1] 0.1851852

D
=

``` r
purchase.tune <- tune(svm, Purchase ~ ., data = training, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(purchase.tune)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##      cost
    ##  5.623413
    ## 
    ## - best performance: 0.17 
    ## 
    ## - Detailed performance results:
    ##           cost   error dispersion
    ## 1   0.01000000 0.17625 0.04945888
    ## 2   0.01778279 0.18250 0.05109903
    ## 3   0.03162278 0.17500 0.04677072
    ## 4   0.05623413 0.17625 0.04910660
    ## 5   0.10000000 0.17625 0.04730589
    ## 6   0.17782794 0.18000 0.05006940
    ## 7   0.31622777 0.17875 0.04825065
    ## 8   0.56234133 0.18250 0.05177408
    ## 9   1.00000000 0.18000 0.05143766
    ## 10  1.77827941 0.17750 0.05062114
    ## 11  3.16227766 0.17375 0.05350558
    ## 12  5.62341325 0.17000 0.04901814
    ## 13 10.00000000 0.17375 0.04945888

E
=

``` r
best.svc.train <- svm(Purchase ~ ., kernel = "linear", data = training, cost = purchase.tune$best.parameter$cost)
purchase.train.pred <- predict(best.svc.train, training)
confusionMatrix(table(training$Purchase, purchase.train.pred),positive = "CH")
```

    ## Confusion Matrix and Statistics
    ## 
    ##     purchase.train.pred
    ##       CH  MM
    ##   CH 431  55
    ##   MM  80 234
    ##                                           
    ##                Accuracy : 0.8312          
    ##                  95% CI : (0.8035, 0.8566)
    ##     No Information Rate : 0.6388          
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.6411          
    ##  Mcnemar's Test P-Value : 0.03887         
    ##                                           
    ##             Sensitivity : 0.8434          
    ##             Specificity : 0.8097          
    ##          Pos Pred Value : 0.8868          
    ##          Neg Pred Value : 0.7452          
    ##              Prevalence : 0.6388          
    ##          Detection Rate : 0.5387          
    ##    Detection Prevalence : 0.6075          
    ##       Balanced Accuracy : 0.8266          
    ##                                           
    ##        'Positive' Class : CH              
    ## 

``` r
(75+52)/(441+52+75+232)
```

    ## [1] 0.15875

``` r
best.svc.test <- svm(Purchase ~ ., kernel = "linear", data = testing, cost = purchase.tune$best.parameter$cost)
purchase.test.pred <- predict(best.svc.test, testing)
table(testing$Purchase, purchase.test.pred)
```

    ##     purchase.test.pred
    ##       CH  MM
    ##   CH 148  19
    ##   MM  22  81

``` r
confusionMatrix(table(testing$Purchase, purchase.test.pred),positive = "CH")
```

    ## Confusion Matrix and Statistics
    ## 
    ##     purchase.test.pred
    ##       CH  MM
    ##   CH 148  19
    ##   MM  22  81
    ##                                           
    ##                Accuracy : 0.8481          
    ##                  95% CI : (0.7997, 0.8888)
    ##     No Information Rate : 0.6296          
    ##     P-Value [Acc > NIR] : 1.699e-15       
    ##                                           
    ##                   Kappa : 0.6764          
    ##  Mcnemar's Test P-Value : 0.7548          
    ##                                           
    ##             Sensitivity : 0.8706          
    ##             Specificity : 0.8100          
    ##          Pos Pred Value : 0.8862          
    ##          Neg Pred Value : 0.7864          
    ##              Prevalence : 0.6296          
    ##          Detection Rate : 0.5481          
    ##    Detection Prevalence : 0.6185          
    ##       Balanced Accuracy : 0.8403          
    ##                                           
    ##        'Positive' Class : CH              
    ## 

``` r
(24+26)/(134+26+24+86)
```

    ## [1] 0.1851852

1.  Fit a gradient-boosted tree to the training data and report the estimated test MSE.

``` r
Purchase.boost<-gbm(Purchase ~ . ,data = training,distribution = "gaussian",n.trees = 10000,
                  shrinkage = 0.01, interaction.depth = 4)
purchase_boost_tree <- rpart::rpart(Purchase ~ . , 
                      data = training,
                      control = rpart.control(minsplit = 50))
purchase_boost_tree
```

    ## n= 800 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##  1) root 800 314 CH (0.60750000 0.39250000)  
    ##    2) LoyalCH>=0.4824895 499  82 CH (0.83567134 0.16432866)  
    ##      4) LoyalCH>=0.7645725 255  12 CH (0.95294118 0.04705882) *
    ##      5) LoyalCH< 0.7645725 244  70 CH (0.71311475 0.28688525)  
    ##       10) PriceDiff>=0.085 161  26 CH (0.83850932 0.16149068) *
    ##       11) PriceDiff< 0.085 83  39 MM (0.46987952 0.53012048)  
    ##         22) ListPriceDiff>=0.235 27   8 CH (0.70370370 0.29629630) *
    ##         23) ListPriceDiff< 0.235 56  20 MM (0.35714286 0.64285714) *
    ##    3) LoyalCH< 0.4824895 301  69 MM (0.22923588 0.77076412)  
    ##      6) LoyalCH>=0.2642325 145  52 MM (0.35862069 0.64137931)  
    ##       12) SalePriceMM>=2.04 61  28 CH (0.54098361 0.45901639)  
    ##         24) PriceCH< 1.94 44  17 CH (0.61363636 0.38636364) *
    ##         25) PriceCH>=1.94 17   6 MM (0.35294118 0.64705882) *
    ##       13) SalePriceMM< 2.04 84  19 MM (0.22619048 0.77380952) *
    ##      7) LoyalCH< 0.2642325 156  17 MM (0.10897436 0.89102564) *

1.  Fit a multiple regression model to the training data and report the estimated test MSE

A little confused here. What are we applying regression model to? We have been focusing on Purchase but that is classification.
===============================================================================================================================

``` r
#Trying to debug convergence issue, but im most likely just doing this wrong
training$Purchase=training$Purchase=="CH"
#training[,c('SalePriceMM', 'SalePriceCH' ,'PriceDiff', 'Store7', 'ListPriceDiff')]= list(NULL)

glm(Purchase ~ . , data = training,family="binomial")
```

    ## 
    ## Call:  glm(formula = Purchase ~ ., family = "binomial", data = training)
    ## 
    ## Coefficients:
    ##    (Intercept)  WeekofPurchase         StoreID         PriceCH  
    ##       -5.72029         0.01300         0.09060        -3.36898  
    ##        PriceMM          DiscCH          DiscMM       SpecialCH  
    ##        2.76378       -12.03264       -25.82861        -0.01511  
    ##      SpecialMM         LoyalCH     SalePriceMM     SalePriceCH  
    ##       -0.36260         6.17093              NA              NA  
    ##      PriceDiff       Store7Yes       PctDiscMM       PctDiscCH  
    ##             NA         0.09893        49.38537        28.29792  
    ##  ListPriceDiff           STORE  
    ##             NA              NA  
    ## 
    ## Degrees of Freedom: 799 Total (i.e. Null);  787 Residual
    ## Null Deviance:       1072 
    ## Residual Deviance: 624.9     AIC: 650.9

``` r
# fit_control <- trainControl(method = "repeatedcv",
#                            number = 10, 
#                            repeats = 10)
# 
# 
# purch_mult <- train(Purchase ~ .,
#                           data = training,
#                           method = "svmPoly",
#                           trControl = fit_control,
#                           tuneGrid = expand.grid(degree = 2:4,
#                                                  scale = c(.001, .01, .1), 
#                                                  C = 2:8))
# purch.mult.preds<-predict(purch_mult, newdata = testing)
# confusionMatrix(table(purch.mult.preds, testing$Purchase), 
#                 positive = "CH")
```

1.  Summarize your results.
