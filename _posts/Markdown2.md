---
title: "R Notebook"
output:
  html_document:
    keep_md: true
    df_print: paged
---



```r
setwd("D:/Audrey")
library(readr)
Dacobs <- read_delim("Dacobs_Audrey_07-05-2020.csv",";", escape_double = FALSE, trim_ws = TRUE)
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   id = col_character(),
##   Date_Birth = col_datetime(format = ""),
##   DEM_18 = col_character(),
##   DEMUNITS = col_character(),
##   DEM_UNITS_H = col_character(),
##   DEM_UNITS_W = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
#colnames(Dacobs)
#str(Dacobs)
```


```r
###########CONCURRENT VALIDITY##############################################
cor.test(Dacobs$Dacobs_Jumping_Conclusions, Dacobs$CBQp_Jumping_Conclusions)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Jumping_Conclusions and Dacobs$CBQp_Jumping_Conclusions
## t = 0.68356, df = 170, p-value = 0.4952
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.09804742  0.20041942
## sample estimates:
##        cor 
## 0.05235511
```

```r
summary(lm(Dacobs$Dacobs_Jumping_Conclusions~Dacobs$CBQp_Jumping_Conclusions+Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Jumping_Conclusions ~ Dacobs$CBQp_Jumping_Conclusions + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.4132  -3.6872   0.0387   3.7443  12.7097 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      30.7745     5.9328   5.187 6.27e-07 ***
## Dacobs$CBQp_Jumping_Conclusions   0.1370     0.2344   0.585    0.560    
## Dacobs$Age                       -0.4660     0.2574  -1.811    0.072 .  
## Dacobs$DEM_01                     1.4514     1.3332   1.089    0.278    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.396 on 163 degrees of freedom
##   (49 observations deleted due to missingness)
## Multiple R-squared:  0.02709,	Adjusted R-squared:  0.009186 
## F-statistic: 1.513 on 3 and 163 DF,  p-value: 0.2131
```

```r
cor.test(Dacobs$Dacobs_Belief_Inflexibility,Dacobs$BCIS_Self_Certainty)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Belief_Inflexibility and Dacobs$BCIS_Self_Certainty
## t = 3.3365, df = 148, p-value = 0.001073
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.1088446 0.4074816
## sample estimates:
##       cor 
## 0.2644924
```

```r
summary(lm(Dacobs$Dacobs_Belief_Inflexibility~Dacobs$BCIS_Self_Certainty+Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Belief_Inflexibility ~ Dacobs$BCIS_Self_Certainty + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.1260 -2.6770 -0.3119  2.7587 13.6171 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 19.5599     4.5590   4.290 3.29e-05 ***
## Dacobs$BCIS_Self_Certainty   0.3885     0.1256   3.093  0.00239 ** 
## Dacobs$Age                  -0.3413     0.2155  -1.584  0.11545    
## Dacobs$DEM_01               -0.9379     1.0426  -0.900  0.36985    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.123 on 141 degrees of freedom
##   (71 observations deleted due to missingness)
## Multiple R-squared:  0.09078,	Adjusted R-squared:  0.07144 
## F-statistic: 4.693 on 3 and 141 DF,  p-value: 0.00373
```

```r
cor.test(Dacobs$Dacobs_Attention_Threats,Dacobs$LSAS_Social_Anx)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Attention_Threats and Dacobs$LSAS_Social_Anx
## t = 1.2937, df = 157, p-value = 0.1977
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.0538042  0.2542854
## sample estimates:
##       cor 
## 0.1027031
```

```r
summary(lm(Dacobs$Dacobs_Attention_Threats~Dacobs$LSAS_Social_Anx+Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Attention_Threats ~ Dacobs$LSAS_Social_Anx + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.1777  -4.1474  -0.0068   4.0960  14.1931 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            34.37918    6.16464   5.577 1.11e-07 ***
## Dacobs$LSAS_Social_Anx  0.08457    0.06633   1.275   0.2043    
## Dacobs$Age             -0.54391    0.29595  -1.838   0.0681 .  
## Dacobs$DEM_01           1.54794    1.53973   1.005   0.3164    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.091 on 150 degrees of freedom
##   (62 observations deleted due to missingness)
## Multiple R-squared:  0.03529,	Adjusted R-squared:  0.016 
## F-statistic: 1.829 on 3 and 150 DF,  p-value: 0.1443
```

```r
cor.test(Dacobs$Dacobs_Social_Cognition_Prob ,Dacobs$SSTICS_ScoreTotal)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Social_Cognition_Prob and Dacobs$SSTICS_ScoreTotal
## t = 6.9587, df = 152, p-value = 9.602e-11
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3614743 0.6028294
## sample estimates:
##       cor 
## 0.4915335
```

```r
summary(lm(Dacobs$Dacobs_Social_Cognition_Prob ~Dacobs$SSTICS_ScoreTotal +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Social_Cognition_Prob ~ Dacobs$SSTICS_ScoreTotal + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.3088  -4.9601   0.2585   4.0403  16.8167 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              21.01286    6.20007   3.389 0.000903 ***
## Dacobs$SSTICS_ScoreTotal  0.27508    0.03844   7.155 3.83e-11 ***
## Dacobs$Age               -0.34912    0.30021  -1.163 0.246770    
## Dacobs$DEM_01            -1.64283    1.48054  -1.110 0.269001    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.866 on 145 degrees of freedom
##   (67 observations deleted due to missingness)
## Multiple R-squared:  0.2711,	Adjusted R-squared:  0.256 
## F-statistic: 17.98 on 3 and 145 DF,  p-value: 5.656e-10
```

```r
cor.test(Dacobs$Dacobs_Subjective_Cognition_Prob  ,Dacobs$ASCO_ScoreTotal )
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Subjective_Cognition_Prob and Dacobs$ASCO_ScoreTotal
## t = 7.1515, df = 153, p-value = 3.313e-11
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3722537 0.6100399
## sample estimates:
##       cor 
## 0.5005283
```

```r
summary(lm(Dacobs$Dacobs_Subjective_Cognition_Prob  ~Dacobs$ASCO_ScoreTotal +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Subjective_Cognition_Prob ~ Dacobs$ASCO_ScoreTotal + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9537  -3.7343  -0.4136   3.4610  13.7507 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            23.47164    5.17034   4.540 1.17e-05 ***
## Dacobs$ASCO_ScoreTotal  0.45868    0.06195   7.404 9.68e-12 ***
## Dacobs$Age             -0.49898    0.25330  -1.970   0.0507 .  
## Dacobs$DEM_01          -1.87728    1.24422  -1.509   0.1335    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.933 on 146 degrees of freedom
##   (66 observations deleted due to missingness)
## Multiple R-squared:  0.2868,	Adjusted R-squared:  0.2721 
## F-statistic: 19.57 on 3 and 146 DF,  p-value: 1.02e-10
```

```r
cor.test(Dacobs$Dacobs_External_Attribution   ,Dacobs$IPSAQ_Negative_IPSAQ)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_External_Attribution and Dacobs$IPSAQ_Negative_IPSAQ
## t = -2.9166, df = 161, p-value = 0.004044
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.36516363 -0.07280458
## sample estimates:
##       cor 
## -0.224018
```

```r
summary(lm(Dacobs$Dacobs_External_Attribution  ~Dacobs$IPSAQ_Negative_IPSAQ  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_External_Attribution ~ Dacobs$IPSAQ_Negative_IPSAQ + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.810 -3.837 -1.087  3.699 13.764 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  33.3837     5.3607   6.227 4.31e-09 ***
## Dacobs$IPSAQ_Negative_IPSAQ  -0.2590     0.1047  -2.473  0.01448 *  
## Dacobs$Age                   -0.6951     0.2528  -2.750  0.00667 ** 
## Dacobs$DEM_01                -1.3674     1.3165  -1.039  0.30059    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.231 on 154 degrees of freedom
##   (58 observations deleted due to missingness)
## Multiple R-squared:  0.09023,	Adjusted R-squared:  0.07251 
## F-statistic: 5.091 on 3 and 154 DF,  p-value: 0.002182
```

```r
cor.test(Dacobs$Dacobs_External_Attribution   ,Dacobs$IPSAQ_Negative_Personal)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_External_Attribution and Dacobs$IPSAQ_Negative_Personal
## t = -0.44125, df = 161, p-value = 0.6596
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1874733  0.1196051
## sample estimates:
##         cor 
## -0.03475439
```

```r
summary(lm(Dacobs$Dacobs_External_Attribution  ~Dacobs$IPSAQ_Negative_Personal  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_External_Attribution ~ Dacobs$IPSAQ_Negative_Personal + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.161 -3.692 -1.155  3.538 14.377 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    30.27215    5.37552   5.631 8.26e-08 ***
## Dacobs$IPSAQ_Negative_Personal -0.02143    0.10912  -0.196   0.8446    
## Dacobs$Age                     -0.67766    0.25764  -2.630   0.0094 ** 
## Dacobs$DEM_01                  -1.57066    1.33961  -1.172   0.2428    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.333 on 154 degrees of freedom
##   (58 observations deleted due to missingness)
## Multiple R-squared:  0.05434,	Adjusted R-squared:  0.03592 
## F-statistic:  2.95 on 3 and 154 DF,  p-value: 0.03461
```

```r
cor.test(Dacobs$Dacobs_External_Attribution   ,Dacobs$IPSAQ_Negative_Situational)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_External_Attribution and Dacobs$IPSAQ_Negative_Situational
## t = -3.2077, df = 161, p-value = 0.001614
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.3843338 -0.0949503
## sample estimates:
##        cor 
## -0.2450935
```

```r
summary(lm(Dacobs$Dacobs_External_Attribution  ~Dacobs$IPSAQ_Negative_Situational  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_External_Attribution ~ Dacobs$IPSAQ_Negative_Situational + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.9850 -4.0291 -0.5568  3.4847 14.0527 
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        31.7751     5.1781   6.136 6.84e-09 ***
## Dacobs$IPSAQ_Negative_Situational  -0.4042     0.1352  -2.990  0.00325 ** 
## Dacobs$Age                         -0.6832     0.2504  -2.728  0.00711 ** 
## Dacobs$DEM_01                      -1.2277     1.3075  -0.939  0.34920    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.185 on 154 degrees of freedom
##   (58 observations deleted due to missingness)
## Multiple R-squared:  0.106,	Adjusted R-squared:  0.08859 
## F-statistic: 6.087 on 3 and 154 DF,  p-value: 0.0006094
```

```r
cor.test(Dacobs$Dacobs_External_Attribution   ,Dacobs$IPSAQ_Negative_Internal)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_External_Attribution and Dacobs$IPSAQ_Negative_Internal
## t = 2.9166, df = 161, p-value = 0.004044
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.07280458 0.36516363
## sample estimates:
##      cor 
## 0.224018
```

```r
summary(lm(Dacobs$Dacobs_External_Attribution  ~Dacobs$IPSAQ_Negative_Internal  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_External_Attribution ~ Dacobs$IPSAQ_Negative_Internal + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.810 -3.837 -1.087  3.699 13.764 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     29.2391     5.2039   5.619 8.78e-08 ***
## Dacobs$IPSAQ_Negative_Internal   0.2590     0.1047   2.473  0.01448 *  
## Dacobs$Age                      -0.6951     0.2528  -2.750  0.00667 ** 
## Dacobs$DEM_01                   -1.3674     1.3165  -1.039  0.30059    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.231 on 154 degrees of freedom
##   (58 observations deleted due to missingness)
## Multiple R-squared:  0.09023,	Adjusted R-squared:  0.07251 
## F-statistic: 5.091 on 3 and 154 DF,  p-value: 0.002182
```

```r
cor.test(Dacobs$Dacobs_Safety_Behaviors    ,Dacobs$COPE_Avoidance)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Safety_Behaviors and Dacobs$COPE_Avoidance
## t = 1.945, df = 147, p-value = 0.05368
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.002463875  0.311270562
## sample estimates:
##       cor 
## 0.1583988
```

```r
summary(lm(Dacobs$Dacobs_Safety_Behaviors   ~Dacobs$COPE_Avoidance  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Safety_Behaviors ~ Dacobs$COPE_Avoidance + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.1503 -3.4963 -0.6647  2.8191 16.8312 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           22.35238    4.92278   4.541  1.2e-05 ***
## Dacobs$COPE_Avoidance  0.16949    0.08355   2.028   0.0444 *  
## Dacobs$Age            -0.62196    0.23650  -2.630   0.0095 ** 
## Dacobs$DEM_01         -0.63611    1.16005  -0.548   0.5843    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.543 on 140 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.07824,	Adjusted R-squared:  0.05848 
## F-statistic: 3.961 on 3 and 140 DF,  p-value: 0.009545
```

```r
cor.test(Dacobs$Dacobs_Safety_Behaviors    ,Dacobs$COPE_Denial)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Safety_Behaviors and Dacobs$COPE_Denial
## t = 1.7837, df = 147, p-value = 0.07653
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.01561173  0.29934734
## sample estimates:
##       cor 
## 0.1455536
```

```r
summary(lm(Dacobs$Dacobs_Safety_Behaviors   ~Dacobs$COPE_Denial  +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Safety_Behaviors ~ Dacobs$COPE_Denial + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.1138 -3.5823 -0.9674  2.6730 18.0623 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         23.3712     4.8858   4.783 4.32e-06 ***
## Dacobs$COPE_Denial   0.5105     0.2877   1.775   0.0781 .  
## Dacobs$Age          -0.6150     0.2372  -2.592   0.0105 *  
## Dacobs$DEM_01       -0.7296     1.1602  -0.629   0.5305    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.558 on 140 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.07202,	Adjusted R-squared:  0.05213 
## F-statistic: 3.622 on 3 and 140 DF,  p-value: 0.01475
```

```r
cor.test(Dacobs$Dacobs_Safety_Behaviors    ,Dacobs$COPE_Self_Blame)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Safety_Behaviors and Dacobs$COPE_Self_Blame
## t = 0.67415, df = 147, p-value = 0.5013
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1062308  0.2144034
## sample estimates:
##        cor 
## 0.05551748
```

```r
summary(lm(Dacobs$Dacobs_Safety_Behaviors   ~Dacobs$COPE_Self_Blame   +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Safety_Behaviors ~ Dacobs$COPE_Self_Blame + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4569 -3.3362 -0.9275  2.7099 17.6337 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             23.6424     4.9167   4.809 3.88e-06 ***
## Dacobs$COPE_Self_Blame   0.2573     0.2346   1.097  0.27451    
## Dacobs$Age              -0.6365     0.2401  -2.651  0.00896 ** 
## Dacobs$DEM_01           -0.9271     1.1611  -0.799  0.42593    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.589 on 140 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.05923,	Adjusted R-squared:  0.03907 
## F-statistic: 2.938 on 3 and 140 DF,  p-value: 0.03545
```

```r
cor.test(Dacobs$Dacobs_Safety_Behaviors    ,Dacobs$COPE_Self_Distraction)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Safety_Behaviors and Dacobs$COPE_Self_Distraction
## t = 1.0353, df = 147, p-value = 0.3022
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.07676584  0.24256567
## sample estimates:
##        cor 
## 0.08508439
```

```r
summary(lm(Dacobs$Dacobs_Safety_Behaviors   ~Dacobs$COPE_Self_Distraction    +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Safety_Behaviors ~ Dacobs$COPE_Self_Distraction + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.8730 -3.6007 -0.8692  2.5793 17.9393 
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   22.7989     5.1048   4.466 1.63e-05 ***
## Dacobs$COPE_Self_Distraction   0.2262     0.2658   0.851   0.3964    
## Dacobs$Age                    -0.5938     0.2399  -2.475   0.0145 *  
## Dacobs$DEM_01                 -0.8956     1.1653  -0.769   0.4434    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.597 on 140 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.05602,	Adjusted R-squared:  0.0358 
## F-statistic:  2.77 on 3 and 140 DF,  p-value: 0.04398
```

```r
cor.test(Dacobs$Dacobs_Safety_Behaviors    ,Dacobs$COPE_Beh_Disengagement)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Dacobs$Dacobs_Safety_Behaviors and Dacobs$COPE_Beh_Disengagement
## t = 2.4683, df = 147, p-value = 0.01472
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.0399762 0.3490952
## sample estimates:
##       cor 
## 0.1994938
```

```r
summary(lm(Dacobs$Dacobs_Safety_Behaviors   ~Dacobs$COPE_Beh_Disengagement     +Dacobs$Age+Dacobs$DEM_01))
```

```
## 
## Call:
## lm(formula = Dacobs$Dacobs_Safety_Behaviors ~ Dacobs$COPE_Beh_Disengagement + 
##     Dacobs$Age + Dacobs$DEM_01)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.6434 -3.5113 -0.5707  2.7780 16.6006 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    22.5075     4.8688   4.623 8.51e-06 ***
## Dacobs$COPE_Beh_Disengagement   0.7107     0.2963   2.398   0.0178 *  
## Dacobs$Age                     -0.5853     0.2353  -2.488   0.0140 *  
## Dacobs$DEM_01                  -0.5158     1.1574  -0.446   0.6565    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.517 on 140 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.08859,	Adjusted R-squared:  0.06906 
## F-statistic: 4.536 on 3 and 140 DF,  p-value: 0.004565
```


```r
#######################CUTOFF##############################
library("OptimalCutpoints")

df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Jumping_Conclusions)),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint1 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint1)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.602 (0.465, 0.739) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                     Estimate
## cutoff            27.0000000
## Se                 0.4615385
## Sp                 0.8010753
## PPV                0.2448980
## NPV                0.9141104
## DLR.Positive       2.3201663
## DLR.Negative       0.6721735
## FP                37.0000000
## FN                14.0000000
## Optimal criterion  0.2626137
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Belief_Inflexibility )),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint2 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint2)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.663 (0.551, 0.775) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                     Estimate
## cutoff            17.0000000
## Se                 0.5769231
## Sp                 0.7027027
## PPV                0.2142857
## NPV                0.9219858
## DLR.Positive       1.9405594
## DLR.Negative       0.6020710
## FP                55.0000000
## FN                11.0000000
## Optimal criterion  0.2796258
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Attention_Threats)),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint3 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint3)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.668 (0.557, 0.779) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                     Estimate
## cutoff            29.0000000
## Se                 0.4814815
## Sp                 0.7967914
## PPV                0.2549020
## NPV                0.9141104
## DLR.Positive       2.3693957
## DLR.Negative       0.6507581
## FP                38.0000000
## FN                14.0000000
## Optimal criterion  0.2782729
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_External_Attribution)),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint4 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint4)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.744 (0.652, 0.837) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                      Estimate
## cutoff             14.0000000
## Se                  0.9615385
## Sp                  0.4569892
## PPV                 0.1984127
## NPV                 0.9883721
## DLR.Positive        1.7707540
## DLR.Negative        0.0841629
## FP                101.0000000
## FN                  1.0000000
## Optimal criterion   0.4185277
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Social_Cognition_Prob )),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint5 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint5)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.734 (0.638, 0.831) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                     Estimate
## cutoff            20.0000000
## Se                 0.8518519
## Sp                 0.5322581
## PPV                0.2090909
## NPV                0.9611650
## DLR.Positive       1.8212005
## DLR.Negative       0.2783389
## FP                87.0000000
## FN                 4.0000000
## Optimal criterion  0.3841099
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Subjective_Cognition_Prob)),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint6 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint6)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.7 (0.608, 0.792) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                      Estimate
## cutoff             18.0000000
## Se                  0.9230769
## Sp                  0.4324324
## PPV                 0.1860465
## NPV                 0.9756098
## DLR.Positive        1.6263736
## DLR.Negative        0.1778846
## FP                105.0000000
## FN                  2.0000000
## Optimal criterion   0.3555094
```

```r
df=as.data.frame(cbind(x=as.numeric(as.character(Dacobs$Dacobs_Safety_Behaviors)),y=as.numeric(as.character(Dacobs$APSS_HighRisk)))) 
Cutpoint7 <- optimal.cutpoints(X = "x", status = "y", tag.healthy=0, methods = "Youden", data = df)
summary(Cutpoint7)
```

```
## 
## Call:
## optimal.cutpoints.default(X = "x", status = "y", tag.healthy = 0, 
##     methods = "Youden", data = df)
## 
## Area under the ROC curve (AUC):  0.646 (0.522, 0.769) 
## 
## CRITERION: Youden
## Number of optimal cutoffs: 1
## 
##                     Estimate
## cutoff            10.0000000
## Se                 0.7692308
## Sp                 0.4648649
## PPV                0.1680672
## NPV                0.9347826
## DLR.Positive       1.4374514
## DLR.Negative       0.4964222
## FP                99.0000000
## FN                 6.0000000
## Optimal criterion  0.2340956
```


```r
#######################ALPHA##############################
library("ltm")
```

```
## Loading required package: MASS
```

```
## Loading required package: msm
```

```
## Loading required package: polycor
```

```r
Dac=Dacobs [, grepl("DACOBS_0|DACOBS_1|DACOBS_2|DACOBS_3|DACOBS_4",colnames(Dacobs))] 
Dac=Dac[complete.cases(Dac),]
colnames(Dac)
```

```
##  [1] "DACOBS_01" "DACOBS_02" "DACOBS_03" "DACOBS_04" "DACOBS_05" "DACOBS_06"
##  [7] "DACOBS_07" "DACOBS_08" "DACOBS_09" "DACOBS_10" "DACOBS_11" "DACOBS_12"
## [13] "DACOBS_13" "DACOBS_14" "DACOBS_15" "DACOBS_16" "DACOBS_17" "DACOBS_18"
## [19] "DACOBS_19" "DACOBS_20" "DACOBS_21" "DACOBS_22" "DACOBS_23" "DACOBS_24"
## [25] "DACOBS_25" "DACOBS_26" "DACOBS_27" "DACOBS_28" "DACOBS_29" "DACOBS_30"
## [31] "DACOBS_31" "DACOBS_32" "DACOBS_33" "DACOBS_34" "DACOBS_35" "DACOBS_36"
## [37] "DACOBS_37" "DACOBS_38" "DACOBS_39" "DACOBS_40" "DACOBS_41" "DACOBS_42"
```

```r
#total
cronbach.alpha(Dac)
```

```
## 
## Cronbach's alpha for the 'Dac' data-set
## 
## Items: 42
## Sample units: 211
## alpha: 0.911
```

```r
#Dacobs_Jumping_Conclusions = items (3,8,16,18,25,30)
cronbach.alpha(Dac[,c(3,8,16,18,25,30)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(3, 8, 16, 18, 25, 30)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.702
```

```r
#Dacobs_Belief_Inflexibility = items  (13,15,26,34,38,41)
cronbach.alpha(Dac[,c(13,15,26,34,38,41)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(13, 15, 26, 34, 38, 41)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.511
```

```r
#Dacobs_Attention_Threats = items (1,2,6,10,20,37)
cronbach.alpha(Dac[,c(1,2,6,10,20,37)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(1, 2, 6, 10, 20, 37)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.667
```

```r
#Dacobs_External_Attribution = items (7,12,17,22,24,29)
cronbach.alpha(Dac[,c(7,12,17,22,24,29)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(7, 12, 17, 22, 24, 29)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.71
```

```r
#Dacobs_Social_Cognition_Prob = items (4,9,11,14,19,39)
cronbach.alpha(Dac[,c(4,9,11,14,19,39)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(4, 9, 11, 14, 19, 39)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.801
```

```r
#Dacobs_Subjective_Cognition_Prob = items (5,21,28,32,36,40)
cronbach.alpha(Dac[,c(5,21,28,32,36,40)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(5, 21, 28, 32, 36, 40)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.726
```

```r
#Dacobs_Safety_Behaviors = items (23,27,31,33,35,42)
cronbach.alpha(Dac[,c(23,27,31,33,35,42)])
```

```
## 
## Cronbach's alpha for the 'Dac[, c(23, 27, 31, 33, 35, 42)]' data-set
## 
## Items: 6
## Sample units: 211
## alpha: 0.69
```


```r
#######################CFA##############################
library(lavaan)
```

```
## This is lavaan 0.6-6
```

```
## lavaan is BETA software! Please report any bugs.
```

```r
colnames(Dac)
```

```
##  [1] "DACOBS_01" "DACOBS_02" "DACOBS_03" "DACOBS_04" "DACOBS_05" "DACOBS_06"
##  [7] "DACOBS_07" "DACOBS_08" "DACOBS_09" "DACOBS_10" "DACOBS_11" "DACOBS_12"
## [13] "DACOBS_13" "DACOBS_14" "DACOBS_15" "DACOBS_16" "DACOBS_17" "DACOBS_18"
## [19] "DACOBS_19" "DACOBS_20" "DACOBS_21" "DACOBS_22" "DACOBS_23" "DACOBS_24"
## [25] "DACOBS_25" "DACOBS_26" "DACOBS_27" "DACOBS_28" "DACOBS_29" "DACOBS_30"
## [31] "DACOBS_31" "DACOBS_32" "DACOBS_33" "DACOBS_34" "DACOBS_35" "DACOBS_36"
## [37] "DACOBS_37" "DACOBS_38" "DACOBS_39" "DACOBS_40" "DACOBS_41" "DACOBS_42"
```

```r
#Dacobs_Jumping_Conclusions = items (3,8,16,18,25,30)
colnames(Dac[,c(3,8,16,18,25,30)])
```

```
## [1] "DACOBS_03" "DACOBS_08" "DACOBS_16" "DACOBS_18" "DACOBS_25" "DACOBS_30"
```

```r
#Dacobs_Belief_Inflexibility = items  (13,15,26,34,38,41)
colnames(Dac[,c(13,15,26,34,38,41)])
```

```
## [1] "DACOBS_13" "DACOBS_15" "DACOBS_26" "DACOBS_34" "DACOBS_38" "DACOBS_41"
```

```r
#Dacobs_Attention_Threats = items (1,2,6,10,20,37)
colnames(Dac[,c(1,2,6,10,20,37)])
```

```
## [1] "DACOBS_01" "DACOBS_02" "DACOBS_06" "DACOBS_10" "DACOBS_20" "DACOBS_37"
```

```r
#Dacobs_External_Attribution = items (7,12,17,22,24,29)
colnames(Dac[,c(7,12,17,22,24,29)])
```

```
## [1] "DACOBS_07" "DACOBS_12" "DACOBS_17" "DACOBS_22" "DACOBS_24" "DACOBS_29"
```

```r
#Dacobs_Social_Cognition_Prob = items (4,9,11,14,19,39)
colnames(Dac[,c(4,9,11,14,19,39)])
```

```
## [1] "DACOBS_04" "DACOBS_09" "DACOBS_11" "DACOBS_14" "DACOBS_19" "DACOBS_39"
```

```r
#Dacobs_Subjective_Cognition_Prob = items (5,21,28,32,36,40)
colnames(Dac[,c(5,21,28,32,36,40)])
```

```
## [1] "DACOBS_05" "DACOBS_21" "DACOBS_28" "DACOBS_32" "DACOBS_36" "DACOBS_40"
```

```r
#Dacobs_Safety_Behaviors = items (23,27,31,33,35,42)
colnames(Dac[,c(23,27,31,33,35,42)])
```

```
## [1] "DACOBS_23" "DACOBS_27" "DACOBS_31" "DACOBS_33" "DACOBS_35" "DACOBS_42"
```

```r
CFA.model <- ' 
F1 =~ DACOBS_03 + DACOBS_08 + DACOBS_16 + DACOBS_18 + DACOBS_25 + DACOBS_30
F2 =~ DACOBS_13 + DACOBS_15 + DACOBS_26 + DACOBS_34 + DACOBS_38 + DACOBS_41
F3 =~ DACOBS_01 + DACOBS_02 + DACOBS_06 + DACOBS_10 + DACOBS_20 + DACOBS_37
F4 =~ DACOBS_07 + DACOBS_12 + DACOBS_17 + DACOBS_22 + DACOBS_24 + DACOBS_29
F5 =~ DACOBS_04 + DACOBS_09 + DACOBS_11 + DACOBS_14 + DACOBS_19 + DACOBS_39
F6 =~ DACOBS_05 + DACOBS_21 + DACOBS_28 + DACOBS_32 + DACOBS_36 + DACOBS_40
F7 =~ DACOBS_23 + DACOBS_27 + DACOBS_31 + DACOBS_33 + DACOBS_35 + DACOBS_42
'

fit <- cfa(CFA.model, data = Dac, estimator = "ML")
```

```
## Warning in lav_object_post_check(object): lavaan WARNING: covariance matrix of latent variables
##                 is not positive definite;
##                 use lavInspect(fit, "cov.lv") to investigate.
```

```r
summary(fit, fit.measures = TRUE)
```

```
## lavaan 0.6-6 ended normally after 95 iterations
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of free parameters                        105
##                                                       
##   Number of observations                           211
##                                                       
## Model Test User Model:
##                                                       
##   Test statistic                              1572.545
##   Degrees of freedom                               798
##   P-value (Chi-square)                           0.000
## 
## Model Test Baseline Model:
## 
##   Test statistic                              3616.424
##   Degrees of freedom                               861
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.719
##   Tucker-Lewis Index (TLI)                       0.697
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)             -15020.671
##   Loglikelihood unrestricted model (H1)     -14234.398
##                                                       
##   Akaike (AIC)                               30251.341
##   Bayesian (BIC)                             30603.287
##   Sample-size adjusted Bayesian (BIC)        30270.581
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.068
##   90 Percent confidence interval - lower         0.063
##   90 Percent confidence interval - upper         0.073
##   P-value RMSEA <= 0.05                          0.000
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.082
## 
## Parameter Estimates:
## 
##   Standard errors                             Standard
##   Information                                 Expected
##   Information saturated (h1) model          Structured
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   F1 =~                                               
##     DACOBS_03         1.000                           
##     DACOBS_08         1.080    0.199    5.428    0.000
##     DACOBS_16         0.864    0.186    4.655    0.000
##     DACOBS_18         1.453    0.257    5.647    0.000
##     DACOBS_25         0.986    0.200    4.922    0.000
##     DACOBS_30         0.907    0.182    4.992    0.000
##   F2 =~                                               
##     DACOBS_13         1.000                           
##     DACOBS_15         2.166    0.481    4.507    0.000
##     DACOBS_26         1.328    0.362    3.666    0.000
##     DACOBS_34         1.134    0.291    3.892    0.000
##     DACOBS_38         0.870    0.262    3.320    0.001
##     DACOBS_41         1.056    0.284    3.724    0.000
##   F3 =~                                               
##     DACOBS_01         1.000                           
##     DACOBS_02         0.929    0.214    4.333    0.000
##     DACOBS_06         1.504    0.293    5.130    0.000
##     DACOBS_10         0.991    0.219    4.528    0.000
##     DACOBS_20         1.068    0.278    3.845    0.000
##     DACOBS_37         1.679    0.321    5.236    0.000
##   F4 =~                                               
##     DACOBS_07         1.000                           
##     DACOBS_12         0.178    0.070    2.546    0.011
##     DACOBS_17         0.774    0.083    9.348    0.000
##     DACOBS_22         0.381    0.099    3.838    0.000
##     DACOBS_24         0.679    0.075    9.072    0.000
##     DACOBS_29         0.858    0.091    9.462    0.000
##   F5 =~                                               
##     DACOBS_04         1.000                           
##     DACOBS_09         0.936    0.100    9.317    0.000
##     DACOBS_11         0.562    0.089    6.336    0.000
##     DACOBS_14         0.824    0.096    8.555    0.000
##     DACOBS_19         0.974    0.105    9.276    0.000
##     DACOBS_39         0.921    0.113    8.133    0.000
##   F6 =~                                               
##     DACOBS_05         1.000                           
##     DACOBS_21         0.762    0.103    7.369    0.000
##     DACOBS_28         0.641    0.097    6.578    0.000
##     DACOBS_32         0.683    0.092    7.441    0.000
##     DACOBS_36         0.377    0.074    5.095    0.000
##     DACOBS_40         0.547    0.078    7.034    0.000
##   F7 =~                                               
##     DACOBS_23         1.000                           
##     DACOBS_27         1.770    0.330    5.369    0.000
##     DACOBS_31         1.900    0.313    6.079    0.000
##     DACOBS_33         1.713    0.321    5.331    0.000
##     DACOBS_35         2.077    0.319    6.517    0.000
##     DACOBS_42         0.954    0.148    6.434    0.000
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   F1 ~~                                               
##     F2                0.194    0.059    3.302    0.001
##     F3                0.411    0.107    3.834    0.000
##     F4                0.399    0.111    3.614    0.000
##     F5                0.471    0.116    4.045    0.000
##     F6                0.232    0.103    2.251    0.024
##     F7                0.103    0.036    2.836    0.005
##   F2 ~~                                               
##     F3                0.235    0.069    3.390    0.001
##     F4                0.418    0.104    4.022    0.000
##     F5                0.462    0.111    4.164    0.000
##     F6                0.536    0.126    4.240    0.000
##     F7                0.137    0.037    3.741    0.000
##   F3 ~~                                               
##     F4                0.674    0.153    4.396    0.000
##     F5                0.750    0.164    4.578    0.000
##     F6                0.587    0.143    4.104    0.000
##     F7                0.192    0.050    3.858    0.000
##   F4 ~~                                               
##     F5                1.293    0.203    6.361    0.000
##     F6                1.215    0.203    5.976    0.000
##     F7                0.329    0.068    4.831    0.000
##   F5 ~~                                               
##     F6                1.250    0.202    6.193    0.000
##     F7                0.291    0.063    4.619    0.000
##   F6 ~~                                               
##     F7                0.276    0.065    4.212    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .DACOBS_03         1.998    0.213    9.399    0.000
##    .DACOBS_08         1.189    0.139    8.542    0.000
##    .DACOBS_16         1.642    0.173    9.479    0.000
##    .DACOBS_18         1.593    0.201    7.925    0.000
##    .DACOBS_25         1.685    0.182    9.263    0.000
##    .DACOBS_30         1.333    0.145    9.193    0.000
##    .DACOBS_13         1.424    0.139   10.225    0.000
##    .DACOBS_15         1.685    0.180    9.338    0.000
##    .DACOBS_26         2.241    0.220   10.209    0.000
##    .DACOBS_34         1.243    0.122   10.150    0.000
##    .DACOBS_38         1.410    0.138   10.250    0.000
##    .DACOBS_41         1.325    0.130   10.197    0.000
##    .DACOBS_01         2.696    0.272    9.896    0.000
##    .DACOBS_02         1.968    0.200    9.823    0.000
##    .DACOBS_06         1.874    0.210    8.930    0.000
##    .DACOBS_10         1.820    0.187    9.715    0.000
##    .DACOBS_20         4.153    0.415    9.996    0.000
##    .DACOBS_37         1.922    0.224    8.591    0.000
##    .DACOBS_07         1.719    0.197    8.706    0.000
##    .DACOBS_12         1.381    0.135   10.211    0.000
##    .DACOBS_17         0.855    0.102    8.375    0.000
##    .DACOBS_22         2.613    0.258   10.125    0.000
##    .DACOBS_24         0.755    0.088    8.629    0.000
##    .DACOBS_29         0.988    0.120    8.253    0.000
##    .DACOBS_04         1.603    0.180    8.922    0.000
##    .DACOBS_09         1.349    0.152    8.865    0.000
##    .DACOBS_11         1.694    0.172    9.877    0.000
##    .DACOBS_14         1.462    0.158    9.277    0.000
##    .DACOBS_19         1.488    0.167    8.892    0.000
##    .DACOBS_39         2.183    0.231    9.441    0.000
##    .DACOBS_05         1.508    0.197    7.665    0.000
##    .DACOBS_21         2.301    0.247    9.324    0.000
##    .DACOBS_28         2.243    0.234    9.589    0.000
##    .DACOBS_32         1.795    0.193    9.296    0.000
##    .DACOBS_36         1.470    0.148    9.914    0.000
##    .DACOBS_40         1.356    0.143    9.447    0.000
##    .DACOBS_23         0.404    0.045    9.026    0.000
##    .DACOBS_27         1.873    0.199    9.432    0.000
##    .DACOBS_31         1.308    0.147    8.880    0.000
##    .DACOBS_33         1.797    0.190    9.453    0.000
##    .DACOBS_35         1.082    0.131    8.251    0.000
##    .DACOBS_42         0.246    0.029    8.400    0.000
##     F1                0.580    0.180    3.216    0.001
##     F2                0.167    0.070    2.374    0.018
##     F3                0.529    0.187    2.826    0.005
##     F4                1.610    0.297    5.426    0.000
##     F5                1.526    0.276    5.522    0.000
##     F6                1.799    0.315    5.710    0.000
##     F7                0.182    0.047    3.911    0.000
```

```r
lavInspect(fit, "cov.lv")
```

```
##    F1    F2    F3    F4    F5    F6    F7   
## F1 0.580                                    
## F2 0.194 0.167                              
## F3 0.411 0.235 0.529                        
## F4 0.399 0.418 0.674 1.610                  
## F5 0.471 0.462 0.750 1.293 1.526            
## F6 0.232 0.536 0.587 1.215 1.250 1.799      
## F7 0.103 0.137 0.192 0.329 0.291 0.276 0.182
```


```r
#######################PCA##############################
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following object is masked from 'package:lavaan':
## 
##     cor2cov
```

```
## The following object is masked from 'package:ltm':
## 
##     factor.scores
```

```
## The following object is masked from 'package:polycor':
## 
##     polyserial
```

```r
PCA=principal(Dac, nfactors = 7, residuals = FALSE,rotate="oblimin")
```

```
## Loading required namespace: GPArotation
```

```r
summary(PCA)
```

```
## 
## Factor analysis with Call: principal(r = Dac, nfactors = 7, residuals = FALSE, rotate = "oblimin")
## 
## Test of the hypothesis that 7 factors are sufficient.
## The degrees of freedom for the model is 588  and the objective function was  4.72 
## The number of observations was  211  with Chi Square =  899.08  with prob <  2e-15 
## 
## The root mean square of the residuals (RMSA) is  0.05 
## 
##  With component correlations of 
##      TC1   TC2   TC4  TC5  TC3  TC6   TC7
## TC1 1.00  0.22  0.24 0.32 0.16 0.24  0.10
## TC2 0.22  1.00  0.24 0.10 0.09 0.08 -0.01
## TC4 0.24  0.24  1.00 0.10 0.10 0.14 -0.04
## TC5 0.32  0.10  0.10 1.00 0.16 0.19  0.09
## TC3 0.16  0.09  0.10 0.16 1.00 0.11  0.00
## TC6 0.24  0.08  0.14 0.19 0.11 1.00  0.05
## TC7 0.10 -0.01 -0.04 0.09 0.00 0.05  1.00
```

```r
FL=PCA$Structure
FL= round(FL,2)
FL[abs(FL)<0.20]=" "
FL=as.data.frame(FL)
FL
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["TC1"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["TC2"],"name":[2],"type":["fctr"],"align":["left"]},{"label":["TC4"],"name":[3],"type":["fctr"],"align":["left"]},{"label":["TC5"],"name":[4],"type":["fctr"],"align":["left"]},{"label":["TC3"],"name":[5],"type":["fctr"],"align":["left"]},{"label":["TC6"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["TC7"],"name":[7],"type":["fctr"],"align":["left"]}],"data":[{"1":"","2":"","3":"0.59","4":"","5":"","6":"0.23","7":"","_rn_":"DACOBS_01","_row":"DACOBS_01"},{"1":"","2":"0.2","3":"0.62","4":"0.23","5":"0.29","6":"","7":"","_rn_":"DACOBS_02","_row":"DACOBS_02"},{"1":"","2":"0.47","3":"0.43","4":"","5":"","6":"-0.31","7":"","_rn_":"DACOBS_03","_row":"DACOBS_03"},{"1":"0.67","2":"0.24","3":"0.27","4":"0.3","5":"","6":"0.29","7":"0.27","_rn_":"DACOBS_04","_row":"DACOBS_04"},{"1":"0.61","2":"","3":"0.27","4":"0.56","5":"","6":"","7":"0.33","_rn_":"DACOBS_05","_row":"DACOBS_05"},{"1":"0.71","2":"0.28","3":"0.3","4":"0.26","5":"0.22","6":"","7":"","_rn_":"DACOBS_06","_row":"DACOBS_06"},{"1":"0.74","2":"","3":"0.29","4":"","5":"","6":"","7":"","_rn_":"DACOBS_07","_row":"DACOBS_07"},{"1":"0.2","2":"0.41","3":"0.56","4":"","5":"","6":"","7":"","_rn_":"DACOBS_08","_row":"DACOBS_08"},{"1":"0.59","2":"0.24","3":"0.31","4":"0.38","5":"","6":"0.37","7":"0.47","_rn_":"DACOBS_09","_row":"DACOBS_09"},{"1":"0.38","2":"0.2","3":"0.57","4":"0.21","5":"","6":"","7":"","_rn_":"DACOBS_10","_row":"DACOBS_10"},{"1":"0.28","2":"0.2","3":"0.56","4":"","5":"0.39","6":"0.2","7":"","_rn_":"DACOBS_11","_row":"DACOBS_11"},{"1":"","2":"0.37","3":"","4":"","5":"","6":"","7":"0.41","_rn_":"DACOBS_12","_row":"DACOBS_12"},{"1":"","2":"0.63","3":"0.2","4":"","5":"","6":"","7":"","_rn_":"DACOBS_13","_row":"DACOBS_13"},{"1":"0.52","2":"0.34","3":"0.27","4":"0.24","5":"","6":"0.48","7":"0.25","_rn_":"DACOBS_14","_row":"DACOBS_14"},{"1":"0.57","2":"","3":"","4":"0.56","5":"","6":"0.29","7":"","_rn_":"DACOBS_15","_row":"DACOBS_15"},{"1":"0.22","2":"","3":"0.62","4":"","5":"","6":"","7":"","_rn_":"DACOBS_16","_row":"DACOBS_16"},{"1":"0.73","2":"0.31","3":"","4":"0.35","5":"","6":"0.26","7":"","_rn_":"DACOBS_17","_row":"DACOBS_17"},{"1":"0.37","2":"0.63","3":"0.34","4":"","5":"","6":"","7":"","_rn_":"DACOBS_18","_row":"DACOBS_18"},{"1":"0.59","2":"0.37","3":"0.29","4":"0.25","5":"","6":"0.51","7":"","_rn_":"DACOBS_19","_row":"DACOBS_19"},{"1":"","2":"","3":"0.33","4":"0.23","5":"0.26","6":"0.59","7":"-0.26","_rn_":"DACOBS_20","_row":"DACOBS_20"},{"1":"0.28","2":"0.25","3":"","4":"0.69","5":"","6":"","7":"","_rn_":"DACOBS_21","_row":"DACOBS_21"},{"1":"0.27","2":"0.3","3":"0.21","4":"0.28","5":"","6":"","7":"-0.6","_rn_":"DACOBS_22","_row":"DACOBS_22"},{"1":"0.26","2":"0.21","3":"","4":"","5":"0.69","6":"","7":"","_rn_":"DACOBS_23","_row":"DACOBS_23"},{"1":"0.71","2":"","3":"","4":"0.33","5":"0.38","6":"","7":"","_rn_":"DACOBS_24","_row":"DACOBS_24"},{"1":"","2":"0.55","3":"0.35","4":"","5":"","6":"0.23","7":"","_rn_":"DACOBS_25","_row":"DACOBS_25"},{"1":"0.24","2":"","3":"0.34","4":"","5":"","6":"0.56","7":"0.32","_rn_":"DACOBS_26","_row":"DACOBS_26"},{"1":"","2":"","3":"","4":"","5":"0.49","6":"0.43","7":"","_rn_":"DACOBS_27","_row":"DACOBS_27"},{"1":"0.25","2":"","3":"","4":"0.63","5":"","6":"","7":"","_rn_":"DACOBS_28","_row":"DACOBS_28"},{"1":"0.77","2":"","3":"","4":"0.27","5":"","6":"0.23","7":"","_rn_":"DACOBS_29","_row":"DACOBS_29"},{"1":"0.22","2":"0.74","3":"","4":"","5":"","6":"","7":"","_rn_":"DACOBS_30","_row":"DACOBS_30"},{"1":"0.35","2":"","3":"","4":"","5":"0.54","6":"0.43","7":"","_rn_":"DACOBS_31","_row":"DACOBS_31"},{"1":"0.27","2":"","3":"","4":"0.76","5":"","6":"","7":"","_rn_":"DACOBS_32","_row":"DACOBS_32"},{"1":"0.26","2":"0.21","3":"","4":"0.46","5":"0.42","6":"0.21","7":"","_rn_":"DACOBS_33","_row":"DACOBS_33"},{"1":"","2":"0.42","3":"","4":"0.47","5":"0.28","6":"","7":"","_rn_":"DACOBS_34","_row":"DACOBS_34"},{"1":"0.3","2":"","3":"0.21","4":"0.25","5":"0.6","6":"0.4","7":"","_rn_":"DACOBS_35","_row":"DACOBS_35"},{"1":"0.3","2":"0.23","3":"","4":"0.21","5":"","6":"0.64","7":"","_rn_":"DACOBS_36","_row":"DACOBS_36"},{"1":"0.44","2":"0.36","3":"0.56","4":"","5":"0.23","6":"0.3","7":"","_rn_":"DACOBS_37","_row":"DACOBS_37"},{"1":"","2":"0.74","3":"","4":"","5":"","6":"","7":"","_rn_":"DACOBS_38","_row":"DACOBS_38"},{"1":"0.6","2":"","3":"0.31","4":"0.44","5":"0.29","6":"0.25","7":"","_rn_":"DACOBS_39","_row":"DACOBS_39"},{"1":"0.26","2":"","3":"","4":"0.55","5":"","6":"","7":"0.6","_rn_":"DACOBS_40","_row":"DACOBS_40"},{"1":"0.25","2":"0.31","3":"0.29","4":"0.32","5":"","6":"","7":"-0.24","_rn_":"DACOBS_41","_row":"DACOBS_41"},{"1":"0.27","2":"","3":"0.25","4":"","5":"0.72","6":"","7":"","_rn_":"DACOBS_42","_row":"DACOBS_42"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

