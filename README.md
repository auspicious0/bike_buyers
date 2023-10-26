- [자전거 구매자 정보를 바탕으로 로지스틱 회귀 분석](#---------------------------)
  * [패키지 설치 및 그래프 설정](#---------------)
  * [데이터 수집](#------)
  * [데이터 전처리](#-------)
  * [카이제곱 검정](#-------)
  * [로지스틱 회귀 분석](#----------)
  * [변수 선택 (Backward Elimination)](#-------backward-elimination-)
  * [모델 평가](#-----)
    + [ROC 곡선](#roc---)
    + [예측 및 성능 평가](#----------)
  * [문의](#--)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

# 자전거 구매자 정보를 바탕으로 로지스틱 회귀 분석

이 프로젝트는 자전거 구매자 정보를 바탕으로 로지스틱 회귀 분석을 수행하는 것을 목표로 합니다. 자전거 구매에 영향을 미치는 요인을 파악하고 예측하는 과정을 다룹니다.

## 패키지 설치 및 그래프 설정

프로젝트를 시작하기 전, 필요한 R 패키지를 설치하고 그래프 설정을 합니다.
 ```
#패키지 부착, 출력 그래프의 크기를 설정
install.packages(c("tidyverse","data.table"))
library(tidyverse)
library(data.table)
library(repr)

options(repr.plot.width=7,repr.plot.height=7)
install.packages("gmodels")
install.packages("caret")

options(repr.plot.width=7, repr.plot.height=7)
```

## 데이터 수집

자전거 구매자 정보 데이터는 Kaggle에서 제공되며 다음 링크에서 얻을 수 있습니다:

[Kaggle Dataset](https://www.kaggle.com/datasets/heeraldedhia/bike-buyers)

데이터를 다운로드하고 분석에 활용합니다.

```
#https://drive.google.com/file/d/1jG1BdFg3hrSqrRIWdJuLy_8y1zvIMPvJ/view?usp=sharing
#https://www.kaggle.com/datasets/heeraldedhia/bike-buyers
#자전거 구매자 정보를 바탕으로 자전거 사는 것에 대한 로지스틱 회귀를 수행하려 한다.
system("gdown --id 1jG1BdFg3hrSqrRIWdJuLy_8y1zvIMPvJ")
system("ls",TRUE)

DF<-fread("bike_buyers.csv",header=T,encoding="UTF-8")%>%as_tibble()
DF%>%show()
```

## 데이터 전처리

데이터를 불러온 후, 결측값 처리와 이상값 처리를 수행합니다. 데이터의 구조를 확인하고 필요한 변수를 팩터(factor)로 변환합니다. 

```
DF <- na.omit(DF)

# 이상치 및 결측값 처리 함수
calculate_outliers <- function(data, column_name) {
  iqr_value <- IQR(data[[column_name]])
  upper_limit <- quantile(data[[column_name]], 0.75) + 1.5 * iqr_value
  lower_limit <- quantile(data[[column_name]], 0.25) - 1.5 * iqr_value

  data[[column_name]] <- ifelse(data[[column_name]] < lower_limit | data[[column_name]] > upper_limit, NA, data[[column_name]])

  return(data)
}
DF <- DF %>%
  mutate(
    `Marital Status` = ifelse(`Marital Status` == "", NA, as.character(`Marital Status`)),
    Gender = ifelse(Gender == "", NA, as.character(Gender)),
    `Home Owner` = ifelse(`Home Owner` == "", NA, as.character(`Home Owner`))
  ) %>%
  drop_na()
# 이상치 및 결측값 처리 및 결과에 대한 상자그림 그리기
DF <- calculate_outliers(DF, "Income")
DF <- calculate_outliers(DF, "Children")
DF <- calculate_outliers(DF, "Cars")
DF <- calculate_outliers(DF, "Age")
DF <- na.omit(DF)
boxplot(DF$Income,DF$Children,DF$Cars,DF$Age)

DF %>% summary()
table(is.na(DF))
DF$`Marital Status` %>%unique()
DF$Gender %>% unique()
DF$Education %>% unique()
DF$Occupation %>% unique()
DF$`Home Owner` %>% unique()
DF$Region %>% unique()
DF$`Purchased Bike` %>% unique()

DF<-select(DF,-ID)%>%
  mutate_at(c("Marital Status","Gender","Education","Occupation","Home Owner","Region","Purchased Bike"),factor)
```

## 카이제곱 검정

1. 자전거 구매 여부와 결혼 상태 (Marital Status) 간의 연관관계

다음의 코드를 사용하여 자전거 구매 여부와 결혼 상태 간의 연관관계를 카이제곱 검정을 통해 분석했습니다. 이 검정은 범주형 변수 간의 독립성을 평가합니다.
```
gmodels::CrossTable(DF$`Purchased Bike`, DF$`Marital Status`, chisq = TRUE, expected = TRUE, prop.r = FALSE, prop.c = FALSE)
```

```
"자전거(anxiousness)와 결혼(Marital Status)와의 연관관계 분석 "

 
   Cell Contents
|-------------------------|
|                       N |
|              Expected N |
| Chi-square contribution |
|         N / Table Total |
|-------------------------|

 
Total Observations in Table:  889 

 
                    | DF$`Marital Status` 
DF$`Purchased Bike` |   Married |    Single | Row Total | 
--------------------|-----------|-----------|-----------|
                 No |       273 |       183 |       456 | 
                    |   246.209 |   209.791 |           | 
                    |     2.915 |     3.421 |           | 
                    |     0.307 |     0.206 |           | 
--------------------|-----------|-----------|-----------|
                Yes |       207 |       226 |       433 | 
                    |   233.791 |   199.209 |           | 
                    |     3.070 |     3.603 |           | 
                    |     0.233 |     0.254 |           | 
--------------------|-----------|-----------|-----------|
       Column Total |       480 |       409 |       889 | 
--------------------|-----------|-----------|-----------|

 
Statistics for All Table Factors


Pearson's Chi-squared test 
------------------------------------------------------------
Chi^2 =  13.00944     d.f. =  1     p =  0.0003099247 

Pearson's Chi-squared test with Yates' continuity correction 
------------------------------------------------------------
Chi^2 =  12.52838     d.f. =  1     p =  0.0004008178 

```


결과에서 p-값을 확인하여 자전거 구매 여부와 결혼 상태 간의 관계가 통계적으로 유의미한지 확인할 수 있습니다.



2. 자전거 구매 여부와 통근 거리 (Commute Distance) 간의 연관관계
자전거 구매 여부와 통근 거리 간의 연관관계를 카이제곱 검정을 통해 분석하였습니다. 아래 코드를 사용하여 실행하였습니다:

```
gmodels::CrossTable(DF$`Purchased Bike`, DF$`Commute Distance`, chisq = TRUE, expected = TRUE, prop.r = FALSE, prop.c = FALSE)
```
카이제곱 검정 결과의 p-값을 통해 자전거 구매 여부와 통근 거리 간의 연관성을 확인할 수 있습니다.

```
"자전거(anxiousness)와 통근(Commute Distance)와의 연관관계 분석 "

 
   Cell Contents
|-------------------------|
|                       N |
|              Expected N |
| Chi-square contribution |
|         N / Table Total |
|-------------------------|

 
Total Observations in Table:  889 

 
                    | DF$`Commute Distance` 
DF$`Purchased Bike` |  0-1 Miles |  1-2 Miles |  10+ Miles |  2-5 Miles | 5-10 Miles |  Row Total | 
--------------------|------------|------------|------------|------------|------------|------------|
                 No |        136 |         82 |         67 |         62 |        109 |        456 | 
                    |    163.627 |     80.531 |     46.677 |     76.940 |     88.225 |            | 
                    |      4.664 |      0.027 |      8.848 |      2.901 |      4.892 |            | 
                    |      0.153 |      0.092 |      0.075 |      0.070 |      0.123 |            | 
--------------------|------------|------------|------------|------------|------------|------------|
                Yes |        183 |         75 |         24 |         88 |         63 |        433 | 
                    |    155.373 |     76.469 |     44.323 |     73.060 |     83.775 |            | 
                    |      4.912 |      0.028 |      9.318 |      3.055 |      5.152 |            | 
                    |      0.206 |      0.084 |      0.027 |      0.099 |      0.071 |            | 
--------------------|------------|------------|------------|------------|------------|------------|
       Column Total |        319 |        157 |         91 |        150 |        172 |        889 | 
--------------------|------------|------------|------------|------------|------------|------------|

 
Statistics for All Table Factors


Pearson's Chi-squared test 
------------------------------------------------------------
Chi^2 =  43.79881     d.f. =  4     p =  7.063732e-09 

```
3. 자전거 구매 여부와 성별 (Gender) 간의 연관관계
자전거 구매 여부와 성별 간의 연관관계를 카이제곱 검정을 통해 분석했습니다. 아래 코드를 사용하여 실행하였습니다:

```
gmodels::CrossTable(DF$`Purchased Bike`, DF$Gender, chisq = TRUE, expected = TRUE, prop.r = FALSE, prop.c = FALSE)
```

카이제곱 검정 결과에서 p-값을 확인하여 자전거 구매 여부와 성별 간의 관계가 통계적으로 유의미한지 확인할 수 있습니다.

```
"자전거(anxiousness)와 성별(Gender)와의 연관관계 분석 "

 
   Cell Contents
|-------------------------|
|                       N |
|              Expected N |
| Chi-square contribution |
|         N / Table Total |
|-------------------------|

 
Total Observations in Table:  889 

 
                    | DF$Gender 
DF$`Purchased Bike` |    Female |      Male | Row Total | 
--------------------|-----------|-----------|-----------|
                 No |       218 |       238 |       456 | 
                    |   225.692 |   230.308 |           | 
                    |     0.262 |     0.257 |           | 
                    |     0.245 |     0.268 |           | 
--------------------|-----------|-----------|-----------|
                Yes |       222 |       211 |       433 | 
                    |   214.308 |   218.692 |           | 
                    |     0.276 |     0.271 |           | 
                    |     0.250 |     0.237 |           | 
--------------------|-----------|-----------|-----------|
       Column Total |       440 |       449 |       889 | 
--------------------|-----------|-----------|-----------|

 
Statistics for All Table Factors


Pearson's Chi-squared test 
------------------------------------------------------------
Chi^2 =  1.065634     d.f. =  1     p =  0.3019336 

Pearson's Chi-squared test with Yates' continuity correction 
------------------------------------------------------------
Chi^2 =  0.9315954     d.f. =  1     p =  0.3344487 

```



## 로지스틱 회귀 분석

학습 데이터와 테스트 데이터 분리
데이터를 학습 데이터와 테스트 데이터로 분리하고 모델 학습을 준비합니다:

```
index <- caret::createDataPartition(y = DF$`Purchased Bike`, p = 0.8, list = FALSE)
train <- DF[index,]
test <- DF[-index,]
```

로지스틱 회귀 모델을 학습합니다:

```
m <- glm(formula = `Purchased Bike` ~ ., data = train, family = "binomial")
summary(m)
```

```
 "glm model m"

Call:
glm(formula = `Purchased Bike` ~ ., family = "binomial", data = train)

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  -6.782e-01  5.645e-01  -1.201  0.22959    
`Marital Status`Single        8.222e-01  1.872e-01   4.393 1.12e-05 ***
GenderMale                    9.021e-03  1.657e-01   0.054  0.95658    
Income                        6.754e-06  5.288e-06   1.277  0.20152    
Children                     -1.362e-01  6.595e-02  -2.065  0.03897 *  
EducationGraduate Degree     -4.215e-01  2.624e-01  -1.607  0.10812    
EducationHigh School          2.274e-02  3.072e-01   0.074  0.94099    
EducationPartial College     -2.464e-01  2.621e-01  -0.940  0.34717    
EducationPartial High School -5.671e-01  4.389e-01  -1.292  0.19626    
OccupationManagement          1.784e-01  5.115e-01   0.349  0.72729    
OccupationManual             -1.138e-01  3.347e-01  -0.340  0.73376    
OccupationProfessional        8.442e-01  3.997e-01   2.112  0.03466 *  
OccupationSkilled Manual      2.452e-01  3.200e-01   0.766  0.44355    
`Home Owner`Yes               4.817e-01  2.068e-01   2.329  0.01986 *  
Cars                         -3.367e-01  1.245e-01  -2.704  0.00685 ** 
`Commute Distance`1-2 Miles  -4.481e-01  2.502e-01  -1.791  0.07329 .  
`Commute Distance`10+ Miles  -1.711e+00  3.709e-01  -4.614 3.94e-06 ***
`Commute Distance`2-5 Miles  -2.952e-01  2.621e-01  -1.126  0.26008    
`Commute Distance`5-10 Miles -1.245e+00  3.079e-01  -4.042 5.30e-05 ***
RegionNorth America           5.437e-02  2.654e-01   0.205  0.83771    
RegionPacific                 1.017e+00  3.094e-01   3.288  0.00101 ** 
Age                           1.020e-02  9.958e-03   1.024  0.30578    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 986.59  on 711  degrees of freedom
Residual deviance: 873.31  on 690  degrees of freedom
AIC: 917.31

Number of Fisher Scoring iterations: 4
```
## 변수 선택 (Backward Elimination)

학습된 모델에서 불필요한 변수를 제거하기 위해 역방향 제거 방법을 사용합니다:

```
mback <- step(m, direction = "backward")
```

## 모델 평가

### ROC 곡선

ROC 곡선을 사용하여 모델의 성능을 시각화하고 적절한 CUT-OFF 값을 선택합니다:

```
install.packages("pROC")
roc_c <- pROC::roc(predict_check$`Purchased Bike`, predict_check$predict_value)
pROC::plot.roc(roc_c, col = "royalblue", print.auc = TRUE, max.auc.polygon = TRUE, print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red", auc.polygon = TRUE, auc.polygon.col = "#D1F2EB")

```
![image](https://github.com/auspicious0/bike_buyers/assets/108572025/414a0572-34d0-4f25-b544-f728f755ce7c)

### 예측 및 성능 평가

선택한 CUT-OFF 값으로 예측을 수행하고 모델의 성능을 혼돈 매트릭스를 통해 확인합니다:

```
predict_value <- predict(mback, test, type = "response") %>% tibble(predict_value = .)
predict_check <- test %>% select(`Purchased Bike`) %>% dplyr::bind_cols(., predict_value)
predict_cutoff_roc <- predict_check %>%
  mutate(predict_biked = as.factor(ifelse(predict_value > 0.5136439, "Yes", "No")))

# 혼돈 매트릭스를 통한 성능 평가
caret::confusionMatrix(predict_cutoff_roc$`Purchased Bike`, predict_cutoff_roc$predict_biked)

```
A tibble: 177 × 3
Purchased Bike	predict_value	predict_biked
<fct>	<dbl>	<fct>
Yes	0.5228698	Yes
Yes	0.5782981	Yes
No	0.4999350	No
No	0.4254288	No
Yes	0.5846695	Yes
Yes	0.5796278	Yes
No	0.2024137	No
Yes	0.5499326	Yes
Yes	0.6366437	Yes
Yes	0.8329877	Yes
No	0.4634943	No
Yes	0.5499326	Yes
No	0.2385416	No
Yes	0.4232865	No
Yes	0.4739440	No
No	0.4943569	No
No	0.5533325	Yes
No	0.3537212	No
Yes	0.5605530	Yes
Yes	0.8519412	Yes
No	0.6782554	Yes
Yes	0.7448223	Yes
Yes	0.8518051	Yes
No	0.4815053	No
Yes	0.1969868	No
Yes	0.2856157	No
No	0.5076084	No
No	0.2385416	No
No	0.5785834	Yes
Yes	0.5796278	Yes
⋮	⋮	⋮
No	0.2202885	No
No	0.6198953	Yes
No	0.4304286	No
No	0.2766733	No
No	0.3742456	No
No	0.4560945	No
Yes	0.1315284	No
Yes	0.6996423	Yes
No	0.3790706	No
Yes	0.6996423	Yes
No	0.6447875	Yes
No	0.4660337	No
Yes	0.6198953	Yes
Yes	0.7359148	Yes
No	0.1356468	No
Yes	0.2202885	No
No	0.2078664	No
Yes	0.4417597	No
No	0.3851596	No
Yes	0.7145817	Yes
No	0.2202885	No
No	0.3112684	No
No	0.3742456	No
No	0.4378422	No
No	0.5716155	Yes
No	0.7123073	Yes
Yes	0.2059617	No
No	0.2031100	No
No	0.2823118	No
Yes	0.3698322	No

```

```

```
Confusion Matrix and Statistics

          Reference
Prediction No Yes
       No  68  23
       Yes 35  51
                                          
               Accuracy : 0.6723          
                 95% CI : (0.5979, 0.7409)
    No Information Rate : 0.5819          
    P-Value [Acc > NIR] : 0.00849         
                                          
                  Kappa : 0.3416          
                                          
 Mcnemar's Test P-Value : 0.14863         
                                          
            Sensitivity : 0.6602          
            Specificity : 0.6892          
         Pos Pred Value : 0.7473          
         Neg Pred Value : 0.5930          
             Prevalence : 0.5819          
         Detection Rate : 0.3842          
   Detection Prevalence : 0.5141          
      Balanced Accuracy : 0.6747          
                                          
       'Positive' Class : No              
                                  
```

정확도 (Accuracy): 0.6723

전체 예측 중에서 올바르게 분류한 비율로, 0.6723 또는 67.23%입니다.(TP + TN) / (TP + TN + FP + FN)

민감도 (Sensitivity): 0.6602

실제 양성 중에서 올바르게 양성으로 분류된 비율로, 0.6602 또는 66.02%입니다.

특이도 (Specificity): 0.6892

실제 음성 중에서 올바르게 음성으로 분류된 비율로, 0.6892 또는 68.92%입니다.

정밀도 (Precision): 0.7473

정밀도는 모델이 양성으로 예측한 샘플 중에서 실제로 양성인 샘플의 비율을 나타냅니다. TP / (TP + FP)

재현율 (Recall): 0.6602

재현율은 실제로 양성인 샘플 중에서 모델이 양성으로 예측한 샘플의 비율을 나타냅니다. TP / (TP + FN)

따라서 자전거를 살 것인지 어느 정도 유의미한 예측을 할 수 있었습니다. 예측의 정도가 크지 않아 신뢰도가 높진 않지만 어떤 병원이나 심각한 자료 분석을 수행하는 경우가 아니기 때문에 참고 정도의 자료로 작용할 수 있을 것으로 보입니다.


## 문의
프로젝트에 관한 문의나 버그 리포트는 [이슈 페이지](https://github.com/auspicious0/bike_buyers/issues)를 통해 제출해주세요.

보다 더 자세한 내용을 원하신다면 [보고서](https://github.com/auspicious0/bike_buyers/blob/main/%EB%A1%9C%EC%A7%80%EC%8A%A4%ED%8B%B1%ED%9A%8C%EA%B7%80_bike_buyers.ipynb) 를 확인해 주시기 바랍니다.
