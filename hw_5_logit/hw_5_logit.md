hw\_5\_logit
================
Kammy Chiu
April 4, 2017

### Introduction

The goal of this project is to build a model such that we can make inferences about the association between age and the probability of a registered voter voting. After tidying a random sample of the Oregon Voter data, I partitioned the data set into a training set and a testing test in order to assess the validity of the relationships found with the training set. Four models were used in this analysis.

``` r
set.seed(87)
voter_sample <- read_csv("data/or_voter.csv") %>%
  sample_n(200000)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   VOTER_ID = col_integer(),
    ##   ZIP_CODE = col_integer()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 1 parsing failure.
    ##     row col   expected     actual
    ## 2941547  -- 40 columns 25 columns

``` r
voter_tidy <- voter_sample %>%
  select(VOTER_ID, BIRTH_DATE.x,PARTY_CODE,
         COUNTY.x, `11/08/2016`:`05/16/2006`) %>%
  gather(`11/08/2016`:`05/16/2006`, key = ELECTION, value = VOTED) %>%
  #add AGE
  mutate(BIRTH_DATE.x = mdy(BIRTH_DATE.x)) %>%
  mutate(AGE = 2017-year(BIRTH_DATE.x)) %>%
  #recode VOTED
  mutate(VOTED = ifelse(VOTED == "-", NA, VOTED)) %>%
  mutate(VOTED = ifelse(VOTED == "YES", 1, 0), VOTED = as.factor(VOTED)) %>%
  #recode PARTY_CODE
  mutate(PARTY_CODE = ifelse(PARTY_CODE %in% c("REP","DEM","NAV"),
                  PARTY_CODE, "OTHER")) %>%
  mutate(PARTY_CODE = as.factor(PARTY_CODE))
```

    ## Warning: 1575 failed to parse.

``` r
#partition data set
set.seed(21)
train_indices <- sample(1:nrow(voter_tidy), size = 9768, replace = FALSE)
train_voter <- slice(voter_tidy, train_indices)
test_voter  <- slice(voter_tidy, -train_indices)
```

### Model 1: VOTE ~ AGE

``` r
#fit model 1 to full data set
m1 <- glm(VOTED ~ AGE, data = voter_tidy, family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
#fit model 1 to training set
m1_t <- glm(VOTED ~ AGE, data = train_voter, family = binomial)

#predict values for test set
y1 <- predict(m1_t, newdata = test_voter, type = "response")
test_voter <- test_voter %>%
  mutate(p_hat1 = y1,
         pred_m1 = p_hat1 > .5)

#form confusion matrix
confusion_mat1 <- test_voter %>%
  group_by(VOTED, pred_m1) %>%
  tally()
confusion_mat1
```

    ## Source: local data frame [7 x 3]
    ## Groups: VOTED [?]
    ## 
    ##    VOTED pred_m1       n
    ##   <fctr>   <lgl>   <int>
    ## 1      0   FALSE  529289
    ## 2      0    TRUE  489691
    ## 3      1   FALSE  335039
    ## 4      1    TRUE  861413
    ## 5     NA   FALSE 1803015
    ## 6     NA    TRUE  970160
    ## 7     NA      NA    1625

``` r
#find miscalculation rate
false_pos <- confusion_mat1[2, 3]
false_neg <- confusion_mat1[3, 3]
total_obs <- nrow(test_voter)
mcr1 <- (false_pos + false_neg)/total_obs
mcr1
```

    ##           n
    ## 1 0.1652689

### Model 2: vote ~ age + party

``` r
#fit model 2 to full data set
m2 <- glm(VOTED ~ AGE + PARTY_CODE, data = voter_tidy, family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
#fit model 2 to training set
m2_t <- glm(VOTED ~ AGE + PARTY_CODE, data = train_voter, family = binomial)

#predicting variables
y2 <- predict(m2_t, newdata = test_voter, type = "response")
test_voter <- test_voter %>%
  mutate(p_hat2 = y2,
         pred_m2 = p_hat2 > .5)

#form confusion matrix
confusion_mat2 <- test_voter %>%
  group_by(VOTED, pred_m2) %>%
  tally()
confusion_mat2
```

    ## Source: local data frame [7 x 3]
    ## Groups: VOTED [?]
    ## 
    ##    VOTED pred_m2       n
    ##   <fctr>   <lgl>   <int>
    ## 1      0   FALSE  531489
    ## 2      0    TRUE  487491
    ## 3      1   FALSE  322372
    ## 4      1    TRUE  874080
    ## 5     NA   FALSE 1863420
    ## 6     NA    TRUE  909755
    ## 7     NA      NA    1625

``` r
#find miscalculation rate
false_pos <- confusion_mat2[2, 3]
false_neg <- confusion_mat2[3, 3]
total_obs <- nrow(test_voter)
mcr2 <- (false_pos + false_neg)/total_obs
mcr2
```

    ##           n
    ## 1 0.1622896

### Model 3: vote ~ age + party + county

``` r
#fit model 3 to full data set
m3 <- glm(VOTED ~ AGE + PARTY_CODE + COUNTY.x, data = voter_tidy, family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
#fit model 3 to training set
m3_t <- glm(VOTED ~ AGE + PARTY_CODE + COUNTY.x, data = train_voter, family = binomial)

#predicting variables
y3 <- predict(m3_t, newdata = test_voter, type = "response")

test_voter <- test_voter %>%
  mutate(p_hat3 = y3,
         pred_m3 = p_hat3 > .5)

#form confusion matrix
confusion_mat3 <- test_voter %>%
  group_by(VOTED, pred_m3) %>%
  tally()
confusion_mat3
```

    ## Source: local data frame [7 x 3]
    ## Groups: VOTED [?]
    ## 
    ##    VOTED pred_m3       n
    ##   <fctr>   <lgl>   <int>
    ## 1      0   FALSE  540164
    ## 2      0    TRUE  478816
    ## 3      1   FALSE  335932
    ## 4      1    TRUE  860520
    ## 5     NA   FALSE 1856603
    ## 6     NA    TRUE  916572
    ## 7     NA      NA    1625

``` r
#find miscalculation rate
false_pos <- confusion_mat3[2, 3]
false_neg <- confusion_mat3[3, 3]
total_obs <- nrow(test_voter)
mcr3 <- (false_pos + false_neg)/total_obs
mcr3
```

    ##           n
    ## 1 0.1632686

### Model 4: vote ~ age + I(age^2)

``` r
#fitt model 4 to full data set
m4 <- glm(VOTED ~ AGE + I(AGE^2), data = voter_tidy, family = binomial)

#fit model 4 to training set
m4_t <- glm(VOTED ~ AGE + I(AGE^2), data = train_voter, family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
#predicting variables
y4 <- predict(m4_t, newdata = test_voter, type = "response")

test_voter <- test_voter %>%
  mutate(p_hat4 = y4,
         pred_m4 = p_hat4 > .5)

#form confusion matrix
confusion_mat4 <- test_voter %>%
  group_by(VOTED, pred_m4) %>%
  tally()
confusion_mat4
```

    ## Source: local data frame [7 x 3]
    ## Groups: VOTED [?]
    ## 
    ##    VOTED pred_m4       n
    ##   <fctr>   <lgl>   <int>
    ## 1      0   FALSE  549532
    ## 2      0    TRUE  469448
    ## 3      1   FALSE  352920
    ## 4      1    TRUE  843532
    ## 5     NA   FALSE 1839467
    ## 6     NA    TRUE  933708
    ## 7     NA      NA    1625

``` r
#find miscalculation rate
false_pos <- confusion_mat4[2, 3]
false_neg <- confusion_mat4[3, 3]
total_obs <- nrow(test_voter)
mcr4 <- (false_pos + false_neg)/total_obs
mcr4
```

    ##           n
    ## 1 0.1647955

### Results

``` r
broom::tidy(m1)
```

    ##          term    estimate    std.error statistic p.value
    ## 1 (Intercept) -1.78808218 4.928987e-03 -362.7687       0
    ## 2         AGE  0.03534319 8.625092e-05  409.7718       0

The coefficient estimate on `AGE` in model 1 is 0.0353, i.e. a one year increase in age corresponds with a 0.0353 increase in probability that voters would vote. This estimate is statistically significant with a p-value of 0.

``` r
broom::tidy(m2)
```

    ##              term    estimate    std.error  statistic      p.value
    ## 1     (Intercept) -1.47223396 5.313898e-03 -277.05348 0.000000e+00
    ## 2             AGE  0.03276218 8.801601e-05  372.22984 0.000000e+00
    ## 3   PARTY_CODENAV -0.64232765 3.946120e-03 -162.77448 0.000000e+00
    ## 4 PARTY_CODEOTHER -0.43668891 5.963303e-03  -73.22937 0.000000e+00
    ## 5   PARTY_CODEREP -0.06889465 3.308239e-03  -20.82517 2.559905e-96

The coefficient estimate on `AGE` in model 2 is 0.0328. Holding all other explanatory variables constant, model 2 predicts that a one year increase in age corresponds with a 0.0328 increase in probability that voters would vote.This estimate is statistically significant with a p-value of 0.

In comparison to voters affiliated with the Democratic Party, holding age constant, voters affiliated with the Republican Party are 6.89% less likely to vote, while non-affiliated voters are 6.42% less likely to vote. This means that voters affiliated with the Democratic Party are the most likely to vote out of the four groups, while non-affiliated voters are the least likely to vote.

``` r
broom::tidy(m3)
```

    ##                  term     estimate    std.error     statistic      p.value
    ## 1         (Intercept) -1.417718012 2.137464e-02  -66.32710725 0.000000e+00
    ## 2                 AGE  0.032817907 8.899114e-05  368.77725023 0.000000e+00
    ## 3       PARTY_CODENAV -0.631756474 3.973721e-03 -158.98358388 0.000000e+00
    ## 4     PARTY_CODEOTHER -0.430200394 5.989987e-03  -71.81991814 0.000000e+00
    ## 5       PARTY_CODEREP -0.051879143 3.408433e-03  -15.22081740 2.572961e-52
    ## 6      COUNTY.xBENTON  0.115765844 2.288369e-02    5.05888001 4.217261e-07
    ## 7   COUNTY.xCLACKAMAS -0.128900362 2.105223e-02   -6.12288418 9.189649e-10
    ## 8     COUNTY.xCLATSOP -0.027964643 2.480907e-02   -1.12719439 2.596603e-01
    ## 9    COUNTY.xCOLUMBIA  0.048661758 2.361652e-02    2.06049620 3.935113e-02
    ## 10       COUNTY.xCOOS -0.175701897 2.349800e-02   -7.47731169 7.585835e-14
    ## 11      COUNTY.xCROOK  0.019098542 2.834159e-02    0.67386976 5.003941e-01
    ## 12      COUNTY.xCURRY  0.260468617 2.800517e-02    9.30073418 1.394792e-20
    ## 13  COUNTY.xDESCHUTES -0.104045426 2.160155e-02   -4.81657209 1.460454e-06
    ## 14    COUNTY.xDOUGLAS -0.195592682 2.219933e-02   -8.81074732 1.243143e-18
    ## 15    COUNTY.xGILLIAM  0.365151768 6.275372e-02    5.81880698 5.926913e-09
    ## 16      COUNTY.xGRANT  0.093143855 3.848082e-02    2.42052703 1.549803e-02
    ## 17     COUNTY.xHARNEY  0.087048327 3.673500e-02    2.36962911 1.780594e-02
    ## 18 COUNTY.xHOOD RIVER  0.123613819 2.821920e-02    4.38048617 1.184148e-05
    ## 19    COUNTY.xJACKSON -0.112526559 2.146702e-02   -5.24183366 1.589887e-07
    ## 20  COUNTY.xJEFFERSON -0.047843837 2.929088e-02   -1.63340391 1.023841e-01
    ## 21  COUNTY.xJOSEPHINE  0.041741224 2.250017e-02    1.85515161 6.357458e-02
    ## 22    COUNTY.xKLAMATH  0.017280094 2.329084e-02    0.74192661 4.581318e-01
    ## 23       COUNTY.xLAKE  0.134930114 3.924728e-02    3.43794827 5.861397e-04
    ## 24       COUNTY.xLANE  0.056813392 2.109754e-02    2.69289221 7.083517e-03
    ## 25    COUNTY.xLINCOLN -0.068552957 2.414934e-02   -2.83870925 4.529641e-03
    ## 26       COUNTY.xLINN -0.229077795 2.211198e-02  -10.35989472 3.773772e-25
    ## 27    COUNTY.xMALHEUR -0.169566748 2.769879e-02   -6.12181083 9.251782e-10
    ## 28     COUNTY.xMARION -0.119707033 2.168153e-02   -5.52115216 3.367840e-08
    ## 29     COUNTY.xMORROW  0.155347107 3.594664e-02    4.32160331 1.548995e-05
    ## 30  COUNTY.xMULTNOMAH -0.011561412 2.086497e-02   -0.55410631 5.795061e-01
    ## 31       COUNTY.xPOLK -0.079376769 2.295748e-02   -3.45755585 5.450992e-04
    ## 32    COUNTY.xSHERMAN  0.132160906 6.474814e-02    2.04115357 4.123557e-02
    ## 33  COUNTY.xTILLAMOOK  0.102138000 2.685494e-02    3.80332223 1.427685e-04
    ## 34   COUNTY.xUMATILLA -0.336311286 2.342324e-02  -14.35801696 9.490399e-47
    ## 35      COUNTY.xUNION  0.000335006 2.664769e-02    0.01257167 9.899695e-01
    ## 36    COUNTY.xWALLOWA  0.351133463 3.900195e-02    9.00297203 2.196883e-19
    ## 37      COUNTY.xWASCO  0.057002446 2.739477e-02    2.08077823 3.745421e-02
    ## 38 COUNTY.xWASHINGTON -0.157243796 2.092989e-02   -7.51288084 5.784017e-14
    ## 39    COUNTY.xWHEELER  0.029608558 7.267203e-02    0.40742715 6.836943e-01
    ## 40    COUNTY.xYAMHILL -0.041621930 2.243453e-02   -1.85526218 6.355880e-02

The coefficient estimate on `AGE` in model 3 is 0.0328. Holding all other explanatory variables constant, model 3 predicts that a one year increase in age corresponds with a 0.0328 increase in probability that voters would vote.This estimate is statistically significant with a p-value of 0. Besides Wallowa County and Umatilla County, coefficient estimates on other counties hover around 0. This means that compared to Baker County, turnout in other counties are not drastically different. The coefficient on Wallowa County is 0.351, i.e. compared to Baker County, voters are 35.1% more likely to vote. The coefficient on Umatilla County is -0.336, i.e. compared to Baker County, voters are 33.6% less likely to vote.

``` r
broom::tidy(m4)
```

    ##          term      estimate    std.error  statistic p.value
    ## 1 (Intercept) -1.891602e+00 5.331914e-03 -354.76970       0
    ## 2         AGE  3.940861e-02 1.094794e-04  359.96363       0
    ## 3    I(AGE^2) -3.648508e-05 4.857793e-07  -75.10628       0

The coefficient estimate on `AGE` in model 4 is `3.94*10^-2`. Holding all other explanatory variables constant, model 3 predicts that a one year increase in age corresponds with a `3.94*10^-2` increase in probability that voters would vote. The coefficient estimate on `AGE^2` is `-3.649*10^5` which implies that the positive relationship between `AGE` and `VOTED` diminishes as `AGE` increase. Both estimates are statistically significant with a p-value of 0.

``` r
misclass <- matrix(c( 0.1652689,0.1622896,0.1632686,0.1647955), ncol=1, byrow=T)
colnames(misclass) <- c("Misclassification Rate")
rownames(misclass) <- c("Model 1", "Model 2", "Model 3", "Model 4")
misclass <- as.table(misclass)
misclass
```

    ##         Misclassification Rate
    ## Model 1              0.1652689
    ## Model 2              0.1622896
    ## Model 3              0.1632686
    ## Model 4              0.1647955

As shown in the above table, the misclassification rates for the 4 models are really similar, i.e. each model is just as accurate/inacurrate as the rest. Model 2 has the lowest misclassification rate, while Model 1 has the highest misclassification rate.
