>  aucJM(multJMFit, newdata=pbc2, Tstart=2, Thoriz = NULL,
+         Dt = 2, idVar = "id")

	Time-dependent AUC for the Joint Model multJMFit

Estimated AUC: 0.9199
At time: 4
Using information up to time: 2 (278 subjects still at risk)

> 
> head(pbc2)
  id    years status      drug      age    sex      year
1  1  1.09517   dead D-penicil 58.76684 female 0.0000000
2  1  1.09517   dead D-penicil 58.76684 female 0.5256817
3  2 14.15234  alive D-penicil 56.44782 female 0.0000000
4  2 14.15234  alive D-penicil 56.44782 female 0.4983025
5  2 14.15234  alive D-penicil 56.44782 female 0.9993429
6  2 14.15234  alive D-penicil 56.44782 female 2.1027270
  ascites hepatomegaly spiders                   edema
1     Yes          Yes     Yes edema despite diuretics
2     Yes          Yes     Yes edema despite diuretics
3      No          Yes     Yes                No edema
4      No          Yes     Yes                No edema
5      No          Yes     Yes                No edema
6      No          Yes     Yes                No edema
  serBilir serChol albumin alkaline  SGOT platelets
1     14.5     261    2.60     1718 138.0       190
2     21.3      NA    2.94     1612   6.2       183
3      1.1     302    4.14     7395 113.5       221
4      0.8      NA    3.60     2107 139.5       188
5      1.0      NA    3.55     1711 144.2       161
6      1.9      NA    3.92     1365 144.2       122
  prothrombin histologic status2
1        12.2          4       1
2        11.2          4       1
3        10.6          3       0
4        11.0          3       0
5        11.6          3       0
6        10.6          3       0
> head(D1)
           ID_d ttoallcvd allcvd    SBP base_age male
1 a100016012504      29.3      0 126.00       22    1
2 a100016012504      27.3      0 103.00       22    1
3 a100016012504      24.3      0 114.00       22    1
4 a100016012504       9.3      0 117.98       22    1
5 a100023004268      30.5      0 108.00       30    1
6 a100023004268      28.5      0  97.00       30    1
  black Time
1     0    0
2     0    2
3     0    5
4     0   20
5     0    0
6     0    2
> head(D0)
  age BIRTHYR HXDIAB   BMINOW GLUCOSE   TOTCHL RXCHL
1  22    1963      0 24.03079     108 161.0000    NA
2  24    1963      0 23.59952      NA 145.9566    NA
3  27    1963      0 25.82319      NA 186.0000     0
4  42    1963      0 26.32069     115 200.0000     0
5  30    1955      0 21.70754      93 198.0000    NA
6  32    1955      0 21.73136      NA 173.0214    NA
     SBP   DBP RXHYP SMOKER INCCHF TOT_DTH CVD_DTH CHD_DTH
1 126.00 95.00     0      1      0       0       0       0
2 103.00 71.00     0      1      0       0       0       0
3 114.00 72.00     0      1      0       0       0       0
4 117.98 68.23     0      1      0       0       0       0
5 108.00 74.00     0      0      0       0       0       0
6  97.00 64.00     0      0      0       0       0       0
  FNFSTRK NFMI TTODTH TTOMI TTOCHF ARIC male  GROUP CARDIA
1       0    0   29.3  29.3   29.3    0    1 CARIDA      1
2       0    0   27.3  27.3   27.3    0    1 CARIDA      1
3       0    0   24.3  24.3   24.3    0    1 CARIDA      1
4       0    0    9.3   9.3    9.3    0    1 CARIDA      1
5       0    0   30.5  30.5   30.5    0    1 CARIDA      1
6       0    0   28.5  28.5   28.5    0    1 CARIDA      1
  CHS FHS FOF MESA JHS          ID_d black allcvd
1   0   0   0    0   0 a100016012504     0      0
2   0   0   0    0   0 a100016012504     0      0
3   0   0   0    0   0 a100016012504     0      0
4   0   0   0    0   0 a100016012504     0      0
5   0   0   0    0   0 a100023004268     0      0
6   0   0   0    0   0 a100023004268     0      0
            EDU_G ttostrk ttoallcvd base_age Time
1 high school/ged    29.3      29.3       22    0
2 high school/ged    27.3      27.3       22    2
3 high school/ged    24.3      24.3       22    5
4 high school/ged     9.3       9.3       22   20
5 college or high    30.5      30.5       30    0
6 college or high    28.5      28.5       30    2
> ?pbc2
> View(pbc2)
> View(D1)
> View(D0)
> D1 = D0[D0$CARDIA==1 & D0$ttoallcvd>0, c("ID_d","ttoallcvd","allcvd","SBP",'GLUCOSE',"base_age", "male", "black", "Time")]
> D1 = na.omit(D1)
> histogram(D0$SBP)
Hit <Return> to see next plot: 
> histogram(log(D0$SBP))
Hit <Return> to see next plot: 
> histogram(log(D0$GLUCOSE))
Hit <Return> to see next plot: 
> histogram(GLUCOSE)
Error in histogram(GLUCOSE) : object 'GLUCOSE' not found
> histogram(D0$GLUCOSE)
Hit <Return> to see next plot: 
> View(D1)
> D1 = D0[D0$CARDIA==1 & D0$ttoallcvd>0, c("ID_d","ttoallcvd","allcvd","SBP",'GLUCOSE',"base_age", "male", "black", "Time")]
> D1 = na.omit(D1)
> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | id),
+                              +log(GLUCOSE) ~ Time + base_age + male + black + (1 | id)), data = D1,
+                         + families = list(gaussian, binomial))
Error: unexpected '=' in:
"                             +log(GLUCOSE) ~ Time + base_age + male + black + (1 | id)), data = D1,
                        + families ="
> library(JMbayes)
> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | id),
+                              log(GLUCOSE) ~ Time + base_age + male + black + (1 | id)), data = D1,
+                        families = list(gaussian, binomial))
Error in `[.data.frame`(data, namesVars) : undefined columns selected
> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | ID_d),
+                              log(GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, binomial))
Error in checkForRemoteErrors(val) : 
  2 nodes produced errors; first error: Error in node y2[1]
Node inconsistent with parents

non-integer x = 4.682131non-integer x = 4.682131> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (1 | ID_d),
+                              log(GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, binomial))

> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | ID_d),
+                              log(GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, binomial))
non-integer x = 4.682131non-integer x = 4.682131Error in checkForRemoteErrors(val) : 
  2 nodes produced errors; first error: Error in node y2[1]
Node inconsistent with parents

non-integer x = 4.682131non-integer x = 4.682131> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (1 | ID_d),
+                              log(GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, binomial))
Error in checkForRemoteErrors(val) : 
  2 nodes produced errors; first error: Error in node y2[1]
Node inconsistent with parents

non-integer x = 4.682131non-integer x = 4.682131> y2[1]
Error: object 'y2' not found
> kk<- lme(list(log(SBP) ~ Time + base_age + male + black+ (1 | ID_d), data = D1)
+ )
Error in UseMethod("lme") : 
  no applicable method for 'lme' applied to an object of class "list"
> kk<- lme(log(SBP) ~ Time + base_age + male + black+ (1 | ID_d), data = D1)
Error in getGroups.data.frame(dataMix, groups) : 
  invalid formula for groups
> kk<- lme(log(SBP) ~ Time + base_age + male + black, data = D1,random=~1|ID_d)
> summary(kk)
Linear mixed-effects model fit by REML
 Data: D1 
        AIC       BIC   logLik
  -45143.07 -45086.32 22578.53

Random effects:
 Formula: ~1 | ID_d
        (Intercept)   Residual
StdDev:  0.07314355 0.08224485

Fixed effects: log(SBP) ~ Time + base_age + male + black 
               Value   Std.Error    DF  t-value p-value
(Intercept) 4.542339 0.008613005 19400 527.3814       0
Time        0.003536 0.000059328 19400  59.6091       0
base_age    0.003234 0.000327137  5092   9.8861       0
male        0.060275 0.002371084  5092  25.4210       0
black       0.056356 0.002387318  5092  23.6065       0
 Correlation: 
         (Intr) Time   base_g male  
Time     -0.077                     
base_age -0.969 -0.010              
male     -0.147  0.016  0.017       
black    -0.291  0.022  0.152  0.042

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-8.05642420 -0.58187666 -0.04087455  0.53745518  5.62613813 

Number of Observations: 24497
Number of Groups: 5096 
> multMixedFit <- mvglmer(list((SBP) ~ Time + base_age + male + black+ (Time | ID_d),
+                              (GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, binomial))
Error in checkForRemoteErrors(val) : 
  2 nodes produced errors; first error: Error in node y2[1]
Node inconsistent with parents

> multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | ID_d),
+                             log (GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
+                        families = list(gaussian, gaussian))
> 
> 
> 
> 
> summary(multMixedFit)

Call:
mvglmer(formulas = list(log(SBP) ~ Time + base_age + male + black + 
    (Time | ID_d), log(GLUCOSE) ~ Time + base_age + male + black + 
    (1 | ID_d)), data = D1, families = list(gaussian, gaussian))

Data Descriptives:
Number of Groups: 5096
Number of Observations:
                  
log(SBP)     24497
log(GLUCOSE) 24497

       DIC       pD
 -72697.05 20311.79

Random-effects covariance matrix:
              StdDev    Corr        
(Intercept)1  0.0720  (Int)1   Time1
Time1         0.0035 -0.2278        
(Intercept)2  0.0961  0.3104 -0.0039

Outcome: log(SBP) 
            PostMean  StDev StErr   2.5%  97.5% P   Rhat
(Intercept)   4.5542 0.0087 3e-04 4.5379 4.5725 0 1.0017
Time          0.0035 0.0001 0e+00 0.0034 0.0037 0 0.9995
base_age      0.0028 0.0003 0e+00 0.0022 0.0034 0 1.0003
male          0.0655 0.0024 1e-04 0.0608 0.0700 0 1.0014
black         0.0489 0.0024 1e-04 0.0440 0.0535 0 1.0022
sigma         0.0750 0.0004 0e+00 0.0741 0.0758 0 1.0003

Outcome: log(GLUCOSE) 
            PostMean  StDev StErr   2.5%  97.5% P   Rhat
(Intercept)   4.2690 0.0113 4e-04 4.2474 4.2919 0 1.0013
Time          0.0065 0.0001 0e+00 0.0063 0.0066 0 1.0002
base_age      0.0043 0.0004 0e+00 0.0035 0.0051 0 1.0011
male          0.0537 0.0032 1e-04 0.0473 0.0598 0 1.0027
black         0.0115 0.0033 1e-04 0.0055 0.0177 0 1.0001
sigma         0.1170 0.0006 0e+00 0.1158 0.1180 0 1.0003

MCMC summary:
engine: JAGS 
iterations: 28000 
adapt: 3000 
burn-in: 3000 
thinning: 50 
time: 43.3 min

> D1.id = D1[!duplicated(D1$ID_d),]
> coxfit = coxph(Surv(ttoallcvd,allcvd) ~ base_age + male + black, data=D1.id, x=TRUE)
> multJMFit <- mvJointModelBayes(multMixedFit, coxFit, timeVar = "Time")
Error in mvJointModelBayes(multMixedFit, coxFit, timeVar = "Time") : 
  object 'coxFit' not found
> 
> multJMFit <- mvJointModelBayes(multMixedFit, coxfit, timeVar = "Time")
Error in mvJointModelBayes(multMixedFit, coxfit, timeVar = "Time") : 
  'survObject' must be a 'coxph' or 'survreg' object fitted with argument 'model' set to TRUE.
> 
> coxfit = coxph(Surv(ttoallcvd,allcvd) ~ base_age + male + black, data=D1.id,model = TRUE)
> multJMFit <- mvJointModelBayes(multMixedFit, coxfit, timeVar = "Time")
There were 50 or more warnings (use warnings() to see the first 50)
> 
> 
> 
> 
> 
> 
> 
> aucJM(multJMFit, newdata=D1[1:30,], Tstart=2, Thoriz = NULL, Dt = 2, idVar = "ID_d")

	Time-dependent AUC for the Joint Model multJMFit

Estimated AUC: NaN
At time: 4
Using information up to time: 2 (7 subjects still at risk)

> summary(multJMFit)

Call:
mvJointModelBayes(mvglmerObject = multMixedFit, survObject = coxfit, 
    timeVar = "Time")

Data Descriptives:
Number of Groups: 5096		Number of events: 247 (4.8%)
Number of Observations:
  log(SBP): 24497
  log(GLUCOSE): 24497

Random-effects covariance matrix:
      StdDev    Corr        
(I)1  0.0720  (Int)1   Time1
Tim1  0.0035 -0.2278        
(I)2  0.0961  0.3104 -0.0039

Survival Outcome:
                   PostMean  StDev  StErr    2.5%  97.5%     P
base_age             0.0675 0.0172 0.0006  0.0310 0.1003 0.000
male                 0.1699 0.1254 0.0030 -0.0731 0.3963 0.174
black                0.1701 0.1276 0.0027 -0.0816 0.4233 0.200
log(SBP)_value       7.3617 0.5375 0.0170  6.3331 8.4477 0.000
log(GLUCOSE)_value   3.2014 0.4433 0.0142  2.3263 4.0517 0.000

Longitudinal Outcome: log(SBP) (family = gaussian, link = identity)
            PostMean  StDev StErr   2.5%  97.5% P
(Intercept)   4.5542 0.0087 3e-04 4.5379 4.5725 0
Time          0.0035 0.0001 0e+00 0.0034 0.0037 0
base_age      0.0028 0.0003 0e+00 0.0022 0.0034 0
male          0.0655 0.0024 1e-04 0.0608 0.0700 0
black         0.0489 0.0024 1e-04 0.0440 0.0535 0
sigma         0.0750 0.0004 0e+00 0.0741 0.0758 0

Longitudinal Outcome: log(GLUCOSE) (family = gaussian, link = identity)
            PostMean  StDev StErr   2.5%  97.5% P
(Intercept)   4.2690 0.0113 4e-04 4.2474 4.2919 0
Time          0.0065 0.0001 0e+00 0.0063 0.0066 0
base_age      0.0043 0.0004 0e+00 0.0035 0.0051 0
male          0.0537 0.0032 1e-04 0.0473 0.0598 0
black         0.0115 0.0033 1e-04 0.0055 0.0177 0
sigma         0.1170 0.0006 0e+00 0.1158 0.1180 0

MCMC summary:
iterations: 900 
burn-in: 600 
thinning: 300 
time: 2 hours
> aucJM(multJMFit, newdata=D1[1:500,], Tstart=2, Thoriz = NULL, Dt = 2, idVar = "ID_d")

	Time-dependent AUC for the Joint Model multJMFit

Estimated AUC: 0.5638
At time: 4
Using information up to time: 2 (95 subjects still at risk)

> aucJM(multJMFit, newdata=D1[1:1000,], Tstart=2, Thoriz = NULL, Dt = 2, idVar = "ID_d")
Error in GK_points_CumHaz[[i]][j, ] : subscript out of bounds
> dim(D1)
[1] 24497     9
> aucJM(multJMFit, newdata=D1, Tstart=2, Thoriz = NULL, Dt = 2, 









































#example from R tutorial 
multMixedFit <- mvglmer(list(log(serBilir) ~ year + (year | id),
                    +spiders ~ year + (1 | id)), data = pbc2,
                    + families = list(gaussian, binomial))
CoxFit <- coxph(Surv(years, status2) ~ drug + age, data = pbc2.id, model = TRUE)
multJMFit <- mvJointModelBayes(multMixedFit, CoxFit, timeVar = "year")
aucJM(multJMFit, newdata=D0[1:30,], Tstart=2, Thoriz = NULL, Dt = 2, idVar = "ID_d")

#####################real data ##############
## fit in real data starting from 2 biomarkers 
D1 = D0[D0$CARDIA==1 & D0$ttoallcvd>0, c("ID_d","ttoallcvd","allcvd","SBP",'GLUCOSE',"base_age", "male", "black", "Time")]
D1 = na.omit(D1)
#lmefit = lme(SBP ~ base_age + male + black + Time, data=D1, random=~1|ID_d)
multMixedFit <- mvglmer(list(log(SBP) ~ Time + base_age + male + black+ (Time | ID_d),
                             log(GLUCOSE) ~ Time + base_age + male + black + (1 | ID_d)), data = D1,
                       families = list(gaussian, gaussian))
summary(multMixedFit)
D1.id = D1[!duplicated(D1$ID_d),]
coxfit = coxph(Surv(ttoallcvd,allcvd) ~ base_age + male + black, data=D1.id,model = TRUE)
#summary(coxfit)
multJMFit <- mvJointModelBayes(multMixedFit, coxfit, timeVar = "Time") ##3:22-
aucJM(multJMFit, newdata=D1[1:30,], Tstart=2, Thoriz = NULL, Dt = 2, idVar = "ID_d")


##############################################################################
##############################################################################




load("LRPP.RData")
D0$Time = D0$age-D0$base_age

D1 = D0[D0$CARDIA==1 & D0$ttoallcvd>0, c("ID_d","ttoallcvd","allcvd","SBP","base_age", "male", "black", "Time")]
D1 = na.omit(D1)
lmefit = lme(SBP ~ base_age + male + black + Time, data=D1, random=~1|ID_d)
summary(lmefit)
D1.id = D1[!duplicated(D1$ID_d),]

coxfit = coxph(Surv(ttoallcvd,allcvd) ~ base_age + male + black, data=D1.id, x=TRUE)
summary(coxfit)

jointfit = jointModel(lmefit, coxfit, timeVar="Time", method="spline-PH-aGH")
summary(jointfit)

jointfitBayes <- jointModelBayes(lmefit, coxfit, timeVar="Time")
summary(jointfitBayes)

roc.pbc <- rocJM(jointfitBayes, newdata = D1, Tstart = 5, Dt = 2,idVar = "ID_d")
