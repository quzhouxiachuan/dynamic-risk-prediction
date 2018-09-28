library(JMbayes)
library(plyr)
library(mice)
library(SciViews)

#setwd('R:/PrevMed/Faculty/Zhao/Yu/data')
setwd('/Volumes/fsmresfiles/PrevMed/Faculty/Zhao/Yu/data')
load('./LRPP_updated.RData')
D0$Time = D0$age-D0$base_age
D1 = D0[D0$COHORT %in% c('FHS OFFSPRING', 'CARDIA', 'CHS',  'ARIC', 'MESA', 'JHS') & D0$ttocvd>0 & D0$male == 0 & D0$RACE == 'White', c("ID_d","ttocvd","Time" ,"cvd", "SBP",'HDLCHL','TOTCHL','HXDIAB','SMOKER','RXHYP','base_age', 'BMINOW')]#D1 = na.omit(D1)
FHS = D0[D0$COHORT =='FHS ORIGINAL',]
###HDL measurement does not start until Time 16. Take time 16 as baseline and delete individual who has missing data at time 16 
FHS = FHS[FHS$Time>=16,]
FHS = FHS[FHS$ttocvd> 16, c("ID_d","ttocvd","Time" ,"cvd", "SBP",'HDLCHL','TOTCHL','HXDIAB','SMOKER','RXHYP','base_age', 'BMINOW') ]

D1 = rbind(D1, FHS)

#imputed <- mice(D1, m=5, maxit = 5, method = 'pmm', seed = 500)
#completedData <- complete(imputed,1)
D1_ = D1[,2:12]
imputed = mice(D1_)
complete = complete(imputed, 1)
Dk=cbind( D1[1], complete)
D1 = Dk 
D1$RXHYP_N = 1- D1$RXHYP

### get the baseline value of sbp and only remain those having changes over 0.18 compared to baseline 
BaseSBP = D1[D1$Time == 0,c('ID_d','SBP')]
colnames(BaseSBP)[2] = 'baselineSBP'
kk = merge(D1,BaseSBP,by ='ID_d',all.left = T)
kk$change = (kk$SBP - kk$baselineSBP)/kk$baselineSBP
kk = kk[kk$change>0.18,]
D1.id = kk[!duplicated(kk$ID_d),] 
D1 = D1[D1$ID_d %in% D1.id$ID_d, ]
D1.id = D1[!duplicated(D1$ID_d),] 
#D1.id = D1[!duplicated(D1$ID_d),] #sample size 1506 
#delete rows that have repeated measurements time later than survival time 
max = ddply(D1, "ID_d", summarize, max = max(ttocvd)) 
D2 = merge (D1, max, by= 'ID_d' , all.x = T)
D1 = D2[-2]
colnames(D1)[length(D1)] = 'ttocvd'
D1 = D1[D1$Time < D1$ttocvd | D1$Time == D1$ttocvd,]
D1.id = D1[!duplicated(D1$ID_d),] #sample size 1506 

###############set up training set and testing set#####################
#####training set allows NA values, while testing set does not ########
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(D1.id), size = floor(.75*nrow(D1.id)), replace = F)
D.id <- D1.id[sample, ]
D =  D1[D1$ID_d %in% D.id$ID_d,]
ND.id  <- D1.id[-sample, ]
ND = D1[D1$ID_d %in% ND.id$ID_d,]
D.id$SBP_RXHYP = ln(D.id$SBP)*D.id$RXHYP
D.id$SBP_RXHYP_N = ln(D.id$SBP)*D.id$RXHYP_N
ND.id$SBP_RXHYP = ln(ND.id$SBP)*ND.id$RXHYP
ND.id$SBP_RXHYP_N = ln(ND.id$SBP)*ND.id$RXHYP_N


multMixedFit1 <- mvglmer(list(ln(SBP) ~ Time  + (Time | ID_d),
                              ln(TOTCHL) ~ Time  + (1 | ID_d)), data = D,
                         families = list(gaussian, gaussian))

#D1.id = D1[!duplicated(D1$ID_d),]
#to use lg() and ln() function from SciViews package 
#coxfit1 = coxph(Surv(ttocvd,cvd) ~ ln(base_age)+ (ln(base_age))^2 +ln(TOTCHL)+ ln(base_age): ln(TOTCHL) + ln(HDLCHL) 
#+ ln(base_age):ln(HDLCHL) + lg(SBP) + ln(base_age):lg(SBP)+SMOKER + ln(base_age):SMOKER+HXDIAB , data=D.id,model = TRUE)
coxfit1 = coxph(Surv(ttocvd,cvd) ~ ln(base_age)+ (ln(base_age))^2 +ln(TOTCHL)+ ln(base_age): ln(TOTCHL) + ln(HDLCHL) 
                + ln(base_age):ln(HDLCHL) + ln(SBP):RXHYP + ln(SBP):RXHYP_N + SMOKER + ln(base_age):SMOKER + HXDIAB , data=D.id,model = TRUE)


coxfit2 = coxph(Surv(ttocvd,cvd) ~ ln(base_age)+ (ln(base_age))^2 +ln(TOTCHL)+ ln(base_age): ln(TOTCHL) + ln(HDLCHL) 
                + ln(base_age):ln(HDLCHL) + SBP_RXHYP + SBP_RXHYP_N + SMOKER + ln(base_age):SMOKER + HXDIAB , data=D.id,model = TRUE)



multJMFit1 <- mvJointModelBayes(multMixedFit1, coxfit1, timeVar = "Time")


## testing model 
#ND1 = na.omit(ND)
ND.id = ND[!duplicated(ND$ID_d),]

aucJM(multJMFit1, newdata=ND, Tstart=16, Thoriz = NULL, Dt = 10, idVar = 'ID_d')
#0.9187
#make Tstart time greater than the smallest repeated measurement time 
aucJM(multJMFit1, newdata=ND, Tstart=20, Thoriz = NULL, Dt = 10, idVar = 'ID_d')
#0.9059
aucJM(multJMFit1, newdata=ND, Tstart=25, Thoriz = NULL, Dt = 10, idVar = 'ID_d')
#0.7337

##aucJM for simple cox regression model 
aucJM(coxfit2, newdata= ND.id, idVar = "ID_d", respVar = "cvd", timeVar = "Time", evTimeVar = "ttocvd", Thoriz= 26, Tstart=16)
#0.6241
aucJM(coxfit2, newdata= ND.id, idVar = "ID_d", respVar = "cvd", timeVar = "Time", evTimeVar = "ttocvd", Thoriz= 30, Tstart=20)
#0.6693 (133)
aucJM(coxfit2, newdata= ND.id, idVar = "ID_d", respVar = "cvd", timeVar = "Time", evTimeVar = "ttocvd", Thoriz= 35, Tstart=25)
# 0.7316 (81)
