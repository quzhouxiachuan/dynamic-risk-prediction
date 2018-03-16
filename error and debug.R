library(JMbayes)
setwd('R:/PrevMed/Faculty/Zhao/Yu/data')
load('./LRPP_updated.RData')
D0$Time = D0$age-D0$base_age
D1 = D0[D0$COHORT=='FHS ORIGINAL' & D0$ttocvd>0 & D0$male == 1, c("ID_d","ttocvd","Time" ,"CVD_DTH","SBP",'HDLCHL','TOTCHL','HXDIAB','SMOKER','RXHYP','base_age')]
D1 = na.omit(D1)

D1$base_age = log(D1$base_age)
D1$SBP = log(D1$SBP)
D1$HDLCHL= log(D1$HDLCHL)
D1$TOTCHL = log(D1$TOTCHL)

multMixedFit1 <- mvglmer(list((SBP) ~ Time  + (Time | ID_d),
                              (TOTCHL) ~ Time  + (1 | ID_d)), data = D1,
                         families = list(gaussian, gaussian))

D1.id = D1[!duplicated(D1$ID_d),]
coxfit1 = coxph(Surv(ttocvd,CVD_DTH) ~ (SBP) + (HDLCHL) + base_age+ SMOKER + RXHYP:SBP + RXHYP+ SBP, data=D1.id,model = TRUE)

multJMFit1 <- mvJointModelBayes(multMixedFit1, coxfit1, timeVar = "Time")
aucJM(multJMFit1, newdata=D1, Tstart=40, Thoriz = NULL, Dt = 2, idVar = 'ID_d')
#aucJM(multJMFit1, newdata=D1, Tstart=40, Thoriz = NULL, Dt = 2, idVar = 'ID_d')
#error: Error in (function (x, vec, rightmost.closed = FALSE, all.inside = FALSE,:'vec' must be sorted non-decreasingly and not contain NAs
#aucJM(multJMFit1, newdata=D1, Tstart=2, Thoriz = NULL, Dt = 2, idVar = 'ID_d')
#error: new dataset is empty, command that generate the error message: newdata2[newdata2[[timeVar]] <= Tstart, ] bc Time
#does not start from 0 

object = multJMFit1
newdata=D1
Tstart=40
Dt = 2
idVar = 'ID_d'
## error message pops up from 
#survfitJM(object, newdata = newdata2, idVar = idVar,   survTimes = Thoriz, M = M)
#Error in (function (x, vec, rightmost.closed = FALSE, all.inside = FALSE,  : 
#                      'vec' must be sorted non-decreasingly and not contain NAs

## error : > right_rows = function (data, times, ids, Q_points) 
+ {
#  +     fids <- factor(ids, levels = unique(ids))
#  +     ind <- mapply(findInterval, split(Q_points, row(Q_points)), 
#                      +                   split(times, fids))
#  +     ind[ind < 1] <- 1
#  +     rownams_id <- split(row.names(data), fids)
#  +     ind <- mapply(`[`, rownams_id, split(ind, col(ind)))
#  +     data[c(ind), ]
#  + }
#> right_rows(newdata2, newdata[[timeVar]], 
#             +            +                                 newdata2[[idVar]], GK_points_postRE)
#Show Traceback

#Rerun with Debug
#Error in (function (x, vec, rightmost.closed = FALSE, all.inside = FALSE,  : 
 #                     'vec' must be sorted non-decreasingly and not contain NAs In addition: Warning message:
  #                    In split.default(times, fids) :
   #                   data length is not a multiple of split variable




