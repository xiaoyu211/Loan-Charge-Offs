data=read.csv('LoanStats3c.csv')
data=data[data[,10]!='-1',-11]
data=data[complete.cases(data),]
status=1-data[,10]
attach(data)
full=glm(status~.,data=data[,c(-10)],family=binomial())
null=glm(status~1,data=data[,c(-10)],family=binomial())

pairs(data[,c(-2,-5,-6,-7,-9,-10,-11)][sample(10000),])
corre=cor(data[,c(-2,-5,-6,-7,-9,-10,-11)])
library(lattice)
levelplot(corre,col.regions = terrain.colors(20))
#check verification
par(mfrow=c(1, 3))
hist(status[verification_status=='Verified'])
hist(status[verification_status=='Source Verified'])
hist(status[verification_status=='Not Verified'])
summary(glm(status~verification_status,family=binomial))
#relevel verification
levels(data[,9])=c("Not Verified","Verified","Verified")
#
data[1,-c(-5,-6,-7,-9,-10,-11,-19)]
l2=glm(status~(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+(tot_cur_bal*avg_cur_bal):mort_acc+int_rate
+dti+delinq_2yrs*pct_tl_nvr_dlq*num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1),data=data[,-10],family=binomial)
summary(l2)
#only adjust numerical terms
l3=glm(status~(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1),data=data[,-10],family=binomial)
summary(l3)
anova(l3,l2)
1-pchisq(6.695,5)
#term interaction
l4=glm(status~(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1)
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):term
,data=data[,-10],family=binomial)
anova(l3,l4)
1-pchisq(46.55,48)
#grade interaction
l6=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):grade
,data=data[,-10],family=binomial)
anova(l3,l6)
1-pchisq(367.87,287)
summary(l6)
levels(data[,5])=c('N','N','N','N','N','N','G')
l5=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):grade
,data=data[,-10],family=binomial)
anova(l5,l6)
1-pchisq(456.47,245)
#
l7=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):emp_length
,data=data[,-10],family=binomial)
anova(l3,l7)
1-pchisq(659.18,517)
summary(l7)
#try combine every two adjacent levels
levels(data[,6])
levels(data[,6])=c('< 7 years','< 7 years','>= 7 years','< 7 years',
'< 7 years','< 7 years','< 7 years','< 7 years','>= 7 years','>= 7 years',
'>= 7 years','< 7 years')

l8=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):emp_length
,data=data[,-10],family=binomial)
anova(l8,l7)
1-pchisq(762.43,437)
#
l9=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):home_ownership
,data=data[,-10],family=binomial)
anova(l3,l9)
1-pchisq(2.9649,87)

l11=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):verification_status
,data=data[,-10],family=binomial)
anova(l3,l11)
1-pchisq(14.573,89)

l13=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):purpose
,data=data[,-10],family=binomial)
anova(l3,l13)
1-pchisq(517.23,526)

###only emp_length and grade interactions are significant
ll1=glm(status~loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75
+term+grade+emp_length+home_ownership+verification_status+purpose-1
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd+inq_last_6mths
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):grade
+(loan_amnt*installment*annual_inc*tot_cur_bal*avg_cur_bal+mort_acc
+avg_cur_bal:mort_acc+tot_cur_bal:mort_acc+int_rate
+dti+delinq_2yrs+pct_tl_nvr_dlq+num_accts_ever_120_pd
+open_acc*total_acc*num_bc_sats+percent_bc_gt_75):emp_length
,data=data[,-10],family=binomial)
anova(ll1,l8)
1-pchisq()

step(null,scope=list(lower=null,upper=ll1),direction='forward',steps=20)
final=glm(formula = status ~ int_rate + dti + tot_cur_bal + emp_length + 
    loan_amnt + total_acc + annual_inc + home_ownership + delinq_2yrs + 
    open_acc + percent_bc_gt_75 + term + inq_last_6mths + purpose + 
    verification_status + pct_tl_nvr_dlq + installment + loan_amnt:annual_inc + 
    tot_cur_bal:annual_inc + loan_amnt:installment, family = binomial(), 
    data = data[, c(-10)])
mean((predict(final)>0)==status)
summary(final)
#delete home_ownership
final2=glm(formula = status ~ int_rate + dti + tot_cur_bal + emp_length + 
    loan_amnt + total_acc + annual_inc  + delinq_2yrs + 
    open_acc + percent_bc_gt_75 + term + inq_last_6mths + purpose + 
    verification_status + pct_tl_nvr_dlq + installment + loan_amnt:annual_inc + 
    tot_cur_bal:annual_inc + loan_amnt:installment, family = binomial(), 
    data = data[, c(-10)])
summary(final2)
mean((predict(final2)>0)==status)
#remove purpose and loan_amnt
final3=glm(formula = status ~ int_rate + dti + tot_cur_bal + emp_length + 
    total_acc + annual_inc  + delinq_2yrs + 
    open_acc + percent_bc_gt_75 + term + inq_last_6mths +  
    verification_status + pct_tl_nvr_dlq + installment + loan_amnt:annual_inc + 
    tot_cur_bal:annual_inc + loan_amnt:installment, family = binomial(), 
    data = data[, c(-10)])
summary(final3)
plot(1:12,c(-0.2269,0,0.09417,0.1319,0.1354,0.1493,0.07409,0.1114,0.1535,0.1077,
0.09613,0.2298),ylab='Employment Length coeffients',xlab='Employment Length')

par(mfrow=c(1,2))
boxplot(predict(final3,type='response')[status==1])
boxplot(predict(final3,type='response')[status==0])
p=0.8
v=0
for (i in 1:100){
s=mean((predict(final3,type='response')>i/100)==status)
if (s>p){p=s;v=i/100}}
mean((predict(final3,type='response')>v)==status)