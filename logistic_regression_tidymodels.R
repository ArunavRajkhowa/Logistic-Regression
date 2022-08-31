#importing libraries
library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC) # for AOC curve
library(ggplot2)
library(tidyr)
library(ROCit)


setwd("D:\\IITK Data Analytics\\R\\Logistic-Regression\\")
## ----
rg_train=read.csv("rg_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("rg_test (1).csv",stringsAsFactors = FALSE)

glimpse(rg_train)

vis_dat(rg_train)

age_band_func=function(x){
  a1=as.numeric(substr(x,1,2))
  a2=as.numeric(substr(x,4,5))
  age=ifelse(substr(x,1,2)=="71",71,
             ifelse(x=="Unknown",NA,0.5*(a1+a2)))
  return(age)
  
}

children_func=function(x){
  x=ifelse(x=="Zero",0,substr(x,1,1))
  x=as.numeric(x)
  
  return(x)
}


fi_func=function(x){
  
  x=gsub("[<,>=]","",x)
  
  temp=data.frame(fi=x)
  
  temp=temp %>% 
    separate(fi,into=c("f1","f2")) %>% 
    mutate(f1=as.numeric(f1),
           f2=as.numeric(f2),
           fi=ifelse(is.na(f1),f2,
                     ifelse(is.na(f2),f1,0.5*(f1+f2)))) %>% 
    select(-f1,-f2)
  
  return(temp$fi)
}

rg_train$Revenue.Grid=as.numeric(rg_train$Revenue.Grid==1)

dp_pipe=recipe(Revenue.Grid~.,data=rg_train) %>% 
  update_role(REF_NO,post_code,post_area,new_role = "drop_vars") %>%
  update_role(status,occupation,occupation_partner,home_status,
                self_employed,self_employed_partner,TVarea,
                gender,region,new_role="to_dummies") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_mutate_at(age_band,fn=age_band_func) %>% 
  step_mutate_at(family_income,fn=fi_func) %>% 
  step_mutate_at(children,fn=children_func) %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>%
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=rg_test)

vis_dat(train)

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

## remove vars with vif higher than 10

for_vif=lm(Revenue.Grid~.-self_employed_X__other__
                         -self_employed_partner_X__other__
                         -Investment.in.Commudity
                         -Investment.in.Derivative
                         -Investment.in.Equity
                         -region_Scotland
                         -Portfolio.Balance,
           data=t1)

# we are using this strictly for vif values only
# this model has nothing to do with the classification problem

sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)




log_fit=glm(Revenue.Grid~.-self_employed_X__other__
            -self_employed_partner_X__other__
            -Investment.in.Commudity
            -Investment.in.Derivative
            -Investment.in.Equity
            -region_Scotland
            -Portfolio.Balance,
            data=t1,
            family = "binomial")
summary(log_fit)

log_fit=stats::step(log_fit)


####


summary(log_fit)

formula(log_fit)

#removing the remaining larger p values >0.1 
log_fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
              Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
              Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
              Online.Purchase.Amount  + status_Widowed + 
               occupation_partner_Retired + 
              self_employed_partner_Yes + TVarea_Scottish.TV,
            data=t1,family="binomial")

summary(log_fit)

#### performance on t2 with auc score

val.score=predict(log_fit,newdata = t2,type='response')

pROC::auc(pROC::roc(t2$Revenue.Grid,val.score))

### now fitting model on the entire data

for_vif=lm(Revenue.Grid~.-self_employed_X__other__
           -self_employed_partner_X__other__
           -Investment.in.Commudity
           -Investment.in.Derivative
           -Investment.in.Equity
           -region_Scotland
           -Portfolio.Balance,data=train)

sort(vif(for_vif),decreasing=T)[1:3]

summary(for_vif)

log_fit.final=glm(Revenue.Grid~.-self_employed_X__other__
                  -self_employed_partner_X__other__
                  -Investment.in.Commudity
                  -Investment.in.Derivative
                  -Investment.in.Equity
                  -region_Scotland
                  -Portfolio.Balance,data=train,family = "binomial")

summary(log_fit.final)

log_fit.final=stats::step(log_fit.final)

summary(log_fit.final)

formula(log_fit.final)

log_fit.final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
                    Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                    Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
                    Home.Loan + Online.Purchase.Amount + status_Partner + occupation_partner_Retired + 
                    self_employed_partner_Yes + TVarea_Meridian + TVarea_Scottish.TV , data=train,
                  family="binomial")
summary(log_fit.final)


### finding cutoff for hard classes

train.score=predict(log_fit.final,newdata = train,type='response')

real=train$Revenue.Grid

m = measureit(score = round(train.score,3), class = real,
                     measure = c("ACC", "SENS", "SPEC","PREC","FSCR"))

cutoff_data =data.frame(Cutoff = m$Cutoff,
                               TP=m$TP,
                               TN=m$TN,
                               FP=m$FP,
                               FN=m$FN, 
                               Depth = m$Depth,
                               Accuracy = m$ACC,
                               Sensitivity = m$SENS,
                              Specificity = m$SPEC, 
                              F1 = m$FSCR) %>% 
          mutate(P=TP+FN,
                 N=TN+FP,
                 KS=(TP/P)-(FP/N)) %>% 
          select(-P,-N) %>% 
          na.omit() %>% 
          arrange(Cutoff)


# Depth	:What portion of the observations fall on or above the cutoff.

#### visualize how these measures move across cutoffs

ggplot(cutoff_data,aes(x=Cutoff,y=KS))+geom_line()


cutoff_long=cutoff_data %>% 
  select(Cutoff,Accuracy:KS) %>% 
  gather(Measure,Value,Accuracy:KS)

ggplot(cutoff_long,aes(x=Cutoff,y=Value,color=Measure))+geom_line()

# KS plot

rocit = rocit(score = train.score, 
               class = real) 

kplot=ksplot(rocit)

# cutoff on the basis of KS

my_cutoff=ksplot$`KS Cutoff`

# Lift Chart

gtable10 = gainstable(score = train.score, 
                       class = real, 
                       ngroup = 10)

print(gtable10)

plot(gtable10, type = 1)


### submission

test.prob.score= predict(log_fit.final,newdata = test,type='response')

write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)

test.predicted=as.numeric(test.prob.score>my_cutoff)

write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)
