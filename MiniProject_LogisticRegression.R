#importing libraries
library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC) # for AOC curve
library(ggplot2)
library(tidyr)
library(ROCit)

### Data Prep Exercise extract
#Problem 1 --------------------------------------------
getwd()
setwd("D:\\IITK Data Analytics\\R\\")
file='census_income.csv'
ci=read.csv(file,stringsAsFactors = F)
str(ci)
#1.1 Find out number of categories in each categorical variable.

label= names(ci)
for (chr in 1:ncol(ci)){
  if (class(ci[,chr])=='character'){
    if(names(ci)[chr]!="Y"){
      message=paste("Number of categories in ",names(ci)[chr]," : ")
      num.cat=length(unique(ci[,chr]))
      print(paste0(message,num.cat))
    }
  }
}



#1.2 Select variables which have 6 or less number of categories 
# and make (n-1) dummy variables for them.

# RELATIONSHIP,RACE, SEX SATISFY THE CONDTION 
##notice the starting empty character for each category...or else 0 everywhere
table(d$race)
ci=ci%>%
  mutate(race_AIE=as.numeric(race==" Amer-Indian-Eskimo"),
         race_API=as.numeric(race==" Asican-Pac-Islander"),
         race_Black=as.numeric(race==" Black"),
         race_White=as.numeric(race==" White")) %>%
  select(-race)
# we ignored the category which had least frequency  


table(ci$relationship) #Other-relative least frequent
ci=ci %>% 
  mutate(relationship_Husband =as.numeric (relationship == ' Husband'),
         relationship_Notin_family=as.numeric (relationship == ' Not-in-family'),
         relationship_Own_child=as.numeric (relationship == ' Own-child'),
         relationship_Unmarried=as.numeric (relationship == ' Unmarried'),
         relationship_Wife=as.numeric (relationship == ' Wife'))%>%
  select(-relationship)


table(ci$sex)
ci=ci %>% 
  mutate(sex_male = as.numeric (sex ==' Male'))%>%
  select(-sex)



# Problem 2 -----------------------------------------------------
# There were many categorical variables which had many more distinct categories and making n-1 dummy
# variables might not be a very good approach. What we can do instead is to bring down number of categories
# 1   by combining similar categories. We saw this in one of the earlier practice assignment we’ll build on top of
# that.
# Here is example code for creating dummy variables with grouping categories for variable workclass.
# Note: Grouping is done on the basis of similar behaviour across classes of target [ which is Y
#                                                                                     in this case]


round(prop.table(table(ci$workclass,ci$Y),1),1)
# you can take any category [ after grouping ] as base [the one to ignore]
ci=ci %>%
  mutate(wc_1=as.numeric(workclass==" Self-emp-inc"),
         wc_2=as.numeric(workclass==" Federal-gov"),
         wc_3=as.numeric(workclass %in% c(" Local-gov"," Self-emp-not-inc"," State-gov")),
         wc_4=as.numeric(workclass==" Private"),
         wc_5=as.numeric(workclass==" ?")) %>%
  select(-workclass)

# You can use similar methods to create dummy variables for variables [ education , marital.status, occupation
# , native.country ].Create dummy variables for rest of the categorical variables .

round(prop.table(table(ci$education,ci$Y),1),1)
ci=ci %>%
  mutate(edu_1=as.numeric(education %in% c(" 10th"," 11th"," 12th"," 7th-8th"," 9th")),
         edu_2=as.numeric(education %in% c(" 1st-4th"," 5th-6th"," Preschool")),
         edu_3=as.numeric(education %in% c(" Assoc-acdm"," HS-grad"," Some-college")),
         edu_4=as.numeric(education ==" Assoc-voc"),
         edu_5=as.numeric(education==" Bachelors"),
         edu_6=as.numeric(education==" Masters")) %>%
  select(-education)


round(prop.table(table(ci$marital.status,ci$Y),1),1)  
ci=ci %>%
  mutate(ms_1=as.numeric(marital.status==" Never-married"),
         ms_2=as.numeric(marital.status %in% c(" Married-AF-spouse"," Married-civ-spouse"))) %>%
  select(-marital.status)


round(prop.table(table(ci$occupation,ci$Y),1),1)  
ci=ci %>%
  mutate(oc_1=as.numeric(occupation==" Exec-managerial"),
         oc_2=as.numeric(occupation==" Prof-specialty"),
         oc_3=as.numeric(occupation %in% c(" Protective-serv"," Sales"," Tech-support")),
         oc_4=as.numeric(occupation %in% c(" Craft-repair"," Transport-moving")),
         oc_5=as.numeric(occupation %in% c(" Priv-house-serv"," Other-service"))) %>%
  select(-occupation)


k=round(prop.table(table(ci$native.country,ci$Y),1),1)
sort(k[,1])

ci=ci %>%
mutate(nc_1=as.numeric(native.country %in% c(" Cambodia"," France"," India",
                                             " Iran"," Japan"," Taiwan"," Yugoslavia")),
       nc_2=as.numeric(native.country %in% c(" ?"," Canada"," China"," Cuba"," England",
                                             " Germany"," Greece"," Hong"," Italy",
                                             " Philippines")),
       nc_3=as.numeric(native.country %in% c(" Hungary"," Ireland"," Poland"," Scotland",
                                             " South"," Thailand"," United-States")),
       nc_4=as.numeric(native.country %in% c(" Columbia"," Dominican-Republic",
                                             " Guatemala"," Holand-Netherlands",
                                             " Outlying-US(Guam-USVI-etc)"))) %>%
  select(-native.country)  
# Problem 3 --------------
library(dplyr)
# Although we should generaly leave numeric variables as is                                       happen in regression modules], but sometimes there is particular value which occurs too many times in a
# variable, in such cases you should make flag variables which
# take value 0,1 in accordance when that variable
# takes that particular values. Here is an example
# of creating flag variable for variable capital.gain:

# this will give % of observations where capital.gain is 0
sum(ci$capital.gain==0)/nrow(ci)
# More than 90% values are 0 , 
# lets go ahead create a flag variable for this


ci=ci %>%
  mutate(cg_flag0=as.numeric(capital.gain==0)) #1 where 0 CG

# In the same manner check for what % of 
# data capital.loss is zero and create a flag variable if this comes out
# to be high
sum(ci$capital.loss==0)/nrow(ci)
#around 95% have values 0
ci=ci %>%
  mutate(cl_flag0=as.numeric(capital.loss==0))




# Problem 4 -----------------------------
# Converting the target
# For running logistic regression your target needs to take values 0,1. 
# Right now Y takes two categorical values.
# convert them so that Y takes value when it is " >50K" and 0 otherwise
# = ci %>% 
  #mutate(ifelse(Y==' >50K'," >50K",0))

#or 
ci$Y=as.numeric(ci$Y==" >50K")

### Data Preparation ends here


###model building starts

#splitting into train and test set. not doing a validation here but it is advised to do so
set.seed(123)
s=sample(1:nrow(ci),0.75 * nrow(ci))
ci_train=ci[s,]
ci_test=ci[-s,]

#simple linear regression model to choose relevant features
fit_initial=lm(Y~.-race_API
               -wc_4
               -ms_2
               -edu_3
               -capital.loss
               -relationship_Husband,data=ci_train)
sort(vif(fit_initial),decreasing = T)[1:3]

summary(fit_initial)

#logistic model building
fit_log=glm(Y~.-race_API
            -wc_4
            -ms_2
            -edu_3
            -capital.loss
            -relationship_Husband,data=ci_train,family = 'binomial')
summary(fit_log)

fit_log=stats::step(fit_log)
formula(fit_log)

#removing the remaining larger p values >0.1 
log_fit=glm(Y ~ age + fnlwgt + education.num + capital.gain + hours.per.week + 
              race_AIE + race_Black + race_White + relationship_Notin_family + 
              relationship_Own_child + relationship_Unmarried + relationship_Wife + 
              sex_male + wc_1 + wc_2 + wc_3 + wc_5 + edu_1 + edu_2 + edu_4 + 
              edu_6 + ms_1 + oc_1 + oc_2 + oc_3 + oc_4 + oc_5 + nc_1 + 
              nc_4 + cg_flag0 + cl_flag0,
            data=ci_train,family="binomial")

  summary(log_fit)

  ci_train$score=predict(log_fit,ci_train,type="response")
    library(ggplot2)
  ggplot(ci_train,aes(x=score,y=Y,color=factor(Y)))+geom_point()+geom_jitter()  

  
  
#### getting optimal cutoff
cutoff=0.3
predicted = as.numeric(ci_train$score>cutoff)  
TP=sum(ci_train$Y==1 & predicted==1)
FP=sum(ci_train$Y==0& predicted==1)
TN=sum(ci_train$Y==0 & predicted==0)
FN=sum(ci_train$Y==1 & predicted==0)
P=TP+FN
N=FP+TN

Sn=TP/P
Sp=TN/N
Dist=sqrt((1-Sn)**2+(1-Sp)**2)
KS=Sn - (FP/N)
M=(9*FN+0.6*FP)/(1.9*(P+N))

###
###
cutoff_data=data.frame(cutoff=99,Dist=99,Accuracy=99, KS=99,M=99)
cutoffs=seq(0,1,length=1000)
for( cutoff in cutoffs){
  predicted=as.numeric(ci_train$score>cutoff)
  TP=sum(ci_train$Y==1 & predicted==1)
  FP=sum(ci_train$Y==0& predicted==1)
  TN=sum(ci_train$Y==0 & predicted==0)
  FN=sum(ci_train$Y==1 & predicted==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  Sp=TN/N
  Dist=sqrt((1-Sn)**2+(1-Sp)**2)
  Accuracy= (TP+TN)/(P+N)
  KS=Sn - (FP/N)
  M=(9*FN+0.6*FP)/(1.9*(P+N))
  cutoff_data=rbind(cutoff_data,c(cutoff,Dist,Accuracy,KS,M))
}
cutoff_data=cutoff_data[-1,]
library(tidyr)
cutoff_data %>%
  gather(Metric,Value,Dist:M) %>%
  ggplot(aes(x=cutoff,y=Value,color=Metric))+geom_line()


cutoff_dist=cutoff_data$cutoff[which.min(cutoff_data$Dist)]
cutoff_Accuracy=cutoff_data$cutoff[which.max(cutoff_data$Accuracy)]
cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)]
cutoff_M=cutoff_data$cutoff[which.min(cutoff_data$M)]



### Performance on test data
ci_test$score=predict(log_fit,ci_test,type="response")
# Confusion Matrix for dist cutoff
table(ci_test$Y,as.numeric(ci_test$score>cutoff_dist))
# Confusion Matrix for accuracy cutoff
table(ci_test$Y,as.numeric(ci_test$score>cutoff_Accuracy))
# Confusion Matrix for KS cutoff
table(ci_test$Y,as.numeric(ci_test$score>cutoff_KS))
# Confusion Matrix for M cutoff
table(ci_test$Y,as.numeric(ci_test$score>cutoff_M))
