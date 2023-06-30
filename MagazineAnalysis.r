library(Hmisc)
library(haven)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(MASS)

rm(list = ls())

coil <- read.csv("C:/Users/wyou2/Desktop/S20.csv")
summarize(coil)

coil <- coil %>% dplyr::select(SeqNum,
MOSTYP,
MGEMLE,
MGEMOM,
MOSHOO,
MAANTH,
everything())

#L3
coil <- coil %>%
mutate_at(vars(MSKB1:MHHUUR), ~ifelse(.x==0, 0,
								ifelse(.x==1, 5.5,
								ifelse(.x==2, 17,
								ifelse(.x==3, 30,
								ifelse(.x==4, 43,
								ifelse(.x==5, 56,
								ifelse(.x==6, 69,
								ifelse(.x==7, 82,
								ifelse(.x==8, 94,
								ifelse(.x==9, 100, -99)))))))))))
head(coil)

		
#L4	
converted<-coil %>% 
  mutate(across(starts_with("P"),~ifelse(.x==0,   0,
									ifelse(.x==1,  25,
									ifelse(.x==2,   75,
									ifelse(.x==3,  150,
									ifelse(.x==4,  300,
									ifelse(.x==5,  750,
									ifelse(.x==6, 3000,
									ifelse(.x==7, 7500,
									ifelse(.x==8,15000,
									ifelse(.x==9,30000, -99))))))))))))

head(converted)
table(converted$PWAPAR, coil$PWAPAR)

								
converted <- dummy_cols(converted, select_columns = c('MOSTYP', 'MOSHOO'),remove_selected_columns = TRUE)

set.seed(1)

#create ID column
converted$id <- 1:nrow(converted)

#use 70% of dataset as training set and 30% as test set 
train <- converted %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(converted, train, by = "id")

sample <- sample(c(TRUE, FALSE), nrow(converted), replace=TRUE, prob=c(0.7,0.3))
train  <- converted[sample, ]
test   <- converted[!sample, ]

train <-subset(train,select=-c(SeqNum))
test <-subset(test,select=-c(SeqNum))

fullModel = glm(Resp ~ ., family = 'binomial', data = train) # model with all variables
nullModel = glm(Resp ~ 1, family = 'binomial', data = train) # model with intercept only

interim<-summary(stepAIC(nullModel, # start with a model containing no variables
                direction = 'forward', # run forward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection

coef<-data.frame(interim[['coefficients']])
final<-coef[coef$Pr...z..<0.05,]
#Finding Variables
print(final)

varnames<-rownames(final)
varnames<-varnames[2:length(varnames)]

finalmodel<-glm(Resp ~ MSKA+MOPLHO+MOSHOO_2+MOSTYP_32+MSKB1+AWAPAR, family = 'binomial', data = train)

#Using Linear Regression
finalmodel<-lm(Resp ~ MSKA+MOPLHO+MOSHOO_2+MOSTYP_32+MSKB1+AWAPAR, data = train)
summary(finalmodel)

test$pred<-predict(finalmodel,newdata=test,type="response")
test<-test[order(-test$pred),]
test$one<-1
test$cumprospects<-cumsum(test$one)
test$cumresp    <-cumsum(test$Resp)

Perf<-subset(test,select=c(pred,cumprospects,cumresp))
Perf$PctProspect<-Perf$cumprospects/nrow(Perf)
Perf$PctResp    <-Perf$cumresp/max(Perf$cumresp)

Perf

cutpoint<-subset(Perf,PctProspect>0.745 & PctProspect<0.755)
cutpoint

cutpoint<-subset(Perf,PctProspect>0.049 & PctProspect<0.051)
cutpoint

#Record 82 is where 4.9% of prospects crosses over into 5% of prospects
Marginal<-Perf %>%
filter(row_number()==82|row_number()==82*2|row_number()==82*3|row_number()==82*4|row_number()==82*5
|row_number()==82*6|row_number()==82*7|row_number()==82*8|row_number()==82*9|row_number()==82*10 
|row_number()==82*11|row_number()==82*12|row_number()==82*13|row_number()==82*14|row_number()==82*15
|row_number()==82*16|row_number()==82*17|row_number()==82*18|row_number()==82*19|row_number()==82*20)

Marginal


coil3<-read.csv("C:/Users/wyou2/Desktop/S20.csv")
coil3$sqrMFALLE<-(coil3$MFALLE-6)^2

strlogit<-glm(Resp~MFALLE,data=coil3,family="binomial")
summary(strlogit)
sqrlogit<-glm(Resp~sqrMFALLE,data=coil3,family="binomial")
summary(sqrlogit)

coil4<-read.csv("C:/Users/wyou2/Desktop/S20.csv")
coil4$sqrMAUT1<-(coil4$MAUT1-6)^2
	
strlogit2<-glm(Resp~MAUT1,data=coil4,family="binomial")
summary(strlogit2)
sqrlogit2<-glm(Resp~sqrMAUT1,data=coil4,family="binomial")
summary(sqrlogit2)










