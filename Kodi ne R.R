#requirement of the necessary packages that are going to help with the project.
library(PerformanceAnalytics)
library(Hmisc)
library(car)
require(bstat)
install.packages("orrcutt")
install.packages("car")
install.packages("rain")
require(car)
install.packages("skedastic")
library(skedastic)
library(car)
library(normtest)
require(lmtestrob)
library(lmtestrob)
library(strucchange)
require(strucchange)
library(sandwich)
library(corrplot)
library(tseries)
library(skedastic)
require(rainbow)
library(mctest)
library(rainbow)
require(tseries)
require(mctest)
library(orcutt)
library(RESET)
library(glmnet)
require(lmtest)
library(lmtest)
require(corrplot)
library(cor)

#Regresi i thjeshteplot(database$MS,database$CPI)
model1<- lm(CPI~GDP, data=X_economics_project_database)
#Kriteret Akaike 
AIC(model1)
BIC(model1)
summary(model1)
plot(X_economics_project_database$GDP, X_economics_project_database$CPI)
abline(model1, col="red")
#correlacioni
cor.test(X_economics_project_database$CPI,X_economics_project_database$GDP)
#Regresi i shumefishte
model<-lm(CPI ~ MS+UN+GDP+IR, data=X_economics_project_database)
summary(model)
#Ramsey test
resettest(model,power=1:5)
#Kriteret Akaike 
AIC(model)
BIC(model)
#modeli log-lin 
model_log_lin <- lm(log(CPI)~MS+ UN + GDP + IR, data= X_economics_project_database)
summary(model_log_lin)
#Ramsey Reset
resettest(model_log_lin)
#modeli log-log
model_log_log <- lm(log(CPI) ~ MS + log(UN) + log(GDP) + log(IR), data=X_economics_project_database)
summary(model_log_log)
#the reset test of Ramsey for the functional of the model 
resettest(model_log_log, power=1:2.5)
model2<-lm(log(CPI)~MS+UN+GDP+IR, #log -lin 
           data=X_economics_project_database)
summary(model2)
AIC(model2)
BIC(model2)
resettest(model2, power=2:4)
#Testi Chow, stability of the model 
cpit<-ts(X_economics_project_database$CPI, frequency=4, 
         start=c(2002,1))
cpit_bp<-breakpoints(cpit~1)
summary(cpit_bp)
plot(cpit)
lines(confint(cpit_bp))
sctest(log(CPI) ~ MS+log(UN)+log(GDP)+log(IR), data=X_economics_project_database, 
       type="Chow", point=16) #

#seperate the model in two parts 

#Heteroskedacisiteti
#Test Beursch-Pagan 
bptest (model_log_log)#1
# let us do just to confirme our suspicion  
white(model4)#1 
bptest(model4, ~ log(GDP) + log(UN) + I(log(GDP)^2) + I(log(UN)^2) + I(log(GDP) * log(UN)),
       data = X_economics_project_database)

#fix heteroscedasticity 


# Modeli me robust standard errors
weights <-2/ fitted(model4)^12
model4_no_hetero <- lm(log(CPI) ~ log(GDP) + log(UN), data=X_economics_project_database, weights= weights)
white(model4_no_hetero)
#based on the white test we conclude presenc of
glejser(model_log_log)#1 

#transforming the model using the log log of log log to see if it fixes 


#Multikolineariteti
rcorr(as.matrix(X_economics_project_database[2:5])) # from the 2 columb to the fifth columb 
datac= cor(X_economics_project_database[3:6], method= c("spearman"))
corrplot(datac, type = "upper", order = "hclust")
chart.Correlation(X_economics_project_database[3:6], 
                  histogram=TRUE, pch=19)
vif(model_log_log)
#Regresioni ndihmes 
modelndihmes <- lm(MS~UN+GDP+IR, 
                   data=X_economics_project_database)
summary(modelndihmes)

#testi Farrar- Glauber
mctest(model_log_log)
#Modeli i ri (heqim variablin IR)
model3<- lm(log(CPI)~log(GDP)+log(UN)+ MS, 
            data=X_economics_project_database)
summary(model3)
mctest(model3)

# Model i ri i heqim variablin (MS)
model4<- lm(log(CPI)~log(GDP)+log(UN), 
            data=X_economics_project_database)
summary(model4)
mctest(model4)
# Load necessary library
library(lmtest)   # For heteroskedasticity tests



#Autokorrelacioni
dwtest(model4) 
#heteroscedacistity
#white test
white(model4)
#glejser test
glejser(model4)
#Elemininmi i autos me metoden iterative
cochrane.orcutt(model3, convergence=8, max.iter = 
                  100)
summary(cochrane.orcutt(model3))
bgtest(cochrane.orcutt(model3))

#Testimi pers shperndarje normale te mbetejeve
res <-residuals(model4_no_hetero)
#model Q-Q
qqnorm(resid(model4_no_hetero))
plot(res, type="l")
hist(res)
boxplot(res)
jb_test <- jarque.bera.test(residuals(model4_no_hetero))
print(jb_test)
#Testi Rainbow
raintest(model4_no_hetero)
rcorr(as.matrix(database1[2:5]))
