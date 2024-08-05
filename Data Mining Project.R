setwd("Y:/My Drive/DATA MINING/ELABORATO DATA MINING")
library(forestmodel)
library(funModeling)
library(VIM)
library(gvlma)
library(car)
library(mice)
library("olsrr")
library(ggm)
library(dplyr)
library(factorMerger)
library(MASS)
library(gam)
library(lmtest)
library(sandwich)
library(robustbase) 
contingency_table <- table(data_complete$purposeC, data_complete$emp_lengthCC)

# Esecuzione del test del chi-quadro
chi_square_result <- chisq.test(contingency_table)
chi_square_result

df2_cat = data_complete %>% select_if(is.factor)

combos <- combn(ncol(df2_cat),2)
apply(combos, 2, function(x) {
  test <- chisq.test(df2_cat[, x[1]], df2_cat[, x[2]])
  tab  <- table(df2_cat[, x[1]], df2_cat[, x[2]])
  out <- data.frame("Row" = colnames(df2_cat)[x[1]]
                    , "Column" = colnames(df2_cat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(df2_cat[,x[1]], df2_cat[,x[2]]))
                    , "u1" =length(unique(df2_cat[,x[1]]))-1
                    , "u2" =length(unique(df2_cat[,x[2]]))-1
                    , "nMinu1u2" =sum(table(df2_cat[,x[1]], 
                                            df2_cat[,x[2]]))* min(length(unique(df2_cat[,x[1]]))-1 ,
                                                                  length(unique(df2_cat[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(df2_cat[,x[1]],
                                                                    df2_cat[,x[2]]))* min(length(unique(df2_cat[,x[1]]))-1 ,
                                                                                          length(unique(df2_cat[,x[2]]))-1)) 
  )
  
  return(out)
  
}) 





b <- read.csv("Anonymize_Loan_Default_data.csv", sep=",",dec = ".",stringsAsFactors=TRUE, na.strings=c("NA","NaN", "","n/a"))
sapply(b, function(x)(sum(is.na(x))))
#MISSING VALUES ANALISI ####

missingness<- aggr(b, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(b), cex.axis=.7,gap=2)
which(is.na(b$funded_amnt))
#omittiamo la riga 55 perchè è quella che presenta tanti NA in una riga
b[-55,]
summary(b)
#OBIETTIVO principale del tuo modello ? analizzare i fattori
#che influenzano l'ammontare effettivamente finanziato (funded_amnt)  
#STARTING MODEL ####
mod1<-lm(funded_amnt~int_rate+term+emp_length+home_ownership+annual_inc
         +verification_status+dti+purpose+total_acc+revol_bal+installment+loan_amnt,data=b)
summary(mod1)
forest_model(mod1)
par(mfrow=c(2,2)) 
plot(mod1)

#campione <- b[sample(nrow(b), 3000), ]
#good_cov_complete <- na.omit(b)
#FCS PER I MISSING VALUES


#DIAGNOSTICA MISSING VALUE USANDO LE VARIABILI DEL STARTING MODEL####

ls(mod1)
mod1$terms
cov=attr(terms(mod1), "term.labels") 
cov
covdata=b[,cov]
tempData <- mice(covdata, m=1, maxit=20, meth='pmm', seed=500)
tempData

data_imputed<- complete(tempData,1)
names(data_imputed)
sapply(data_imputed, function(x)(sum(is.na(x)))) # NA counts

data_complete=cbind(data_imputed, b$funded_amnt)
names(data_complete)
names(data_complete)[13] <- "funded_amnt"
str(data_complete)
sapply(data_complete, function(x)(sum(is.na(x))))
indice_riga_con_mancanti <- which(rowSums(is.na(data_complete)) > 0)
data_complete <- data_complete[-indice_riga_con_mancanti, ]
numeric<-unlist(lapply(data_complete,is.numeric))
numeric
missingness<- aggr(data_complete, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(b), cex.axis=.7,gap=2)

##collinearitÃ  analisi e diagnostica####

ols_vif_tol(mod1)

Corr<- cor(data_complete[,numeric])#ricalcoliamo matrice correlazioni "grezze"
Corr
Corr.Par <- parcor(Corr)
Corr.Par
sapply(data_complete, function(x)(sum(is.na(x))))

#Elimino la variabile installment , e noto che anche loan_amnt Ã¨ praticamente uguale alla colonna
#della variabile dipendente 
mod1<-lm(funded_amnt~int_rate+term+emp_length+home_ownership+annual_inc
         +verification_status+dti+purpose+total_acc+revol_bal,data=data_complete)
summary(mod1)

plot(x=predict(mod1), y=data_complete$funded_amnt,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)
par(mfrow=c(2,2)) 
plot(mod1)
summary(mod1)

mod1<-lm((funded_amnt)~int_rate+term+emp_length+home_ownership+annual_inc
         +verification_status+dti+purpose+(total_acc)+revol_bal,data=data_complete)
summary(mod1)
par(mfrow=c(2,2)) 
plot(mod1)
#OPTIMAL GROUPING####
devtools::install_github("ModelOriented/factorMerger", build_vignettes = FALSE)
library(dplyr)

#Trasformiamo la variabile emp_length in binaria visto che l'optimal grouping non ci da
#buoni risultati
for (row in 1:nrow(data_complete)) {
  emp_length = data_complete[row,"emp_length"]
  if(emp_length == "< 1 year" | emp_length=="1 year" | emp_length=="2 years"
     | emp_length=="3 years" | emp_length=="4 years" | emp_length=="5 years"
     | emp_length=="6 years"| emp_length=="7 years"|emp_length=="8 years"|emp_length=="9 years") {
    data_complete[row, "emp_lengthCC"] = "<10Years"
  }
  if(emp_length =="10+ years") {
    data_complete[row, "emp_lengthCC"] = ">=10Years"
  }
  
  
}

purpose_reduced <- mergeFactors(response = data_complete$funded_amnt,
                                factor = data_complete$purpose)

plot(purpose_reduced , panel = "GIC",title = "", panelGrid = FALSE )
purposeCorrect=cutTree(purpose_reduced)
data_complete$purposeC=as.numeric(purposeCorrect)
data_complete$purposeC=as.factor(data_complete$purposeC)


emp_reduced <- mergeFactors(response = data_complete$funded_amnt,
                            factor = data_complete$emp_length)

plot(emp_reduced , panel = "GIC",title = "", panelGrid = FALSE )
empCorrect=cutTree(emp_reduced)
data_complete$emp_lengthC=as.numeric(empCorrect)
data_complete$emp_lengthC=as.factor(data_complete$emp_lengthC)

mod1<-lm((funded_amnt)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+(total_acc)+(revol_bal),data=data_complete)
summary(mod1)
par(mfrow=c(2,2)) 
plot(mod1)

#BOX COX####

any(na.omit(data_complete$funded_amnt < 0))
data_complete<-data_complete[-1,]
boxcoxreg1<-boxcox(mod1)
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda
mod1<-lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+(total_acc)+revol_bal,data=data_complete)
summary(mod1)
par(mfrow=c(2,2)) 
plot(mod1)
resettest(mod1, power = 2, type = "fitted",  data = data_complete)
# GAM ####

reg2gam<-gam((funded_amnt)~s(int_rate)+term+emp_lengthCC+home_ownership+s(annual_inc)
             +verification_status+s(dti)+purpose+s(total_acc)+s(revol_bal),data=data_complete)
par(mfrow=c(2,2)) 
summary(reg2gam)
plot(reg2gam)

data_complete$emp_lengthCC<-as.factor(data_complete$emp_lengthCC)
mod2<-lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=data_complete)
summary(mod2)
par(mfrow=c(2,2)) 
plot(mod1)

resettest(mod2, power = 2, type = "fitted",  data = data_complete)


#AIC E SBC####
step <- stepAIC(mod1, direction="both")
step
stepAIC(mod1, direction = "both", k = log(nrow(data_complete)))
drop1(mod1, test="F")


resettest(mod1, power = 2, type = "fitted",  data = data_complete)
#ETEROSCHEDASTICITÃ ####
bptest(mod2)
#USIAMO WHITE TEST PER VEDERE ETEROSCHERO..####

ncvTest(mod1)



table(b$purpose)
plot(b$purpose, b$funded_amnt)
plot(b$emp_length, b$funded_amnt)
plot(data_complete$purposeC,data_complete$funded_amnt)
plot(data_complete$emp_lengthCC,data_complete$funded_amnt)
#COOK DISTANCE E DFITTS####

influencePlot(mod1,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

data_complete[6309,]
data_complete[30072,]
data_complete[2813,]
cooksd <- cooks.distance(mod1)
cooksda=data.frame(cooksd)
summary(cooksd)

# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!
n_used=length(mod1$residuals)
cutoff <- 4/(n_used-length(mod1$coefficients)-2)

# drop influencial data
data_complete=data.frame(data_complete[cooksd < cutoff, ])  # influential row numbers

mod1<-lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=data_complete)
summary(mod1)
par(mfrow=c(2,2)) 
plot(mod1)



bptest(mod1)
#USIAMO WHITE TEST PER VEDERE ETEROSCHERO..####

ncvTest(mod1)

# Supponiamo che tu abbia un modello di regressione chiamato 'modello' e i tuoi dati siano nel dataframe 'dati'
dffits = dffits(mod1)

thresh3 = 2*sqrt(length(mod1$coefficients)/length(dffits))
dffits[dffits > thresh3]

data_complete$dffits = dffits
NODFITTS = data_complete[data_complete$dffits <= thresh3, ]
mod1<-lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=NODFITTS)
summary(mod1)
bptest(mod1)
plot(x=predict(mod1), y=NODFITTS$funded_amnt,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

#add diagonal line for estimated regression line
abline(a=0, b=1)

gvlma(mod1)
drop1(mod1, test = "F")
par(mfrow = c(2,2))
plot(mod1)
resettest(mod1, power = 2, type = "fitted",  data = NODFITTS)

#ORA CALCOLIAMO GLI STANDARD ERROR CORRETTI E INFATTI 
#SI NOTA CHE ALCUNE VARIABILI CATEGORICHE SONO SIGNIFICATIVE

#WHITE STANDARD ERROR####

coeftest(mod1, vcov=vcovHC(mod1)) 
#ORA PROVIAMO A STIMARE I PARAMETRI CON UN NUOVO STIMATORE

#robust REGRESSION  ####

control <- lmrob.control(fast.s.large.n = Inf)
modRob <- lmrob((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
+verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=NODFITTS, control = control) 

summary(modRob) 
par(mfrow=c(2,2)) 
plot(modRob)

bptest(modRob)
resettest(modRob, power = 2, type = "fitted",  data = NODFITTS)
#ROBUST INFERENCE CON BOOTSTRAP PER VEDERE SE I PARAMETRI SONO ROBUSTI####
mod1<-lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
         +verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=NODFITTS)
summary(mod1)
fit_ModSel=lm((funded_amnt^0.5)~int_rate+term+emp_lengthCC+home_ownership+annual_inc
              +verification_status+dti+purposeC+log(total_acc)+log(revol_bal+1),data=NODFITTS)
summary(fit_ModSel)

BOOT.MOD=Boot(fit_ModSel, R=1999)
summary(BOOT.MOD, high.moments=TRUE)
Confint(BOOT.MOD, level=c(.95), type="perc")
#Confint(BOOT.MOD, level=c(.95), type="norm")

hist(BOOT.MOD, legend="separate")
plot(x=predict(mod1), y=data_complete$funded_amnt,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)
(exp(fit_ModSel$coefficients[1:22])-1)*100

gvlma(fit_ModSel)

#-----------------------------------------------------------------------------
  # MODELLO LOGISTICO####

b$repay_fail<-as.numeric(b$repay_fail)
b$repay_fail
#VOGLIAMO ANALIZZARE UNA PERSONE CHE HA EFFETTUATO IL PRESTITO , ABBIA EFFETTIVAMENTE 
#RESTITUITO I SOLDI   REPAIR_FAIL=1 SE HA FALLITO IL RESTITUIMENTO DEI SOLDI
# REPAIR_FAIL=0 SE NON HA FALLITO IL RESTITUIMENTO DEI SOLDI

#data_complete$repay_fail<-b$repay_fail
modlog<-glm(formula=repay_fail~funded_amnt+int_rate+term+home_ownership+purpose+annual_inc+dti,data=b)
summary(modlog)
#diagnostica missing value con le variabili per il modello logistico####

ls(modlog)
modlog$terms
cov=attr(terms(modlog), "term.labels") 
cov
covdata=b[,cov]
tempData <- mice(covdata, m=1, maxit=20, meth='pmm', seed=500)
tempData

data_imputed<- complete(tempData,1)
names(data_imputed)
sapply(data_imputed, function(x)(sum(is.na(x)))) # NA counts

data_completeLOG=cbind(data_imputed, b$repay_fail)
names(data_completeLOG)
names(data_completeLOG)[8] <- "repay_fail"
str(data_completeLOG)

purpose_reduced <- mergeFactors(response = data_completeLOG$repay_fail,
                                factor = data_completeLOG$purpose)

plot(purpose_reduced , panel = "GIC",title = "", panelGrid = FALSE )
purposeCorrect=cutTree(purpose_reduced)
data_completeLOG$purposeC=as.numeric(purposeCorrect)
data_completeLOG$purposeC=as.factor(data_completeLOG$purposeC)

modlog<-glm(formula=repay_fail~funded_amnt+int_rate+term+home_ownership+purposeC+annual_inc+dti,data=data_completeLOG,family="binomial")
summary(modlog)
drop1(modlog, test="LRT")

data_completeLOG$predicted = predict(modlog, data_completeLOG, type="response")
data_completeLOG$predicted_y <- ifelse(data_completeLOG$predicted > 0.5,1,0)

table(observed=data_completeLOG$repay_fail, predicted=data_completeLOG$predicted_y)
prob = table(observed=data_completeLOG$repay_fail, predicted=data_completeLOG$predicted_y)/nrow(data_completeLOG)

accuracy = sum(diag(prob))
accuracy 

exp(modlog$coefficients)









