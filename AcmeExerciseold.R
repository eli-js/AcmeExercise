library(summarytools)
library(Hmisc)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(GGally)
library(car)
library(VIM)
library(caret)
library(mice)

## 1. Lectura de Datos
data <- read_csv("ACMETelephoneABT.csv",  encoding = 'UTF-8', stringsAsFactors = T)
default.stringsAsFactors()# me dice si r ha convertido automáticamente mis strings a factor

## 3. Dividimos el conjunto de train y test
inTraining <- createDataPartition(pull(data, churn), p = .7, list = FALSE, times = 1)
df <- slice(data, inTraining)
df_testing <- slice(data, -inTraining)

## 2. Principales descriptivos de las variables - Train

describe(df)
view(dfSummary(df))

### 2.1. Casos observados:

### 2.1. Ajustar datos seg?n casos observados
df$regionType<-replace(df$regionType,df$regionType=="r","rural")
df$regionType<-replace(df$regionType,df$regionType=="s","suburban")
df$regionType<-replace(df$regionType,df$regionType=="t","town")
df$regionType<-replace(df$regionType,df$regionType=="unknown",NA)

df$marriageStatus<-replace(df$marriageStatus,df$marriageStatus=="unknown",NA)
df$marriageStatus<-replace(df$marriageStatus,df$marriageStatus=="no","false")
df$marriageStatus<-replace(df$marriageStatus,df$marriageStatus=="yes","true")

df$age<-replace(df$age,df$age==0,NA)

df$handsetAge<-replace(df$handsetAge,df$handsetAge<0,NA)
df$handsetAge<-df$handsetAge/365

df$creditCard<-replace(df$creditCard,df$creditCard=="f"|df$creditCard=="no","false")
df$creditCard<-replace(df$creditCard,df$creditCard=="t"|df$creditCard=="yes","true")
view(dfSummary(df))


# Tratamiento de datos faltantes 
aggr(df, numbers=T,sortVar=T, combine=F, prop=T)
md.pattern(df)

subset_df <- subset(df, select = -c(1,2,3,4,5)) 
subset_df<- na.omit(subset_df)
view(dfSummary(subset_df))

## 3. Observando las variables numéricas
### 3.1. Distribuci?n de las variables num?ricas 
par(mfrow=c(2,3))
plot(density(subset_df$income, na.rm = T),  main ="Ingresos")
plot(density(subset_df$numHandsets, na.rm = T), main ="Cant m?viles")
plot(density(subset_df$handsetAge, na.rm = T), main ="edad m?vil")
plot(density(subset_df$currentHandsetPrice, na.rm = T), main ="$$ M?vil")
plot(density(subset_df$avgBill, na.rm = T), main ="avgFactura")

par(mfrow=c(2,3))
plot(density(subset_df$avgMins, na.rm = T),main ="avgMins")
plot(density(subset_df$avgrecurringCharge, na.rm = T),main ="avgCharge")
plot(density(subset_df$avgOverBundleMins, na.rm = T), main ="avgOverBund")
plot(density(subset_df$avgRoamCalls, na.rm = T), main ="avgRoamCalls")
plot(density(subset_df$callMinutesChangePct, na.rm = T), main ="callMinutesChangePct")
plot(density(subset_df$billAmountChangePct, na.rm = T), main ="billAmountChangePct")

par(mfrow=c(2,3))
plot(density(subset_df$avgReceivedMins, na.rm = T), main ="avgReceivedMins")
plot(density(subset_df$avgOutCalls, na.rm = T), main ="avgOutCalls")
plot(density(subset_df$avgInCalls, na.rm = T), main ="avgInCalls")
plot(density(subset_df$peakOffPeakRatio, na.rm = T), main ="peakOffPeakRatio")
plot(density(subset_df$peakOffPeakRatioChangePct, na.rm = T), main ="peakOffPeakRatioChangePct")
plot(density(subset_df$avgDroppedCalls, na.rm = T), main ="avgDroppedCalls")

par(mfrow=c(2,3))
plot(density(subset_df$lifeTime, na.rm = T), main ="lifeTime")
plot(density(subset_df$lastMonthCustomerCareCalls, na.rm = T), main ="lastMonthCustomerCareCalls")
plot(density(subset_df$numRetentionCalls, na.rm = T), main ="numRetentionCalls")
plot(density(subset_df$numRetentionOffersAccepted, na.rm = T), main ="numRetentionOffersAccepted")
plot(density(subset_df$newFrequentNumbers, na.rm = T), main ="newFrequentNumbers")

# CORRELACIONES
num = subset_df[, -c(1,5,7,8,9,28)]
M = cor(num)
corrplot(M, method="circle")


# 4. ModeloGLM


train_sub1 <- subset_df[, -c(10,11)]

glm1 = glm( churn ~ children + income + numHandsets + handsetAge + smartPhone + currentHandsetPrice + creditRating + homeOwner + creditCard + avgrecurringCharge + avgOverBundleMins + avgRoamCalls + callMinutesChangePct + billAmountChangePct + avgReceivedMins + avgOutCalls + avgInCalls + peakOffPeakRatio + peakOffPeakRatioChangePct + avgDroppedCalls + lifeTime + lastMonthCustomerCareCalls + numRetentionCalls + numRetentionOffersAccepted + newFrequentNumbers, family = binomial, data = train_sub1)
summary(glm1)

u_glm1 = update(glm1, . ~ . - children - income - numHandsets - creditRating - homeOwner - creditCard - avgRoamCalls - avgOutCalls - avgInCalls - peakOffPeakRatioChangePct - avgDroppedCalls - avgDroppedCalls - lastMonthCustomerCareCalls - numRetentionOffersAccepted - newFrequentNumbers) # Eliminamos dos predictores
anova(u_glm1, glm1, test = "Chisq")

head(predict(glm1))
head(predict(glm1, type = "response"))
head(fitted(glm1)) 

train_sub2 <- subset_df[, -c(13,17,18)]
glm2 = glm( churn ~ children + income + numHandsets + handsetAge + smartPhone + currentHandsetPrice + creditRating + homeOwner + creditCard + avgBill + avgMins  + avgInCalls + peakOffPeakRatio + peakOffPeakRatioChangePct + avgDroppedCalls + lifeTime + lastMonthCustomerCareCalls + numRetentionCalls + numRetentionOffersAccepted + newFrequentNumbers, family = binomial, data = train_sub2)
summary(glm2)





## 4.1. Ejecutamos un primer modelo y revisamos variables significativas
glm = glm( churn ~ children + income + numHandsets + handsetAge + smartPhone + currentHandsetPrice + creditRating + homeOwner + creditCard + avgBill + avgMins + avgrecurringCharge + avgOverBundleMins + avgRoamCalls + callMinutesChangePct + billAmountChangePct + avgReceivedMins + avgOutCalls + avgInCalls + peakOffPeakRatio + peakOffPeakRatioChangePct + avgDroppedCalls + lifeTime + lastMonthCustomerCareCalls + numRetentionCalls + numRetentionOffersAccepted + newFrequentNumbers, family = binomial, data = subset_df)
summary(glm)

## 4.2. Ejecutamos un nuevo modelo y solo incluyendo las variables significativas
glm2 = update(glm, . ~ . - newFrequentNumbers - numRetentionOffersAccepted - lastMonthCustomerCareCalls - peakOffPeakRatioChangePct - avgInCalls - avgRoamCalls - avgReceivedMins - avgOverBundleMins - creditCardtrue - homeOwnerTRUE - creditRatingF)
summary(glm2)





# TRANFORMACION DE VARIABLES
# Factores
subset_df$children<-factor(subset_df$children)
subset_df$smartPhone<-factor(subset_df$smartPhone)
subset_df$creditRating<-factor(subset_df$creditRating)
subset_df$homeOwner<-factor(subset_df$homeOwner)
subset_df$creditCard<-factor(subset_df$creditCard)
subset_df$churn<-factor(subset_df$churn)
#Transformo numéricas
subset_df$income<-log(1+subset_df$income)
subset_df$numHandsets<-log(1+subset_df$numHandsets)
subset_df$handsetAge<-log(1+subset_df$handsetAge)
subset_df$currentHandsetPrice<-log(1+subset_df$currentHandsetPrice)
subset_df$avgBill<-log(1+subset_df$avgBill)
subset_df$avgMins<-log(1+subset_df$avgMins)
subset_df$avgrecurringCharge<-log(1+subset_df$avgrecurringCharge)
subset_df$avgOverBundleMins<-log(1+subset_df$avgOverBundleMins)
subset_df$avgRoamCalls<-log(1+subset_df$avgRoamCalls)
subset_df$callMinutesChangePct<-log(1+subset_df$callMinutesChangePct)
subset_df$billAmountChangePct<-log(1+subset_df$billAmountChangePct)
subset_df$avgReceivedMins<-log(1+subset_df$avgReceivedMins)
subset_df$avgOutCalls<-log(1+subset_df$avgOutCalls)
subset_df$avgReceivedMins<-log(1+subset_df$avgReceivedMins)
subset_df$peakOffPeakRatio<-log(1+subset_df$peakOffPeakRatio)
subset_df$peakOffPeakRatioChangePct<-log(1+subset_df$peakOffPeakRatioChangePct)
subset_df$avgDroppedCalls<-log(1+subset_df$avgDroppedCalls)
subset_df$lifeTime<-log(1+subset_df$lifeTime)
subset_df$lastMonthCustomerCareCalls<-log(1+subset_df$lastMonthCustomerCareCalls)
subset_df$numRetentionCalls<-log(1+subset_df$numRetentionCalls)
subset_df$numRetentionOffersAccepted<-log(1+subset_df$numRetentionOffersAccepted)
subset_df$newFrequentNumbers<-log(1+subset_df$newFrequentNumbers)
view(dfSummary(subset_df))

