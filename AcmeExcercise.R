library(readr) # Lectura de datos
library(VIM) # Para dibujar el patrón leer datos faltantes
library(Hmisc)# Para usar la función describe de los datos
library(dplyr) # Dividir el conjunto de datos en train y test
library(summarytools) # Resumen descriptivo de las variables
library(tidyr) # Función gather y otras para filtrar data sets
library(scales) # Para usar la función percent usada en la detección de valores nulos
library(caret) # Para crear particiones de datos (train / test)
library(corrplot) # Para dibujar la matriz de correlaciones
library(ggplot2)
library(GGally) # para hacer un ggpairs


##################################################################################
##################################################################################

# 1. Lectura de Datos


Acme_data <- read_csv("InteligenciaNegocio/AcmeExcercise/data/ACMETelephoneABT.csv", 
                             col_types = cols(customer = col_skip()))

class(Acme_data)
default.stringsAsFactors()

## 1.1. Principales Descriptivos

describe(Acme_data)
view(dfSummary(Acme_data))

# Ajustamos los valores incorrectos de los datos: Corregir NAs y unificar valores

Acme_data$regionType<-replace(Acme_data$regionType,Acme_data$regionType=="r","rural")
Acme_data$regionType<-replace(Acme_data$regionType,Acme_data$regionType=="s","suburban")
Acme_data$regionType<-replace(Acme_data$regionType,Acme_data$regionType=="t","town")
Acme_data$regionType<-replace(Acme_data$regionType,Acme_data$regionType=="unknown",NA)
Acme_data$regionType = factor(Acme_data$regionType, levels = c("town", "rural", "suburban"))

Acme_data$marriageStatus<-replace(Acme_data$marriageStatus,Acme_data$marriageStatus=="no","false")
Acme_data$marriageStatus<-replace(Acme_data$marriageStatus,Acme_data$marriageStatus=="unknown",NA)
Acme_data$marriageStatus<-replace(Acme_data$marriageStatus,Acme_data$marriageStatus=="yes","true")
Acme_data$marriageStatus <- factor(Acme_data$marriageStatus, levels = c("true", "false"))

Acme_data$creditCard<-replace(Acme_data$creditCard,Acme_data$creditCard=="f","false")
Acme_data$creditCard<-replace(Acme_data$creditCard,Acme_data$creditCard=="no","false")
Acme_data$creditCard<-replace(Acme_data$creditCard,Acme_data$creditCard=="t","true")
Acme_data$creditCard<-replace(Acme_data$creditCard,Acme_data$creditCard=="yes","true")
Acme_data$creditCard <- factor(Acme_data$creditCard, levels = c("true", "false"))

Acme_data$age[which(Acme_data$age == 0)] <- NA
Acme_data$income[which(Acme_data$income == 0)] <- NA
Acme_data$handsetAge[which(Acme_data$handsetAge < 0)] <- NA

Acme_data$occupation = factor(Acme_data$occupation)
Acme_data$children = factor(Acme_data$children)
Acme_data$smartPhone = factor(Acme_data$smartPhone)
Acme_data$creditRating = factor(Acme_data$creditRating)
Acme_data$homeOwner = factor(Acme_data$homeOwner)
Acme_data$churn = factor(Acme_data$churn)

view(dfSummary(Acme_data))# Ver el dataFrame después de los ajustes

# 1.2. Análisis y tratamiento de Datos Faltantes: 

aggr(Acme_data, numbers=T,sortVar=T, combine=F, prop=T)
md.pattern(Acme_data)

# Verificamos las variables cuyos datos faltantes superan el 25%

Faltantes <- Acme_data %>% summarize_all(funs((sum(is.na(.)) / length(.)))) %>% 
  gather() %>%  
  arrange(desc(value)) %>% 
  filter(value>0.25) %>% 
  mutate(value=percent(value))

Faltantes # Tabla de columnas con datos faltantes superiores al 25%
#DropCols <- Faltantes$key[1:5] # Obtendo los nombres de las columnas con datos faltantes superiores al 25%

# Omitimos variables cuyos faltantes superen al > 25% : occupation, regionType, marriageStatus, income y age
New_Acme <- subset(Acme_data, select =-c(1,2,3,4,6)) 
New_Acme<- na.omit(New_Acme) # Elimino 12 registros con campos faltantes inferior a 0,12%
view(dfSummary(New_Acme)) # Para visualizar el nuevo data set despu´s de omitir datos faltantes

# 1.3. Análisis de correlaciones entre variables
num = New_Acme[, -c(1,4,6,7,8,27)]
CorMatrix = round(cor(x = num, method = "pearson"), 3)
corrplot(CorMatrix, method="circle")
CorMatrix

# Se ven correlaciones superiores a 0,7 entre:
# avgBill - avgOverBundleMins: 0,77
# avgBill - avgMins: 0,72
# avgMins - avgReceivedMins: 0,83
# avgMins - avgOutCalls: 0,70

# Podemos visualizar las correlaciones identificadas

correladas <- New_Acme[,c(9,10,12,16,17,27)]

correladas %>%
  na.omit() %>%
  ggpairs(columns = 1:6, ggplot2::aes(colour=churn))

##################################################################################
##################################################################################

#2. Dividimos los datos en 70% para grupo de training y 30% para grupo de testing.
inTraining <- createDataPartition(pull(New_Acme, churn), p = .7, list = FALSE, times = 1)
df_training <- slice(New_Acme, inTraining)
df_testing <- slice(New_Acme, -inTraining)
# view(dfSummary(df_training)) # Visualizar descriptivos de Training

##
##################################################################################
##################################################################################
#3. Transformaciones


##################################################################################
##################################################################################
#4. Modelos
#Creamos dos datasets, para hacer dos modelos diferentes

##4.1.Modelo 1: Regresión logística









