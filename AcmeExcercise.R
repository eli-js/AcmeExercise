#library(readr) # Lectura de datos
library(VIM) # Para leer datos (fread) y dibujar el patrón leer datos faltantes
library(Hmisc)# Para usar la función describe de los datos
library(dplyr) # Dividir el conjunto de datos en train y test
library(summarytools) # Resumen descriptivo de las variables
library(tidyr) # Función gather y otras para filtrar data sets
library(scales) # Para usar la función percent usada en la detección de valores nulos


##################################################################################
##################################################################################

# 1. Lectura de Datos

Acme_data <- fread("C:/Users/DORAFA/Documents/InteligenciaNegocio/AcmeExcercise/data/ACMETelephoneABT.csv", 
                   drop="customer",na.strings="")
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
DropCols <- Faltantes$key[1:5] # Obtendo los nombres de las columnas con datos faltantes superiores al 25%
#x <-data.frame(DropCols[1:5])

# Omitimos variables cuyos faltantes superen al > 25% : occupation, regionType y age

New_Acme <- subset(Acme_data, -c()) 
New_Acme<- na.omit(New_Acme) # Elimino 12 registros con campos faltantes inferior a 0,12%
# view(dfSummary(New_Acme)) # Para visualizar el nuevo data set despu´s de omitir datos faltantes

##################################################################################
##################################################################################

#2. Dividimos los datos en 70% para grupo de training y 30% para grupo de testing.
inTraining <- createDataPartition(pull(New_Acme, churn), p = .7, list = FALSE, times = 1)
df_training <- slice(New_Acme, inTraining)
df_testing <- slice(New_Acme, -inTraining)
# view(dfSummary(df_training)) # Visualizar descriptivos de Training









