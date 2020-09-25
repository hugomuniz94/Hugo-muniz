library(readr)
library(tidyverse)
library("caret")
library("pROC")
library(ROSE)
library(nortest)
library(lmtest)
library(mice)

df <- read_csv("~/MBA/Analise Preditiva/Trabalhos/Trabalho2/email_marketing.csv", col_names = T)

# retirei as variaveis com mais de 30% de NA e que nao eram significantes para o modelo
email2 <- df[,-c(1,4,5,6,7,8,9,10,11,12,13,14,16,17,22,23,24,26,27,28,29,30)]

# transformei em fator a variavel resposta fg_clik
email2$fg_clik <- as.factor(email2$fg_clik)
email2$tp_sexo <- as.factor(email2$tp_sexo)

# apliquei um metodo de imputação para cada tipo de variavel
emailnum <- email2[,c(5:8)]
emailmult <- email2[,c(2,3)]
emailtwo <- email2[,c(1,4)]

# Imputei valores para o NA's

# vairaveis numericas
tempDatanum <- mice(emailnum, m=1, maxit=1, method='pmm', seed=500)
summary(tempDatanum)
completedDatanum <- complete(tempDatanum,1)

# variaveis do tipo factor com mais de 2 levels
tempDatamult <- mice(emailmult, m=1, maxit=1, method='polyreg', seed=500)
summary(tempDatamult)
completedDatamult <- complete(tempDatamult,1)

# variaveis factor com 2 levels
tempDatatwo <- mice(emailtwo, m=1, maxit=1, method='logreg', seed=500)
summary(tempDatatwo)
completedDatatwo <- complete(tempDatatwo,1)

# juntei os 3 datasets
email3 <- cbind(completedDatatwo, completedDatamult, completedDatanum)
str(email3)

# fiz oversampling 
data_balanced_over <- ovun.sample(fg_clik ~ ., data = email3, method = "over", p = 0.2)$data
str(data_balanced_over)
table(data_balanced_over$fg_clik)
table(is.na(data_balanced_over))

email <- data_balanced_over

# Normalizei o dados

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(email[,2:8], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, email[,2:8])
# summarize the transformed dataset
summary(transformed)

# adicionei a variavel respota ao dataset obtido
email <-cbind(transformed, email$fg_clik)

# mudei o nome da coluna
names(email)[8] <- "fg_clik"
str(email)

# transformei em factor
email$fg_clik <- as.factor(email$fg_clik)
email$tp_sexo <- as.factor(email$tp_sexo)

# treino
smp_size <- floor(0.75 * nrow(email))

## Seed
set.seed(123)

# Dividi em treino e teste

train_ind <- sample(seq_len(nrow(email)), size = smp_size)

train <- email[train_ind, ]
test <- email[-train_ind, ]

#com a variavel tot_rec o algoritmo nao converge

modelo_over <- glm(fg_clik ~ tp_camp + tot_env + tot_open + tot_clik + qtd_dias_ult_tran, data = train, family = "binomial")
summary(modelo_over)

# calculo do coeficiente de determinacao R² aproximado
1 - (summary(modelo_over)$deviance/summary(modelo_over)$null.deviance)

# analisei os pressupostos estatísticos

#normalidade
anares <- rstandard(modelo_over)
ad.test(anares)

#homocedasticidade
bptest(modelo_over)

#autocorrelacao
dwtest(modelo_over) 

# fiz a predicao
test$pred <- predict(modelo_over, test, type = "response")

# Avaliei o modelo
ROC <- roc(test$fg_clik, test$pred)
plot(ROC, col = "blue")
auc(ROC) #0.9572

str(test)

test$pred <- ifelse(test$pred > 0.5,1,0)
test$pred <- as.factor(test$pred)
table(test$pred)

confusionMatrix(data = test$pred,         
                reference = test$fg_clik, positive = "1")


