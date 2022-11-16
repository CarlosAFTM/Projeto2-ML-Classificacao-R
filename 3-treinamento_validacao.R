#define diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos")
getwd()

library(caret)
library(ROSE)
library(corrgram)
library(tidyverse)
library(naivebayes)
library(kernlab)


#Gerando os dados de treino e de teste do modelo
set.seed(123)
training.samples <- df1$CLASS %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- df1[training.samples, ]
test.data <- df1[-training.samples, ]


prop.table(table(train.data$CLASS))
prop.table(table(test.data$CLASS))
#os datasets estão balanceados.


#para validar o modelo. Opção cv significa "cross validation - validação cruzada, neste caso com 5 combinações diferentes
control1 <- trainControl(method = "cv", number = 5)


#Modelo Random Forest

model_rf1 <- train(CLASS ~ ., 
                   seed = 123,
                   data = train.data, 
                   method = "rf", 
                   trControl = control1, 
                   metric = "Accuracy")

#Modelo Naive_Bayes

model_nb1 <- train(CLASS ~ ., 
                   seed = 123,
                   data = train.data, 
                   method = "naive_bayes", 
                   trControl = control1, 
                   metric = "Accuracy")

#Modelo SVM

model_svm1 <- train(CLASS ~ .,
                  seed = 123,
                  data = train.data, 
                  method = "svmLinear", 
                  trControl = control1, 
                  metric = "Accuracy")


#Avaliando os modelos

model_rf1$results #R2 (max Accuracy 0,96) 
model_nb1$results #R2 (max Accuracy 0,86)
model_svm1$results # (Accuracy 0,90)

#Modelo Random Forest teve a melhor acurácia (0,96), seguido pelo modelo SVM (0,90) e pelo Naive Bayes (0,86).
#Vamos utilizar o modelo Random Forest para fazer as previsões:

previsoes <- predict(model_rf1, test.data)

# Vamos verificar a acurácia

caret::confusionMatrix(test.data$CLASS, previsoes, positive = '1')
#Accuracy de 0,9677


# Calculamos o Score AUC

roc.curve(test.data$CLASS, previsoes, plotit = T, col = "#6600cc")




