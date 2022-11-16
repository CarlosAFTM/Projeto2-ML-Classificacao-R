#define diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto2")
getwd()

library(RWeka)
library(tidyverse)


#carrega dataset 
list.files()
df <- read.arff("Acoustic_Extinguisher_Fire_Dataset.arff")
View(df)
dim(df)
str(df)
sapply(df, unique)


#Variáveis

#Size: Tamanho da lata de combustível a incendiar em cm. Apesar de numérica, também é uma variável fator;
# 1 = 7cm, 2 = 12cm, 3 = 14cm, 4 = 16cm, 5 = 20c, 6 = Metade cheia GLP, 7 = Totalmente Cheia GLP. 

#Fuel: Tipo de combustível testado para extinção do incêncio;

#Distance: Distância do combustível incendiado para o mecanismo extintor em cm;

#Desibel: Volume Sonoro do mecanismo extintor em db;

#Airflow: Fluxo de ar resultante das ondas sonoras (m/s);

#Frequency: Frequência da onda sonora (Hz);

#Class: Status (1 = incêndio extinto ou 0 = não extinto).

#verifica valores NA
any(is.na(df))

#Como a variável Size tem especificações diferentes em relação ao tipo de combustível (conforme descrição),
#decidimos convertê-la para o tipo fator.

#Vamos também colocar a variável target na 1ª posição do dataframe.

colnames(df)
df1 <- df %>%
  select(CLASS,1,2,3,4,5,6)
view(df1)

df1$SIZE <- as.factor(df1$SIZE) 
str(df1)
