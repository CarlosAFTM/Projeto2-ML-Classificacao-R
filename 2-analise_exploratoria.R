#define diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos")
getwd()

library(tidyverse)
library(corrgram)

#Histogramas das variáveis numéricas

ggplot(df1, aes(x = FREQUENCY)) +
  geom_histogram(binwidth = 0.5, fill = "darksalmon") +
  ggtitle("FREQUENCY")


ggplot(df1, aes(x = DESIBEL)) +
  geom_histogram(binwidth = 0.5, fill = "darksalmon") +
  ggtitle("DESIBEL")


ggplot(df1, aes(x = DISTANCE)) +
  geom_histogram(binwidth = 2.0, fill = "darksalmon") +
  ggtitle("DISTANCE")


ggplot(df1, aes(x = AIRFLOW)) +
  geom_histogram(binwidth = 0.5, fill = "darksalmon") +
  ggtitle("AIRFLOW")

#Chama a atenção o grande número de observações iguais a zero da variável AIRFLOW.

nrow(filter(df1,AIRFLOW == 0))

#Vamos criar uma coluna do tipo fator, indicando "sim" para AIRFLOW = 0  e "nao" para AIRFLOW diferente
#de 0 e relacionar às outras variáveis do dataframe. Será uma variável do tipo fator.

indice <- which(df1$AIRFLOW == 0)
df1$AFzero <- "nao"
df1[indice,which(colnames(df1)=="AFzero")] = "sim"
df1$AFzero <- as.factor(df1$AFzero)

rm(indice)

ggplot(df1, aes(x = AIRFLOW, y = FREQUENCY, color = CLASS)) +
  geom_point() +
  ggtitle("FLUXO DE AR X FREQUÊNCIA SONORA")+
  theme(legend.position = c(0.9,0.1), legend.background = element_rect(fill = "#ccccff"))
#A extinção do incêndio está fortemente associada a níveis maiores de fluxo de ar.
#Quanto à frequência percebe-se que frequências próximas de zero ou superiores a 60 hz 
#geram menos corrente de ar.

ggplot(df1, aes(x = AIRFLOW, y = DESIBEL, color = CLASS)) +
  geom_point() +
  ggtitle("FLUXO DE AR X VOLUME SONORO")+
  theme(legend.position = c(0.9,0.1), legend.background = element_rect(fill = "#ccccff"))
#Já em relação ao volume, percebe-se claramente que volumes maiores geram maior fluxo de ar, não 
#havendo incêndios extintos com volumes abaixo de 80db.


ggplot(df1, aes(x = DISTANCE, y = AIRFLOW, group = DISTANCE)) +
  geom_boxplot(fill = "#ff33ff") +
  ggtitle("FLUXO DE AR X DISTÂNCIA")
#Em relação ao aumento da distância, percebe-se que o fluxo de ar claramente diminui.


corrgram(df1[4:7],cor.method = "pearson", panel = panel.cor)
# A correlação do volume sonoro com a fluxo de ar é positiva e a correlação da frequência com o fluxo de ar
#é negativa. A correlação mais importante é do fluxo de ar com a distância (correlação negativa).

ggplot(df1, aes(x = FREQUENCY, y = DESIBEL, color = CLASS)) +
  geom_point() +
  ggtitle("FREQUENCIA X VOLUME SONORO")+
  theme(legend.position = c(0.9,0.1), legend.background = element_rect(fill = "#ccccff"))
#Aqui fica bem claro que a maior parte dos sucessos para extinção do incêndio situam-se em frequências
# entre 20 e 50 Hz com volume alto (acima de 90 db).


prop.table(table(df1$CLASS == 1,df1$AFzero == "sim"))*100
#Percebemos que em uma pequena parcela das observações, houve extinção do incêndio mesmo com fluxo de ar igual
#a zero (0,10% das observações do dataset, aproximadamente).

#Gerando boxplots para análise das variáveis fator:

ggplot(df1, aes(x = CLASS, y = DISTANCE))+
  geom_boxplot(fill = "#0066FF") +
  ggtitle("CLASSE x DISTÂNCIA")
#Percebe-se aqui que há impacto claro da distância do aparato ao incêndio quanto ao sucesso na extinção.

ggplot(df1, aes(x = CLASS, y = SIZE, group = CLASS, fill = CLASS))+
  geom_boxplot()+
  ggtitle("CLASSE x TAMANHO")

#Percebe-se que incêndios de tamanhos menores tem maior sucesso de extinção, mas o impacto dessa variável
#parece ser menor que o da variável DISTANCE.


#verificando a distribuição de frequências da variável FUEL:

prop.table(table("Combustível" =  df1$FUEL,"Resultado" = df1$CLASS))*100
#Percebe-se que alguns combustíveis são menos suscetíveis ao sistema extintor (tiner e querosene), pois têm
#maior percentual de fracassos que sucessos.






















