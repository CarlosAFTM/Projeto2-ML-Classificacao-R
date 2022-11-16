setwd("/home/carlos/FCD/BigDataRAzure/projetos")
getwd()

library(h2o)
library(ggbeeswarm)

#Linha opcional para indicar ao H20 em que diretório está instalado o Java
#Sys.setenv(JAVA_HOME="/usr/lib/jvm/default-java") ##your own path of Java SE intalled

#Iniciando o H2O
h2o.init()

# O H2O requer que os dados estejam no formato de dataframe do H2O
h2o_frame <- as.h2o(df1)
class(h2o_frame)
head(h2o_frame)
str(h2o_frame)

# Split dos dados em treino e teste

h2o_frame_split <- h2o.splitFrame(h2o_frame, ratios = 0.70, seed = 123)
head(h2o_frame_split)

# Modelo AutoML
# Não vamos fazer balanceamento de classes (não será necessário). Para avaliação de modelos,
#o H2O utiliza como padrão a avaliação cruzada com 5 amostras (folds = 5). Tivemos ainda que 
#definir também um parâmetro para limite de tempo de processamento (**max_runtime_secs = 60 \* 10**)#
#e um limite no número de algoritmos de Machine Learning para avaliação **(XGBoost, DRF, Deeplearning)**,
#devido à limitações de capacidade de processamento de dados e memória. A métrica escolhida para avaliação
#é AUC (area sob a curva).

modelo_automl <- h2o.automl(y = 'CLASS',
                            seed = 123,
                            balance_classes = FALSE,
                            training_frame = h2o_frame_split[[1]],
                            leaderboard_frame = h2o_frame_split[[2]],
                            max_runtime_secs = 60 * 10, 
                            include_algos = c('XGBoost', 'DRF', 'DeepLearning'),
                            sort_metric = "auc")

# Extrai o leaderboard
leaderboard_automl <- as.data.frame(modelo_automl@leaderboard)
View(leaderboard_automl)

# Extrai o líder (modelo com melhor performance - XGBoost)
lider_automl <- modelo_automl@leader
lider_automl


#Verificando o desempenho do modelo na previsão dos dados de teste.

predicted <- h2o.predict(lider_automl,h2o_frame_split[[2]])
?h2o.confusionMatrix

performanceH20 <- h2o.performance(lider_automl,h2o_frame_split[[2]])
performanceH20


h2o.confusionMatrix(performanceH20)
h2o.accuracy(performanceH20,thresholds = "max")


#Métrica AUC
h2o.auc(performanceH20)
plot(performanceH20,type='roc')


#O desempenho do modelo AutoML foi superior ao modelo manual (0,98 contra 0,96 de acurácia).
#No entanto os dois modelos foram excelentes


# Para o melhor modelo extraímos a contribuição de cada variável para as previsões.
# Os valores extraídos são chamados de valores SHAP
# Usamos os dados de teste

var_contrib <- predict_contributions.H2OModel(lider_automl, h2o_frame_split[[2]])

# Preparando o dataframe
df_var_contrib <- var_contrib %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)), shap_force = mean(shap_value)) %>% 
  ungroup()


View(df_var_contrib)

# Plot da importância de cada variável para prever a variável alvo
df_var_contrib %>% 
  select(feature, shap_importance) %>%
  distinct() %>% 
  ggplot(aes(x = reorder(feature,shap_importance), y = shap_importance)) +
  geom_col(fill = '#990066') +
  coord_flip() +
  xlab(NULL) +
  ylab("Valor Médio das Métricas SHAP") +
  theme_minimal(base_size = 15)

#Verificamos assim a importância de cada variável para o modelo AutoML.

# Desliga o H2O

h2o.shutdown()
Y
