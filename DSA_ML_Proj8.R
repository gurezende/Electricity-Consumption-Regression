"Projeto de Conclusão do Curso de Machine Learning da Data Science Academy
- Problema de Negócio: Construir um modelo preditivo que possa prever o consumo de energia de eletrodomésticos
  com base nos dados de sensores IoT coletados."

setwd('/Users/santos@us.ibm.com/Documents/FCD/machineLearning/Projetos')

# Importando Pacotes
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(corrplot)
library(caret)

# Importando o dataset de treino
df <- read.csv('dataset/projeto8-training.csv', header = TRUE, sep=',')

# Visualizando o dataset
View(df)

######## Explorando o Dataset ##########

# Informações do dataset
str(df)
"Temos 32 colunas e 14803 observações. Praticamente todas as colunas são numéricas, com exceção de
Date, WeekStatus e Day_of_Week, apresentadas como fator."

# Resumo estatístico
summary(df)
"- As temperaturas costuma variar na base de 10 graus.
- As informações foram coletadas entre Janeiro e Maio.
- Os dias da semana estão balanceados, todos na faixa de 2000 a 2200 observações
- Para nossa variável target, temos que a média de consumo é de 98, provavelmente puxada por outliers, 
uma vez que metade dos valores (mediana) está em 60 e 75% dos valores está abaixo de 100."

# Transformar coluna date para datetime format (ano-mes-dia-hora-min-seg)
df$date <- ymd_hms(df$date)

# Criar colunas Month, Day e Hour para poder ver os períodos de maior movimento
df$month <- month(df$date)
df$day <- day(df$date)
df$hour <- hour(df$date)

# Remover coluna date
df <- df[,-1]






#----

######## Visualizando o Dataset ##########

# Visualizando todos os histogramas para variáveis numéricas
var_numericas <- select_if(df, is.numeric)
for (i in seq(1,length(var_numericas))){
  hist(var_numericas[,i], main = paste('Histograma de', colnames(var_numericas[i])), xlab = toupper(colnames(var_numericas[i])), col='gold')
}
"
* Appliances: os consumos tem assimetria positiva, tendo valores bem concentrados entre 0 e 100;
  * Temos outliers em Appliances, os quais teremos que tratar antes de modelar os dados;
* Lights: Assimetria positiva. Valores concentrados em 0;
* Nas medições de Temperatura e Umidade Relativa, algumas se assemelham a uma distribuição normal;
* Na medida T_out, os valores se concentram entre 5 e 10 graus;
* Já em RH_out, o número mais comum está na faixa dos 85 ao 95;
* rv1 r rv2 são variáveis quase constantes, com valores muito próximos entre si;
* Ainda não sabemos o quanto cada variável irá influenciar em nosso modelo, entretando, é importante verificar
como está a distribuição de cada variável a fim de extrair a maior quantidade de insights do dataset.
"


# Visualizando o mês mais movimentado
mean_month <- df %>% group_by(month) %>% summarise(mean(Appliances))
colnames(mean_month) <- c('month','mean_appliances')
ggplot(data= mean_month, aes(x= month, y= mean_appliances)) + geom_bar(stat = 'identity', fill='coral2')+
  ggtitle("Média de Consumo por Mês")
'Mês mais movimentado é Fevereiro, seguido de Abril. Entretanto, as médias não variam muito'


# Visualizando os dias do mês mais movimentado
mean_day <- df %>% group_by(day) %>% summarise(mean(Appliances))
colnames(mean_day) <- c('day','mean_appliances')
ggplot(data= mean_day, aes(x= day, y= mean_appliances)) + geom_line(stat = 'identity', col='red')+
  geom_point(col='red') + ggtitle("Média de Consumo por Dia do Mês")

sum(mean_day$mean_appliances[1:15])/15 # Média: 99.1862
sum(mean_day$mean_appliances[16:31])/15 # Média: 103.7328
'Os dias mais movimentados são 4, 14, 16 e 30. A segunda quinzena do mêsé mais movimentada, tendo média de
103 contra 99 da primeira quinzena.'

# Visualizando os dias da semana mais movimentados
mean_weekday <- df %>% group_by(Day_of_week) %>% summarise(mean(Appliances))
colnames(mean_weekday) <- c('weekday','mean_appliances')
ggplot(data= mean_weekday, aes(x= weekday, y= mean_appliances)) + geom_bar(stat = 'identity', fill='coral2') +
  ggtitle("Média de Consumo por Dia da Semana")
'Os dias mais movimentados são Segunda, Sábado e Sexta-feira.'

# Visualizando os horários mais movimentado
mean_hour <- df %>% group_by(hour) %>% summarise(mean(Appliances))
colnames(mean_hour) <- c('hour','mean_appliances')
ggplot(data= mean_hour, aes(x= hour, y= mean_appliances)) + geom_line(stat = 'identity', col='red')+
  geom_point(col='coral2') + ggtitle("Média de Consumo por Hora do Dia")
'Durante o dia, temos dois horários de pico: um menor entre 10-13hs e outro maior entre 17-19hs.
Entre 0hs e 6h da manhã, o consumo é bem pequeno. Os dados mostram-se coerentes com a realidade.'




#----

######## Limpeza e Transformação de Dados #############

# Verificando dados faltantes no dataset
sum(is.na(df))
"Não temos dados faltantes no dataset."

# Verificar correlação entre os dados, buscando por possíveis multicolinearidades
correlation <- cor(var_numericas)
corrplot(correlation, type = "upper")
corrplot(correlation)
"As variáveis de Temperaturas e Umidade Relativa estão bem correlacionadas.
NSM carrega forte correlação com a variável target, assim como lights e hour
Trabalharemos isso na fase de escolha de atributos."

# Removendo Outliers
summary(df$Appliances)
"Conforme visto nos Histogramas, a variável target Appliances está com valores muito acima da média.
Vamos remover 5% desses valores a fim de deixar o nosso dataset mais homogêneo."

# Verificando o quantile 90% (200).
q <- quantile(df$Appliances, probs = 0.90)

# Vamos remover todos os outliers acima de 250 em Appliances do Dataset. Serão 1458 observações descartadas.
length(df$Appliances[df$Appliances>q])
df_clean <- df[df$Appliances<q,]

# Comparando 2 Boxplots antes e depois da remoção de outliers
par(mfrow=c(1,2))
boxplot(df$Appliances, horizontal = TRUE, col='coral2', main= 'Boxplot Appliances com Outliers')
boxplot(df_clean$Appliances, horizontal = TRUE, col='palegreen4', main= 'Boxplot Appliances sem Outliers')

# Cria coluna com horários de pico. 1 = sim ; 0 = não
df_clean$horario_pico <- ifelse(df_clean$hour %in% c(10,11,12,13,16,17,18,19,20),1,0)

# WeekStatus para numerico
df_clean$WeekStatus <- ifelse(df_clean$WeekStatus=='Weekday',1,0)

#Day_of_week para numerico
df_clean$Day_of_week <- as.numeric(as.factor(df_clean$Day_of_week))

# Criando colunas Temperatura Media e RH Media
df_clean$t_media <- rowMeans(df_clean[,seq(3,19,2)])
df_clean$rh_media <- rowMeans(df_clean[,seq(4,20,2)])

# Correlação entre a nova coluna e nossa variável Target
cor(x=df_clean$t_media, y= df_clean$Appliances)
"Correlação de 0.23"

#---



#----

######## Engenharia de Atributos ########
"Nesta sessão vamos criar um modelo de Random Forest para verificar quais são as variáveis
mais importantes para usar no modelo final. Como os dados já estão divididos em treino e teste, não 
vou fazer divisão para esta etapa do processo."

# Carregando pacotes
library(randomForest)

df_scale <- data.frame(cbind(scale(df_clean[,-c(1,30:35)]), 
                  df_clean$WeekStatus, df_clean$Day_of_week,
                  df_clean$month, df_clean$day, df_clean$hour,
                  df_clean$horario_pico, df_clean$Appliances))

colnames(df_scale) <- c("lights","T1","RH_1","T2","RH_2","T3","RH_3","T4","RH_4","T5","RH_5",
                        "T6","RH_6","T7","RH_7","T8","RH_8","T9","RH_9","T_out","Press_mm_hg",
                        "RH_out","Windspeed","Visibility","Tdewpoint","rv1","rv2","NSM","WeekStatus",
                        "Day_of_week","month","day","hour","horario_pico", 'target')

# Criando um subset do dataset apenas para verificar Var importance.
sample <- sample.int(n = nrow(df_scale), size = floor(.3*nrow(df_scale)), replace = F)
df_scalesub <- df_scale[sample, ]


# Treinando o modelo de RF
rfmodel <- randomForest(target ~ .,
                        data = df_scalesub,
                        ntree=600,
                        importance=TRUE)
#Sys.time()

# Imprimindo a importancia das  variáveis
varImpPlot(rfmodel)
"Após criação do modelo, verificando o plot das variáveis consideradas mais importantes pelo modelo, 
decidimos por usar as seguintes variáveis:
- 'Appliances','NSM','Press_mm_hg','Windspeed',
- 'T2', 'RH_2', 'T3', 'RH_3', 'T6', 'RH_6', 'T_9', 'RH_9'
- 'Tdewpoint', 'lights','rv1', 'Day_of_week', 'horario_pico','day' and 'hour'"






#----

########### Criando os modelos ############

# Criando dataset com as variáveis escolhidas
df_myvars <- df_clean[,c('Appliances','NSM','Press_mm_hg','Windspeed', 'T2', 'RH_2', 'T3', 'RH_3',
                         'T6', 'RH_6', 'T_out', 'RH_out','Tdewpoint', 'lights','rv1', 'Day_of_week',
                         'horario_pico','day','hour')]


# Colocar os dados na mesma escala
# Criando novo dataset com dados normalizados e renomeando as colunas.
df_myvars <- as.data.frame(cbind(scale(df_myvars[,-c(1,16:19)]), df_clean$Day_of_week, df_clean$horario_pico,
                                 df_clean$day, df_clean$hour, df_clean$Appliances))

colnames(df_myvars) <- c('NSM','Press_mm_hg','Windspeed', 'T2', 'RH_2', 'T3', 'RH_3',
                         'T6', 'RH_6', 'T_out', 'RH_out','Tdewpoint', 'lights','rv1', 'Day_of_week',
                         'horario_pico','day','hour','target')



# Treinando modelo XGBoost
library(xgboost)
modelxgb <- train(data = df_myvars,
                  target ~ .,
                  method = 'xgbTree')

modelxgb

# Validação XGB
predxgb <- predict(modelxgb, validation[,-26])
maexgb = MAE(predxgb, validation$target)
rmsexgb = RMSE(predxgb, validation$target)
r2xgb = R2(predxgb, validation$target, form = "traditional")
cat(" MAE:", maexgb, "\n", "RMSE:", rmsexgb, "\n", "R-squared:", r2xgb)
--------------------------------"
Resultados: 
+ MAE: 13.67926 
+ RMSE: 20.43405 
+ R-squared: 0.5733198
--------------------------------"


# Treinando modelo SVM
library(e1071)
modelsvm <- svm(target ~ ., data= df_myvars)


# Validação SVM
pred <- predict(modelsvm, df_myvars[,-19])

maesvm = MAE(pred, df_myvars$target)
rmsesvm = RMSE(pred, df_myvars$target)
r2svm = R2(pred, df_myvars$target, form = "traditional")
cat(" MAE:", maesvm, "\n", "RMSE:", rmsesvm, "\n", "R-squared:", r2svm)

"--------------------------------
Resultados:
+ MAE: 12.96156 
+ RMSE: 20.48398 
+ R-squared: 0.5492296
--------------------------------"






#----

####### Previsões ##########
"Optei por escolher o modelo SVM, dado o seu desempenho semelhante ao XGB e por ser bem mais rápido."

comparamodelos <- data.frame(XGB=c(round(maexgb,4), round(rmsexgb,4), round(r2xgb,4)),
                             SVM=c(round(maesvm,4), round(rmsesvm,4), round(r2svm,4)),
                             row.names = c('MAE', 'RMSE', 'R-Squared'))
comparamodelos

# Carregar Dataset de Teste
teste <- read.csv('dataset/projeto8-testing.csv')

# Transformações
"Precisamos fazer as mesmas transformações nos dados. 
A seguir, vamos realizar os seguintes passos, na ordem:
1. Transformar coluna date em datetime
2. Criar colunas day e hour
3. Verificar dados faltantes
4. Criar subset com colunas 'Appliances','NSM','Press_mm_hg','Tdewpoint',
                     'Windspeed', 'T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5',
                     'T6','RH_6','T7','RH_7','T8','RH_8','T9', 'RH_9',
                     'lights', 'day','Day_of_week','hour'
5. Criar coluna cond_tempo e horário pico
5. Padronizar os dados.
6. Renomear colunas"

teste$date <- ymd_hms(teste$date)
teste$day <- day(teste$date)
teste$hour <- hour(teste$date)
sum(is.na(teste))

# Cria coluna com horários de pico. 1 = sim ; 0 = não
teste$horario_pico <- ifelse(teste$hour %in% c(10,11,12,13,16,17,18,19,20),1,0)

df_teste <- teste[,c('Appliances','NSM','Press_mm_hg','Windspeed', 'T2', 'RH_2', 'T3', 'RH_3',
                     'T6', 'RH_6', 'T_out', 'RH_out','Tdewpoint', 'lights','rv1', 'Day_of_week',
                     'horario_pico','day','hour')]
df_teste$Day_of_week <- as.numeric(as.factor(df_teste$Day_of_week))

df_teste <- as.data.frame(cbind(scale(df_teste[,-c(1,16:19)]),teste$Day_of_week, 
                                teste$horario_pico, teste$day, teste$hour, teste$Appliances))

colnames(df_teste) <- c('NSM','Press_mm_hg','Windspeed', 'T2', 'RH_2', 'T3', 'RH_3',
                     'T6', 'RH_6', 'T_out', 'RH_out','Tdewpoint', 'lights','rv1', 'Day_of_week',
                     'horario_pico','day','hour','target')

# Previsões SVM
previsoessvm <- predict(modelsvm, df_teste[,-19])

# Avaliando o modelo
mae = MAE(previsoessvm, df_teste$target)
rmse = RMSE(previsoessvm, df_teste$target)
r2 = R2(previsoessvm, df_teste$target, form = "traditional")
cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)

# Dataset Real vs. Performance
perf <- data.frame(real = df_teste$target, Prev = previsoessvm)
View(perf)


#------- --- ---

# Previsões XGB
previsoesxgb <- predict(modelxgb, df_teste[,-27])

# Avaliando o modelo
mae = MAE(previsoesxgb, df_teste$target)
rmse = RMSE(previsoesxgb, df_teste$target)
r2 = R2(previsoesxgb, df_teste$target, form = "traditional")
cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)

# Dataset Real vs. Performance
perf <- data.frame(real = df_teste$target, Prev = previsoesxgb)
View(perf)

