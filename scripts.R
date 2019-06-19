#Configurando o diretorio de trabalho
setwd("~/Desktop/Curso_1 _BigDataAnalytics_R_e_Azure/CAP20_Projetos_FeedBack/Projeto2")
getwd()


# Pacotes e bibliotecas utilizados
library("data.table")
library("dplyr")
library("ggplot2")
library("corrplot")
library("caTools")
library("neuralnet")



# 1 - Coleta de Dados
system.time(dados_cidades <- fread("town_state.csv"))
system.time(dados_vendas <- fread("train.csv"))


# 2 - Analise exploratória inicial dos dados
# Como o volume de dados é muito grande para capacidade da minha maquina.
# Utilizarei uma amostra desses dados, utilizando 500 mil observacoes.
dados_vendas <- dados_vendas %>% 
  sample_n(size = 500000)

# Analise previa dos dados
head(dados_clientes)
head(dados_produtos)
head(dados_vendas)
head(dados_cidades)

dim(dados_vendas)
dim(dados_clientes)
dim(dados_produtos)
dim(dados_cidades)

str(dados_vendas)
str(dados_clientes)
str(dados_produtos)
str(dados_cidades)

any(is.na(dados_vendas))
any(is.na(dados_clientes))
any(is.na(dados_produtos))
any(is.na(dados_cidades))

View(dados_vendas)
View(dados_clientes)
View(dados_produtos)
View(dados_cidades)

summary(dados_vendas)


# Salvando a primeira versao do arquivo
write.csv(dados_vendas, "dados_vendas.csv")
write.csv(dados_cidades, "dados_cidades.csv")


# 3 - Analise, tratamento e transformacao dos dados

# Alterando nome das variaveis
colnames(dados_vendas) <- c('dia','loja','id_canal_venda','id_rota','id_cliente','id_produto','unidade_venda','valor_venda','unidade_dev_next_week','valor_dev_next_week','demanda_ajustada')
colnames(dados_cidades) <- c('loja','cidade','estado')

# Incluindo coluna com os dias da semana nomeados
dados_vendas$dia_semana = sapply(dados_vendas$dia, function(x){
  ifelse(x == 3 , "Quinta", 
         ifelse(x == 4 , "Sexta",
                ifelse(x == 5 , "Sabado",
                       ifelse(x == 6 , "Domingo",
                              ifelse(x == 7 , "Segunda",
                                     ifelse(x == 8 , "Terca","Quarta"))))))})

# Criando fator da variavel dia_semana_nome para analise dos dados
dados_vendas$dia_semana <- factor(dados_vendas$dia_semana, levels = c("Quinta", "Sexta", "Sabado", "Domingo","Segunda","Terca","Quarta"), labels = c("Quinta", "Sexta", "Sabado", "Domingo","Segunda","Terca","Quarta"))


# Verificando valores missing - Ok sem valores missing
any(is.na(dados_vendas))

# Avaliando dados por dia da semana
table(dados_vendas$dia_semana)

# Medidas de Tendência Central das variáveis numericas
# Com base nesta analise identificamos que os valores da media e mediana estao muito distantes e 
# tambem a diferenca entre o 3 quartil e o valor maximo.
summary(dados_vendas$unidade_venda)
summary(dados_vendas$unidade_dev_next_week)
summary(dados_vendas$demanda_ajustada)

# Analisando Quantil e Percentil das variaveis
quantil_vd <- quantile(dados_vendas$unidade_venda)
quantil_dv <- quantile(dados_vendas$unidade_dev_next_week)
quantil_da <- quantile(dados_vendas$demanda_ajustada)

quantile(dados_vendas$unidade_venda, probs = c(0.01, 0.99))
quantile(dados_vendas$unidade_venda, seq( from = 0, to = 1, by = 0.10))

quantile(dados_vendas$unidade_dev_next_week, probs = c(0.01, 0.99))
quantile(dados_vendas$unidade_dev_next_week, seq( from = 0, to = 1, by = 0.10))

quantile(dados_vendas$demanda_ajustada, probs = c(0.01, 0.99))
quantile(dados_vendas$demanda_ajustada, seq( from = 0, to = 1, by = 0.10))

#Diferença entre Q3 e Q1
IQR(dados_vendas$unidade_venda) 
IQR(dados_vendas$unidade_dev_next_week) 
IQR(dados_vendas$demanda_ajustada) 

#Verificando outliers abaixo e acima
#Abaixo utilizaremos a formula Q1 - 1,5 * IQR
quantil_vd[[2]] - (1.5 * IQR(dados_vendas$unidade_venda))
quantil_dv[[2]] - (1.5 * IQR(dados_vendas$unidade_dev_next_week))
quantil_da[[2]] - (1.5 * IQR(dados_vendas$demanda_ajustada))

#Acima utilizaremos a formula Q3 + 1,5 * IQR
quantil_vd[[4]] + (1.5 * IQR(dados_vendas$unidade_venda))
quantil_dv[[4]] + (1.5 * IQR(dados_vendas$unidade_dev_next_week))
quantil_da[[4]] + (1.5 * IQR(dados_vendas$demanda_ajustada))

# Analisando desvio padrao
sd(dados_vendas$unidade_venda) #Desvio padrao muito alto
sd(dados_vendas$unidade_dev_next_week) #Desvio padrao aceitavel
sd(dados_vendas$demanda_ajustada) #Desvio padrao muito alto

# Boxplot
# Observe que pelo boxplot podemos identificar muitos outiers
# Leitura de Baixo para Cima - Q1, Q2 (Mediana) e Q3
boxplot(dados_vendas$unidade_venda, main = "Boxplot Vendas", ylab = "QT Vendas")
boxplot(dados_vendas$unidade_dev_next_week, main = "Boxplot Devolucao", ylab = "QT Devolucao")
boxplot(dados_vendas$demanda_ajustada, main = "Boxplot Demanda Ajustada", ylab = "Dem Ajustada")


# Vou criar um modelo SEM tratar nenhuma variavel para avaliar a acuracia antes e depois do 
# tratamento das variaveis.
# Podemos observar que a acuracia do modelo foi boa, porem há muitos graus de liberdade
# e muitos outliers nesses dados que ainda nao foram tratados o que pode prejudicar a 
# generalizacao do modelo.

modelo_lm_v1 <- lm(demanda_ajustada ~. , data = dados_vendas)
modelo_lm_v2 <- lm(demanda_ajustada ~ dia + unidade_venda + id_produto + valor_venda + unidade_dev_next_week + valor_dev_next_week, data = dados_vendas)
modelo_lm_v3 <- lm(demanda_ajustada ~ dia + unidade_venda + valor_venda + unidade_dev_next_week + valor_dev_next_week + id_rota + id_canal_venda, data = dados_vendas)

summary(modelo_lm_v1)
summary(modelo_lm_v2)
summary(modelo_lm_v3)

# Obtendo os resíduos
res <- residuals(modelo_lm_v2)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Plot dos Modelos
plot(modelo_lm_v1)
plot(modelo_lm_v2)
plot(modelo_lm_v3)



# Tratando os valores outliers

# Vou retirar do conjunto de dados valores com unidade de venda superiores a 12 e
# demanda superiores a 12
# conforme avaliado anteriormente na analise dos quartis.
# Apos retirada desses outliers ficamos com 443.750 observacoes no dataset
dados_vendas <- dados_vendas %>%
                  filter(unidade_venda <= 12)

dados_vendas <- dados_vendas %>%
  filter(demanda_ajustada <= 12)





# Medias de Tendência Central da variável estoque
# Agora podemos observar que os dados de media e mediana sao proximos.
summary(dados_vendas$demanda_ajustada)
summary(dados_vendas$unidade_venda)
summary(dados_vendas$unidade_dev_next_week)

# Analisando novamente o desvio padrao
sd(dados_vendas$unidade_venda) #Desvio padrao aceitavel
sd(dados_vendas$unidade_dev_next_week) #Desvio padrao aceitavel
sd(dados_vendas$demanda_ajustada) #Desvio padrao aceitavel

# Criando um histograma para a variavel target
ggplot(dados_vendas, aes(x = demanda_ajustada)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()


# Explorando relacionamento entre as variáveis: Matriz de Correlação
# Ha uma correlacao positiva forte entre a variavel unidade_venda x demanda_ajustada
cor(dados_vendas[c("unidade_venda", "unidade_dev_next_week", "demanda_ajustada")])


# Salvando a segunda versao do arquivo
write.csv(dados_vendas, "dados_v2.csv")


# Analisando a correlacao das variaveis
# Definindo as colunas para a análise de correlação 
cols <- c("unidade_venda", "unidade_dev_next_week", "valor_venda" ,"valor_dev_next_week","demanda_ajustada","dia")
correlacao <- cor(dados_vendas[,cols])

# Criando um corrplot
corrplot(correlacao, method = 'color')


# Verificando quantidade vendida x devolvida por dias da semana
dados_vendas %>% 
  group_by(dia_semana) %>%
  arrange(dia_semana) %>%
  summarise(unidade_venda = sum(unidade_venda),
            unidade_dev_next_week = sum(unidade_dev_next_week))
  

# Visualizando em forma de grafico
dados_vendas %>%
  group_by(dia_semana) %>%
  summarise(unidade_venda = sum(unidade_venda),
            unidade_dev_next_week = sum(unidade_dev_next_week))%>%
  melt(id = c("dia_semana")) %>%
  ggplot(aes(x = dia_semana, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="dodge") + 
  ylab("Quantidade") + 
  labs(fill = "Tipo")
  ggtitle("Quantidade Vendida x Devolvida")


# Demonstrando a quantidade de venda por estados
dados_vendas %>%
  inner_join(dados_cidades, by = 'loja') %>%
  group_by(estado) %>%
  select(estado, loja, unidade_venda) %>%
  summarise(unidade_venda = sum(unidade_venda))%>%
  ggplot(aes(x = reorder(estado, -unidade_venda), y = unidade_venda, fill = estado)) +
  geom_bar(stat = "identity", position="dodge") +
  guides(fill = FALSE) +
  xlab("Estado") +
  ylab("QT Venda") +
  ggtitle("Vendas por estado") +
  theme(axis.text = element_text(angle = 90))
  

# Demonstrando o faturamento por estados
dados_vendas %>%
  inner_join(dados_cidades, by = 'loja') %>%
  group_by(estado) %>%
  select(estado, loja, valor_venda) %>%
  summarise(valor_venda = sum(valor_venda))%>%
  ggplot(aes(x = reorder(estado, -valor_venda), y = valor_venda, fill = estado)) +
  geom_bar(stat = "identity", position="dodge") +
  guides(fill = FALSE) +
  xlab("Estado") +
  ylab("Faturamento") +
  ggtitle("Faturamento por estado") +
  theme(axis.text = element_text(angle = 90))




# Criando e avaliando os modelos preditivos

# Criando dados de treino e de teste (70% e 30% respectivamente)
amostra <- sample.split(dados_vendas$demanda_ajustada, SplitRatio = 0.70)
treino = subset(dados_vendas, amostra == TRUE)
teste = subset(dados_vendas, amostra == FALSE)

# Modelo LM Em torno de 98% de acuracia
modelo_lm <- lm(demanda_ajustada ~ dia + unidade_venda + valor_venda + unidade_dev_next_week + valor_dev_next_week, data = dados_vendas)

summary(modelo_lm)

previsao_lm <- predict(modelo_lm, teste)


# Visualizando os valores previstos e observados, tratando valores negativos e gerando um gráfico para
# demonstrar a quantidade de erros e acertos do modelo.
resultados <- cbind(teste$demanda_ajustada,round(previsao_lm)) 
colnames(resultados) <- c('Real','Previsto')
resultados <- as.data.frame(resultados)
View(resultados)

# Verificar se ha valores negativos
min(resultados)

# Funcao para tratar valores negativos
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
View(round(resultados$Previsto))

# Incluindo uma coluna com ERRO e ACERTO do modelo para exibirmos graficos
func <- function(x , y){
  if(x == y){
    "ACERTO"
  }else{
    "ERRO"
  }}

resultados$tipo = mapply(func,resultados$Real,resultados$Previsto)
View(resultados)

resultados %>% 
  group_by(tipo) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = tipo, y = total, fill = tipo)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(label = total)) +
  guides(fill = FALSE) +
  ylab("QT Observacoes") +
  ggtitle("Erros x Acertos do Modelo - 98% de Acertos")



# Modelo Rede Neural 99.9 de acuracia (muito bom)
# Devido ao modelo de rede neural ser muito pesado para testes e minha capacidade
# computacional nao é alta, então vou reduzir o tamanho do dataset para 30 mil observacoes
dados_vendas <- dados_vendas %>% 
  sample_n(size = 30000)


# Normalizando as variaveis preditoras

maxs <- apply(dados_vendas[, cols], 2, max) 
mins <- apply(dados_vendas[, cols], 2, min)

# Imprimindo os valores
maxs
mins

# Normalizando
dados_normalizados <- dados_vendas
dados_normalizados[, cols] <- as.data.frame(scale(dados_vendas[, cols], center = mins, scale = maxs - mins))

# Gerando nova amostra dos dados normalizados
amostra <- sample.split(dados_normalizados$demanda_ajustada, SplitRatio = 0.70)
treino = subset(dados_normalizados, amostra == TRUE)
teste = subset(dados_normalizados, amostra == FALSE)

formula = "demanda_ajustada ~ dia + unidade_venda + valor_venda + unidade_dev_next_week + valor_dev_next_week"

modelo_rede_neural <- neuralnet(formula, data = treino, hidden = c(5,3), linear.output = TRUE)

plot(modelo_rede_neural)

# Fazendo previsoes com os dados de teste
previsoes_rn <- compute(modelo_rede_neural, teste)
previsoes_rn

# Convertendo os dados de previsoes e teste
previsoes_rn <- previsoes_rn$net.result * (max(dados_vendas$demanda_ajustada) - min(dados_vendas$demanda_ajustada)) + min(dados_vendas$demanda_ajustada)
dados_teste_convertidos <- (teste$demanda_ajustada) * (max(dados_vendas$demanda_ajustada) - min(dados_vendas$demanda_ajustada)) + min(dados_vendas$demanda_ajustada)


# Visualizando os valores previstos e observados
resultados <- cbind(dados_teste_convertidos,round(previsoes_rn)) 
colnames(resultados) <- c('Real','Previsto')
resultados <- as.data.frame(resultados)

# Tratando os valores menores que zero
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)



# Calculando o Mean Squared Error
MSE.nn <- sum((dados_teste_convertidos - previsoes_rn)^2)/nrow(teste)
MSE.nn

# Obtendo os erros de previsao
error.df <- data.frame(dados_teste_convertidos, previsoes_rn)
head(error.df)


# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(dados_vendas$demanda_ajustada) - resultados$Real)^2)

# R-Squared - Coeficiente de determinação
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

# Plot dos erros
library(ggplot2)
ggplot(error.df, aes(x = dados_teste_convertidos,y = previsoes_rn)) + 
  geom_point() + stat_smooth()


# Incluindo uma coluna com erros e acertos e exibindo o grafico
resultados$tipo = mapply(func,resultados$Real,resultados$Previsto)

resultados %>% 
  group_by(tipo) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = tipo, y = total, fill = tipo)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(label = total)) +
  guides(fill = FALSE) +
  ylab("QT Observacoes") +
  ggtitle("Erros x Acertos do Modelo - 98% de Acertos")
