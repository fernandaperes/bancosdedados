
######################### MANOVA com uma VI #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(MVN)) install.packages("MVN") 
library(MVN)
if(!require(GGally)) install.packages("GGally") 
library(GGally)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 6.csv') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados

head(dados, 10)


# Passo 3: Verificação da normalidade MULTIVARIADA - por grupo:

# Teste de Shapiro-Wilk - pacote rstatix:
dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~mshapiro_test(.))


# Teste de Henze-Zirkler - pacote MVN:
mvn(data = dados[,2:4], subset = "Alcool", mvnTest = "hz")



## Verificação da normalidade UNIVARIADA - por grupo:
dados %>% group_by(Alcool) %>% 
  shapiro_test(Latencia, Memoria)


# Passo 4: Verificação da presença de outliers MULTIVARIADOS
## Pela distância de Mahalanobis (outlier = p<0,001)

dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~mahalanobis_distance(.)) %>% 
  filter(is.outlier == TRUE)


## Verificação da presença de outliers UNIVARIADOS - por grupo:

boxplot(dados$Memoria ~ dados$Alcool)
boxplot(dados$Latencia ~ dados$Alcool)


dados %>% group_by(Alcool) %>% 
  identify_outliers(Memoria)

dados %>% group_by(Alcool) %>% 
  identify_outliers(Latencia)



# Passo 5: Verificação da homogeneidade das matrizes de covariâncias e variâncias
## Se rompido e n iguais por grupo: Pillai e Hotelling são confiáveis
## Caso os n sejam diferentes, uma opção é usar uma MANOVA robusta
## Alfa: 0,001
box_m(dados[,c("Memoria", "Latencia")], dados$Alcool)


## Verificação da homogeneidade de variâncias - teste de Levene (pacote car)
leveneTest(Memoria ~ Alcool, dados, center = mean)
leveneTest(Latencia ~ Alcool, dados, center = mean)


# Passo 6: Verificação da presença de multicolinearidade (r > 0,9)
## Matriz de correlação
matriz <- cor(dados[,3:4])
View(matriz)


# Passo 7: Verificação da relação linear entre as variáveis dependentes por grupo
pairs(dados[,3:4], pch = 19,
      col = dados$Alcool)

graf <- dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~ggpairs(.), result = "plots")

graf$plots

graf$plots[which=1]
graf$plots[which=2]
graf$plots[which=3]


# Passo 8: Modelo de MANOVA

## Construção do modelo:
modelo <- manova(cbind(Latencia, Memoria) ~ Alcool, data=dados)


## Análise dos resultados:

options(scipen = 999)

summary(modelo, test="Wilks")

summary(modelo, test="Pillai")


## ANOVA univariada:

summary.aov(modelo)



# Passo 9: Estimated Marginal Means (Pacote emmeans)

dados %>% emmeans_test(Memoria ~ Alcool, p.adjust.method = "bonferroni")

dados %>% emmeans_test(Latencia ~ Alcool, p.adjust.method = "bonferroni")


## Outra opção: post-hocs

TukeyHSD(x = aov(Memoria ~ Alcool, data=dados), "Alcool", conf.level = 0.95)

TukeyHSD(x = aov(Latencia ~ Alcool, data = dados), 'Alcool', conf.level = 0.95)




# Passo 10: Análise descritiva

ggplot(dados, aes(x=Latencia, y=Memoria, group=Alcool, color=Alcool)) +
  geom_point()

dados %>% group_by(Alcool) %>% 
  get_summary_stats(Memoria, Latencia, type="mean_sd")
