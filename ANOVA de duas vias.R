
######################### ANOVA de duas vias #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 6.csv') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados


summary(dados$Alcool)

dados$Alcool <- factor(dados$Alcool,
                       levels = c("Nenhum",
                                  "2 Canecas",
                                  "4 Canecas"))


# Passo 3: Verificação dos pressupostos nos dados brutos

## Verificação da normalidade - Shapiro por grupo:
dados %>% group_by(Genero, Alcool) %>% 
  shapiro_test(Memoria)


## Verificação da presença de outliers por grupo:
boxplot(dados$Memoria ~ dados$Genero:dados$Alcool)

dados %>% group_by(Genero, Alcool) %>% 
  identify_outliers(Memoria)


## Verificação da homogeneidade de variâncias - teste de Levene (pacote car)
leveneTest(Memoria ~ Genero*Alcool, dados, center = mean)



# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Mudado para ser baseado na média (comparável ao SPSS)

# Passo 4: Verificação dos pressupostos nos resíduos

## Construção do modelo:
modelo <- aov(Memoria ~ Genero*Alcool, dados)


## Teste de normalidade para os resíduos:
shapiro.test(modelo$residuals)


## Verificação da presença de outliers entre os resíduos:
boxplot(modelo$residuals)

dados$Residuos <- modelo$residuals

dados %>% group_by(Genero, Alcool) %>% 
  identify_outliers(Residuos)

dados %>% identify_outliers(Residuos)


## Verificação da homogeneidade de variâncias - teste de Levene (pacote car)
leveneTest(Residuos ~ Genero*Alcool, dados, center = mean)


# Passo 5: Realização da ANOVA

## Mudança no contraste para equivaler ao SPSS:
options(contrasts = c("contr.sum", "contr.poly"))

## Criação do modelo:
modelo <- aov(Memoria ~ Genero*Alcool, dados)
# summary(modelo)
Anova(modelo, type = 'III')


# Passo 6: Estimated Marginal Means (Pacote emmeans)

dados %>% group_by(Genero) %>% 
  emmeans_test(Memoria ~ Alcool, p.adjust.method = "bonferroni")
# Outra opção: correção de Sidak

dados %>% group_by(Alcool) %>% 
  emmeans_test(Memoria ~ Genero, p.adjust.method = "bonferroni")



# Passo 7: Análise do post-hoc (Pacote DescTools)
# Post-hocs permitidos: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"


# Uso do Duncan
PostHocTest(modelo, method = "duncan")

# Uso do TukeyHSD
PostHocTest(modelo, method = "hsd")

# Uso do Bonferroni
PostHocTest(modelo, method = "bonferroni")


# Passo 8: Gráfico de interação (Pacote ggplot2)

## Com gêneros com cores diferentes
ggplot(dados, aes(x = Alcool, y = Memoria, group = Genero, color = Genero)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)


## Com gêneros com linhas diferentes
ggplot(dados, aes(x = Alcool, y = Memoria, group = Genero)) +
  geom_line(stat = "summary", fun.data="mean_se", size = 0.6, aes(linetype = Genero)) +
  geom_point(stat = "summary", fun.y = "mean", size = 2, aes(shape = Genero)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)



# Passo 9: Análise descritiva dos dados - Pacote rstatix
dados %>%
  group_by(Genero, Alcool) %>%
  get_summary_stats(Memoria, type = "mean_sd")
