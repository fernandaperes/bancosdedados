
######################### Teste de Kruskal-Wallis #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(rstatix)) install.packages("rstatix") # Instalação do pacote caso não esteja instalado
library(rstatix)                                  # Carregamento do pacote
if(!require(ggplot2)) install.packages("ggplot2") # Instalação do pacote caso não esteja instalado
library(ggplot2)                                  # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 5.csv', sep = ';', dec = ',') # Carregamento do arquivo csv
View(dados)                                       # Visualização dos dados em janela separada
glimpse(dados)                                          # Visualização de um resumo dos dados


# Passo 3: Realização do teste de Kruskal-Wallis

kruskal.test(BC ~ Grupo, data = dados)
kruskal.test(Pressao ~ Grupo, data = dados)


# Passo 4: Testes de post-hoc

# Teste de Dunn com ajuste do valor de p
dunn_test(BC ~ Grupo, data = dados, p.adjust.method = "bonferroni")
dunn_test(Pressao ~ Grupo, data = dados, p.adjust.method = "bonferroni")


# Passo 5: Análise descritiva dos dados
dados %>% group_by(Grupo) %>% 
  get_summary_stats(BC, Pressao, type = "median_iqr")


# Passo 6: Visualização dos dados
par(mfrow=c(1,2))
boxplot(BC ~ Grupo, data = dados)
boxplot(Pressao ~ Grupo, data = dados)


# Passo 7: Análise da distribuição

par(mfrow=c(1,3))
hist(dados$BC[dados$Grupo == "Placebo"],
     ylab = "Frequência", xlab = "bps", main="Placebo")
hist(dados$BC[dados$Grupo == "AH Novo"],
     ylab = "Frequência", xlab = "bps", main="AH Novo")
hist(dados$BC[dados$Grupo == "AH Padrão"],
     ylab = "Frequência", xlab = "bps", main="AH Padrão")



par(mfrow=c(1,3))
hist(dados$Pressao[dados$Grupo == "Placebo"],
     ylab="Frequência", xlab="bps", main="Placebo")
hist(dados$Pressao[dados$Grupo == "AH Novo"],
     ylab="Frequência", xlab="bps", main="AH Novo")
hist(dados$Pressao[dados$Grupo == "AH Padrão"],
     ylab="Frequência", xlab="bps", main="AH Padrão")


# Histograma com todos os grupos, separados por cor
ggplot(dados, aes(x = BC)) +
  geom_histogram(aes(color = Grupo, fill = Grupo),
                 alpha = 0.3, position = "identity", binwidth = 10)

ggplot(dados, aes(x = Pressao)) +
  geom_histogram(aes(color = Grupo, fill = Grupo),
                 alpha = 0.3, position = "dodge", binwidth = 10)
