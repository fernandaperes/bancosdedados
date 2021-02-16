#################### Carregando pacotes ####################

if (!require(dplyr))
  install.packages("dplyr")

library(dplyr)


############### Carregando o banco de dados ###############

# Passo 1: selecionar o diretório de trabalho (working directory)


## Opção 1 - Manualmente: Session > Set Working Directory > Choose Directory

## Opção 2:
setwd("C:/Users/ferna/Desktop")


# Passo 2: carregar o banco de dados

dados <- read.csv('Banco de Dados 2 Codificado.csv', sep = ';', dec = ',')


############### Visualizando o banco de dados ###############

View(dados)
glimpse(dados)


############### Ajustando as variáveis ###############

# Transformando Gênero em fator:

dados$Genero <- factor(dados$Genero, label = c("M", "F"), levels = c(0, 1))


# Transformando Grau de Instrução em fator:

dados$Grau_de_Instruçao <- factor(dados$Grau_de_Instruçao,
                                  label = c("Fundamental", "Medio", "Superior"),
                                  levels = 0:2, order = T)


# Codificando valores ausentes (missing values):

dados[dados==-999] <- NA



