# Instalando pacotes

## Opção 1: através de "packages" no canto inferior direito

## Opção 2:
install.packages()

## Opção 3:
if(!require(dplyr))
  install.packages("dplyr")


#################### Carregando pacotes ####################

library(car)
require(dplyr)



############### Carregando o banco de dados ###############

# Passo 1: selecionar o diretório de trabalho (working directory)


## Opção 1 - Manualmente: Session > Set Working Directory > Choose Directory

## Opção 2:
setwd("C:/Users/ferna/Desktop")


# Passo 2: carregar o banco de dados

dados <- read.csv('Banco de Dados 2.csv', sep = ';', dec = ',')

## Funções para visualizar o banco de dados:
View(dados)
glimpse(dados)
