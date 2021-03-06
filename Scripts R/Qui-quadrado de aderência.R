
######################### Qui-quadrado de ader�ncia #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)


# Passo 2: Carregamento do banco de dados e montagem do modelo

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:
# setwd("C:/Users/ferna/Desktop")


# Carregamento do Banco

dados <- read.csv2('Banco de Dados 9.1.csv')

View(dados)                                # Visualiza��o dos dados em janela separada
glimpse(dados)                             # Visualiza��o de um resumo dos dados

# Passo 3: Realiza��o do teste de Qui-quadrado

## Cria��o da tabela de frequ�ncia

tabela <- table(dados$Tipo_Ervilha)
tabela


## Realiza��o do modelo

quiqua <- chisq.test(tabela, p = c(0.5625, 0.1875, 0.1875, 0.0625))
quiqua


# Passo 4: An�lise dos res�duos padronizados ajustados

## Res�duo padronizado (SPSS) - res�duos de Pearson:
quiqua$residuals


## Res�duo padronizado ajustado (SPSS):
quiqua$stdres


## Res�duo padronizado ajustado > 1,96 ou < -1,96 -- alfa de 5%


# Passo 5: C�lculo do ponto de corte para os res�duos padronizados

## Calcular o novo alfa:
### Dividiremos o 0,05 pela quantidade de colunas

novoalfa <- 0.05/4
novoalfa

novoalfa <- 0.05/length(tabela)
novoalfa


## Calcular o ponto de corte, com base no novo alfa:
### A divis�o por dois � por ser bicaudal

qnorm(novoalfa/2)

### Novos limites para os res�duos padronizados ajustados: > 2,50 e < -2,50


# Passo 6 (opcional): C�lculo do p para os res�duos

round(2*(1-pnorm(abs(quiqua$stdres))),6)

## Devem ser comparados com o novo alfa -- 0,0125



# Passo 7: Tamanho de efeito - V de Cramer

cramer_v(tabela, p = c(0.5625, 0.1875, 0.1875, 0.0625))


## A interpreta��o depende dos graus de liberdade:
## gl = colunas-1
## Nesse caso: gl = 3 e o V de Cramer corresponde a um
### tamanho de efeito insignificante (Cohen, 1988)

