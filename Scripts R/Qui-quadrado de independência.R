
######################### Qui-quadrado de independ�ncia #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(psych)) install.packages("psych") 
library(psych)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

# Passo 2: Carregamento do banco de dados e montagem do modelo

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de c�digo abaixo:
# setwd("C:/Users/ferna/Desktop")

############ Primeira op��o: banco j� no formato de tabela de conting�ncia ############

# Carregamento do Banco

dadosC <- read.csv2('Banco de Dados 9.2 Contingencia.csv', row.names = 1)


View(dadosC)                                # Visualiza��o dos dados em janela separada
glimpse(dadosC)                             # Visualiza��o de um resumo dos dados


# Realiza��o do teste de Qui-quadrado

quiqua1 <- chisq.test(dadosC)
quiqua1


#######################################################################################
######################## Segunda op��o: banco no formato padr�o #######################

# Carregamento do Banco

dados <- read.csv2('Banco de Dados 9.2.csv')

View(dados)                                # Visualiza��o dos dados em janela separada
glimpse(dados)                             # Visualiza��o de um resumo dos dados


# Realiza��o do teste de Qui-quadrado

## Cria��o da tabela de conting�ncia

summary(dados$Faixa_Etaria)

dados$Faixa_Etaria <- factor(dados$Faixa_Etaria,
                             levels = c("Menos de 30 anos",
                                        "30 a 50 anos",
                                        "Mais de 50 anos"))

tabela <- table(dados$Diabetes, dados$Faixa_Etaria)
tabela

## Realiza��o do modelo

quiqua2 <- chisq.test(tabela)
quiqua2

options(scipen=999)

### Em tabelas 2x2: "correct = TRUE" ativa a corre��o de Yates



# Passo 3: An�lise das frequ�ncias esperadas
## Pressuposto: frequ�ncias esperadas > 5

quiqua2$expected



# Passo 4: An�lise dos res�duos padronizados ajustados

## Res�duo padronizado (SPSS) - res�duos de Pearson:
quiqua2$residuals


## Res�duo padronizado ajustado (SPSS):
quiqua2$stdres


## Res�duo padronizado ajustado > 1,96 ou < -1,96 -- alfa de 5%


# Passo 5: C�lculo do ponto de corte para os res�duos padronizados

## Calcular o novo alfa:
### Sendo "l" o n�mero de linhas e "c" o n�mero de colunas
### Dividiremos o 0,05 pelo produto c*l (n�mero de c�lulas)

novoalfa <- 0.05/(2*3)
novoalfa

novoalfa <- 0.05/(nrow(tabela)*ncol(tabela))


## Calcular o ponto de corte, com base no novo alfa:
### A divis�o por dois � por ser bicaudal

qnorm(novoalfa/2)

### Res�duos significativos: > 2,64 ou < -2,64 -- novo alfa: 0,008


# Passo 6 (opcional): C�lculo do p para os res�duos

round(2*(1-pnorm(abs(quiqua2$stdres))),6)

## Devem ser comparados com o novo alfa: 0,008


# Passo 7: Tamanho de efeito - V de Cramer

cramer_v(tabela)

## A interpreta��o depende dos graus de liberdade:
## gl = (linhas-1) * (colunas-1)
## Nesse caso: gl = 2 e o V de Cramer corresponde a um tamanho de efeito pequeno (Cohen, 1988)


### Para tabelas 2 x 2:
# phi(tabela)



# Passo 8: Representa��o visual dos res�duos ajustados

corrplot(quiqua2$stdres, is.cor = FALSE,
         method = "color",
         tl.col = "black", tl.srt = 0)

