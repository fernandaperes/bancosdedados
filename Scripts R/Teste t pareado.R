
######################### Teste t Pareado #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(psych)) install.packages("psych") # Instalação do pacote caso não esteja instalado
library(psych)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 4.csv', sep = ';', dec = ',') %>%
  rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
                     Convulsoes_S6 = Convulsões_S6, Genero = Gênero)
View(dados)                                              # Visualização dos dados em janela separada
glimpse(dados)                                                 # Visualização de um resumo dos dados

# Passo 3: Verificação da normalidade dos dados

dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1

shapiro.test(dados$DiferencaPTS1)


# Passo 4: Realização do teste t pareado

t.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)


# Passo 5 (opcional): Visualização da distribuição dos dados

par(mfrow=c(1,2)) # Estabeleci que quero que os gráficos saiam na mesma linha
boxplot(dados$Convulsoes_PT, ylab="Quantidade de Convulsões", xlab="Pré-Tratamento")
boxplot(dados$Convulsoes_S1, ylab="Quantidade de Convulsões", xlab="1ª semana de Tratamento")


# Passo 6 (opcional): Análise descritiva dos dados
summary(dados$Convulsoes_PT)
summary(dados$Convulsoes_S1)

## Outra forma: pela função describe do pacote Psych
describe(dados$Convulsoes_PT)
describe(dados$Convulsoes_S1)
