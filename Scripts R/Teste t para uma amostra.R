
######################### Teste t para uma Amostra #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 2.csv', sep = ';', dec = ',') # Carregamento do arquivo csv
View(dados)                                       # Visualização dos dados em janela separada
glimpse(dados)                                          # Visualização de um resumo dos dados


# Passo 3: Verificação da normalidade dos dados

shapiro.test(dados$Altura)



# Passo 4: Realização do teste t para uma amostra

t.test(dados$Altura, mu = 167)


# Observação:
  # O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
    # alternative = "greater" ou alternative = "less"
  # Exemplo: t.test(dados$Altura, mu = 167, alternative = "greater")
    # Nesse caso, o teste verificará se é a média amostral é maior que a média testada


# Passo 5 (opcional): Visualização da distribuição dos dados

boxplot(dados$Altura, ylab = "Altura (cm)")


