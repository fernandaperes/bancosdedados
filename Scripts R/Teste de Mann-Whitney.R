
######################### Teste de Mann-Whitney #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(dplyr)) install.packages("rstatix") # Instalação do pacote caso não esteja instalado
library(rstatix)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 3.csv', sep = ';', dec = ',') # Carregamento do arquivo csv
View(dados)                                       # Visualização dos dados em janela separada
glimpse(dados)                                          # Visualização de um resumo dos dados


# Passo 3: Realização do teste de Mann-Whitney

wilcox.test(Nota_Biol ~ Posicao_Sala, data = dados)
wilcox.test(Nota_Fis ~ Posicao_Sala, data = dados)
wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados)

# Observação:
  # O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
    # alternative = "greater" ou alternative = "less"
  # Exemplo: wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados, alternative="greater")
    # Nesse caso, o teste verificará se é a mediana do primeiro grupo é maior que a mediana do segundo
      # O R está considerando "Frente" como primeiro grupo


# Passo 4: Análise descritiva dos dados

dados %>% group_by(Posicao_Sala) %>% 
  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "median_iqr")

# Dados paramétricos?
# dados %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")


# Passo 5: Visualização da distribuição
par(mfrow=c(1,2))
hist(dados$Nota_Biol[dados$Posicao_Sala == "Frente"],
     ylab="Frequência", xlab="Nota", main="Grupo Frente")
hist(dados$Nota_Biol[dados$Posicao_Sala == "Fundos"],
     ylab="Frequência", xlab="Nota", main="Grupo Fundos")
