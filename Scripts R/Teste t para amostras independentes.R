
######################### Teste t para Amostras Independentes #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") # Instalação do pacote caso não esteja instalado
library(RVAideMemoire)                                        # Carregamento do pacote
if(!require(car)) install.packages("car") # Instalação do pacote caso não esteja instalado
library(car)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 3.csv', sep = ';', dec = ',') # Carregamento do arquivo csv
View(dados)                                       # Visualização dos dados em janela separada
glimpse(dados)                                          # Visualização de um resumo dos dados


# Passo 3: Verificação da normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire)

byf.shapiro(Nota_Biol ~ Posicao_Sala, dados)
byf.shapiro(Nota_Fis ~ Posicao_Sala, dados)
byf.shapiro(Nota_Hist ~ Posicao_Sala, dados)


# Passo 4: Verificação da homogeneidade de variâncias
## Teste de Levene (pacote car)

leveneTest(Nota_Biol ~ Posicao_Sala, dados, center=mean)
leveneTest(Nota_Fis ~ Posicao_Sala, dados, center=mean)
leveneTest(Nota_Hist ~ Posicao_Sala, dados, center=mean)

# Observação:
  # Por default, o teste realizado pelo pacote car tem como base a mediana (median)
    # O teste baseado na mediana é mais robusto
  # Mudamos para ser baseado na média (comparável ao SPSS)


# Passo 5: Realização do teste t para amostras independentes

t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE)
t.test(Nota_Fis ~ Posicao_Sala, dados, var.equal=FALSE)
t.test(Nota_Hist ~ Posicao_Sala, dados, var.equal=FALSE)

# Observação:
  # O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
    # alternative = "greater" ou alternative = "less"
  # Exemplo: t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE, alternative="greater")
    # Nesse caso, o teste verificará se é a média do primeiro grupo é maior que a média do segundo
      # O R está considerando "Frente" como primeiro grupo


# Passo 6 (opcional): Visualização da distribuição dos dados

par(mfrow=c(1,3)) # Estabeleci que quero que os gráficos saiam na mesma linha
boxplot(Nota_Biol ~ Posicao_Sala, data = dados, ylab="Notas de Biologia", xlab="Posição na Sala")
boxplot(Nota_Fis ~ Posicao_Sala, data = dados, ylab="Notas de Física", xlab="Posição na Sala")
boxplot(Nota_Hist ~ Posicao_Sala, data = dados, ylab="Notas de História", xlab="Posição na Sala")

