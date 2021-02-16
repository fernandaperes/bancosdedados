
######################### Correlação Linear Bivariada #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 10.csv') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados



# Passo 3: Verificação dos pressupostos para a correlação de Pearson

## Normalidade (Shapiro-Wilk):
shapiro.test(dados$Ansiedade)
shapiro.test(dados$Nota)


## Presença de outliers:
boxplot(dados$Ansiedade)
boxplot(dados$Nota)


## Relação linear entre as variáveis:
plot(dados$Ansiedade, dados$Nota)



# Passo 4: Verificação dos pressupostos nos resíduos

## Construção do modelo:
mod_reg <- lm(Nota ~ Ansiedade, dados)


## Análise gráfica:
par(mfrow=c(1,2))
plot(mod_reg, which=c(1,3))

par(mfrow=c(1,1))

### Gráfico 1: valores previstos x resíduos
#### Permite verificar se há homogeneidade de variâncias (homocedasticidade) - resíduos
# distribuídos de acordo com um padrão aproximadamente retangular;
#### Permite verificar se a relação entre as variáveis é linear - linha vermelha
# aproximadamente horizontal;

### Gráfico 3: valores previstos x resíduos padronizados
#### Permite verificar se há outliers - valores de resíduos padronizados acima de 3 ou
# abaixo de -3;



# Passo 5: Realização da correlação

## Correlação Linear de Pearson (coeficiente = r):
cor.test(dados$Nota, dados$Ansiedade, method = "pearson")


## Correlação de Postos de Spearman (coeficiente = rô):
cor.test(dados$Nota, dados$Ansiedade, method = "spearman")


## Correlação Tau de Kendall (coeficiente = tau):
cor.test(dados$Nota, dados$Ansiedade, method = "kendall")


# Passo 6: Gráfico de dispersão
ggplot(dados, aes(x=Ansiedade, y=Nota)) +
  labs(x = "Ansiedade pré-prova", y = "Desempenho na prova") +
  geom_point(size = 1.8) +
  theme_classic()



# Passo 7 (opcional): Matrizes de correlação

## Criando a matriz:
matriz <- cor(dados[2:4], method = "pearson")
View(matriz)

## Arredondando para duas casas decimais:
matriz <- round(cor(dados[2:4], method = "pearson"), 2)
View(matriz)


## Criando uma matriz visual (pacote corrplot)

corrplot(matriz, method = "number")


### Opções de métodos: method = circle, color, pie
### Opções de tipos: type = upper, lower
### Ordenar: order = hclust

corrplot(matriz, method="color", 
         type="upper", order="hclust", 
         addCoef.col = "black", # adiciona o coeficiente à matriz
         tl.col="black", tl.srt=45, # cor e rotação do nome das variáveis
         diag=FALSE # não mostrar a diagonal principal
         )

#### http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram



