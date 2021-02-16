#################### Carregando pacotes ####################

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(psych))
  install.packages("psych")
library(psych)


############### Carregando o banco de dados ###############

# Passo 1: selecionar o diretório de trabalho (working directory)
## Session > Set Working Directory > Choose Directory


# Passo 2: carregar o banco de dados

dados <- read.csv('Banco de Dados 2.csv', sep = ';', dec = ',')

dados <- read.csv2('Banco de Dados 2.csv')

############### Visualizando o banco de dados ###############

View(dados)
glimpse(dados)


###### Tabelas de frequências de variáveis categóricas ######

# Frequências absolutas:

table(dados$Genero)

table(dados$Grau_de_Instruçao)

## Tabela cruzada com frequências absolutas:

table(dados$Genero, dados$Grau_de_Instruçao)


# Frequências relativas:

prop.table(table(dados$Genero))

prop.table(table(dados$Grau_de_Instruçao))

prop.table(table(dados$Genero, dados$Grau_de_Instruçao))


############## Medidas para variáveis quantitativas ##############

# Tabela de frequências:

## Variáveis discretas:
table(dados$N_Filhos)
prop.table(table(dados$N_Filhos))

## Variáveis contínuas:

# Necessário criar categorias que correspondam a faixas de valores:

## Passo 1: analisar a amplitude
range(dados$Salario)

## Passo 2 (opcional): avaliar a quantidade de categorias adequada (método Sturges)
nclass.Sturges(dados$Salario)

## Passo 3: criação da tabela com as faixas
table(cut(dados$Salario, seq(0, 6, l = 7)))


# Função summary - fornece média, mediana, quartis e valores mín e máx

summary(dados$N_Filhos)

summary(dados$Salario)


# Funções describe e describe.by (pacote 'psych') - média, desvio, erro, mediana

describe(dados$Salario)

describeBy(dados$Salario, group = dados$Genero)

describeBy(dados$Salario, group = dados$Genero:dados$Grau_de_Instruçao)

# Usando o group_by do pacote dplyr

tabela <- dados %>% group_by(Genero, Grau_de_Instruçao) %>% 
  summarise(média = mean(Salario),
            DP = sd(Salario),
            mediana = median(Salario))
tabela