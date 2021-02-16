
######################### Teste de Friedman #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix) 
if(!require(reshape)) install.packages("reshape") 
library(reshape) 
if(!require(PMCMRplus)) install.packages("PMCMRplus") 
library(PMCMRplus)   
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)    

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv2('Banco de Dados 7.csv') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados


# Passo 3: Alterar o formato do banco de dados de "wide" para "long" (pacote: reshape)

# Reestruturando o banco de dados

dadosl <- melt(dados,
               id = "ID",
               measured = c("Professor1", "Professor2", "Professor3", "Professor4"))

View(dadosl)

# Renomeando as colunas do novo banco
colnames(dadosl) = c("ID", "Professor", "Nota")


# Ordenando as colunas pelo sujeito experimental
dadosl <- sort_df(dadosl, vars = "ID")


glimpse(dadosl)


# Transformando a variável ID em fator
dadosl$ID <- factor(dadosl$ID)



# Passo 3: Realização do teste de Friedman

friedman.test(Nota ~ Professor | ID, data = dadosl)

## Montagem do modelo: VD ~ VI (intra-sujeitos) | ID

## Outra opção:
### friedman.test(dadosl$Nota, dadosl$Professor, dadosl$ID)


# Passo 4: Testes de post-hoc

## Opção 1: Wilcoxon com correção de Bonferroni
dadosl %>% wilcox_test(Nota ~ Professor, paired = TRUE, p.adjust.method = "bonferroni")


## Opção 2 - post-hocs do pacote PMCMRplus: 
### Dunn-Bonferroni - equivalente ao SPSS:

frdAllPairsSiegelTest(dadosl$Nota, dadosl$Professor,
                      dadosl$ID, p.adjust.method = "bonferroni")


### Outros:

frdAllPairsNemenyiTest(dadosl$Nota, dadosl$Professor,
                       dadosl$ID, p.adjust.method = "bonferroni")


frdAllPairsConoverTest(dadosl$Nota, dadosl$Professor,
                       dadosl$ID, p.adjust.method = "bonferroni")



# Passo 5: Análise descritiva dos dados
dadosl %>% group_by(Professor) %>% 
  get_summary_stats(Nota, type = "median_iqr")


# Passo 6: Visualização dos dados
boxplot(Nota ~ Professor, data = dadosl)


# Passo 7: Análise da distribuição

par(mfrow=c(2,2))
hist(dadosl$Nota[dadosl$Professor == "Professor1"],
     ylab = "Frequência", xlab = "Notas", main="Professor 1")
hist(dadosl$Nota[dadosl$Professor == "Professor2"],
     ylab = "Frequência", xlab = "Notas", main="Professor 2")
hist(dadosl$Nota[dadosl$Professor == "Professor3"],
     ylab = "Frequência", xlab = "Notas", main="Professor 3")
hist(dadosl$Nota[dadosl$Professor == "Professor4"],
     ylab = "Frequência", xlab = "Notas", main="Professor 4")

# Histograma com todos os grupos, separados por cor
ggplot(dadosl, aes(x = Nota)) +
  geom_histogram(aes(color = Professor, fill = Professor),
                 alpha = 0.3, position = "stack", binwidth = 1)

