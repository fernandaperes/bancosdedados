
##################### ANOVA mista ####################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(ez)) install.packages("ez") 
library(ez)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(reshape)) install.packages("reshape") 
library(reshape)
if(!require(car)) install.packages("car") 
library(car)
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 7.4.csv') # Carregamento do arquivo csv
View(dados)                                  # Visualização dos dados em janela separada
glimpse(dados)                               # Visualização de um resumo dos dados

summary(dados)


# Passo 3: Alterar o formato do banco de dados de "wide" para "long"

# Reestruturando o banco de dados

dadosl <- reshape(dados, direction="long",
                  idvar = "ID",
                  varying = list(c("TG.1","TG.2","TG.3","TG.4","TG.5"),
                                 c("Peso.1","Peso.2","Peso.3","Peso.4","Peso.5")),
                  v.names = c("TG", "Peso"),
                  timevar = "Tempo")

# Ordenando as colunas pelo sujeito experimental
dadosl <- sort_df(dadosl, vars = "ID")

glimpse(dadosl)

# Transformando as variáveis ID e Tempo em fator
dadosl$ID <- factor(dadosl$ID)
dadosl$Tempo <- factor(dadosl$Tempo)


# Passo 4: Checar os pressupostos de normalidade e ausência de outliers (pacote: rstatix)

# Verificando a presença de outliers por grupo
boxplot(TG ~ Gênero:Tempo, dadosl)


# Verificando a normalidade por grupo
dadosl %>% group_by(Gênero, Tempo) %>% 
  shapiro_test(TG)


# Verificando a homogeneidade de variâncias
leveneTest(TG ~ Gênero, dadosl)


# Passo 5: Construção do modelo da ANOVA com medidas repetidas (pacote: ez)

mod.ANOVA <- ezANOVA(data = dadosl,
                     dv = .(TG),
                     wid = .(ID),
                     within = .(Tempo),
                     between = .(Gênero),
                     detailed = TRUE,
                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# between = variável independente entre sujeitos
# between_covariates = covariável
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo

# options(scipen = 999)  

mod.ANOVA

# Obs.: Libera o teste de esfericidade de Mauchly, e as correções
#       de Greenhouse-Geisser e Huynh-Feldt


# Passo 6: Visualização das diferenças
ggplot(dadosl, aes(x = Tempo, y = TG, group = Gênero, color = Gênero)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.8) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)


# Passo 7: Testes de post-hoc

CompTempo <- dadosl %>% group_by(Gênero) %>%
  emmeans_test(TG ~ Tempo, p.adjust.method = "bonf")
View(CompTempo)
# Outra opção: correção de Sidak

CompGen <- dadosl %>% group_by(Tempo) %>%
  emmeans_test(TG ~ Gênero, p.adjust.method = "bonferroni")
View(CompGen)


# Passo 8 (opcional): Análise descritiva dos dados (pacote: rstatix)
dadosl %>% group_by(Tempo, Gênero) %>% 
  get_summary_stats(TG, type = "mean_sd")

