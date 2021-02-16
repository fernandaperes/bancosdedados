
######################### ANOVA de uma via #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)                                        
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(psych)) install.packages("psych") 
library(psych)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 5.csv') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados


# Passo 3: Verificação da normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire)

byf.shapiro(BC ~ Grupo, dados)
byf.shapiro(Pressao ~ Grupo, dados)


# Passo 4: Verificação da homogeneidade de variâncias
## Teste de Levene (pacote car)

leveneTest(BC ~ Grupo, dados, center=mean)
leveneTest(Pressao ~ Grupo, dados, center=mean)

# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Mudamos para ser baseado na média (comparável ao SPSS)


# Passo 5: Verificação da presença de outliers (por grupo) - Pacotes dplyr e rstatix

# Para BC:
dados %>% 
  group_by(Grupo) %>% 
  identify_outliers(BC)

## Pelo boxplot:
boxplot(BC ~ Grupo, data = dados, ylab="Frequência cardíaca (bps)", xlab="Tratamento")

# Para Pressão:
dados %>% 
  group_by(Grupo) %>%
  identify_outliers(Pressao)

## Pelo boxplot:
boxplot(Pressao ~ Grupo, data = dados, ylab="Pressão arterial (mmHg)", xlab="Tratamento")


# Passo 6: Realização da ANOVA

## Criação do modelo para BC
anova_BC <- aov(BC ~ Grupo, dados)
summary(anova_BC)

## Criação do modelo para Pressão
anova_Pressao <- aov(Pressao ~ Grupo, dados)
summary(anova_Pressao)


# Passo 7: Análise post-hoc - Pacote DescTools
# Post-hocs permitidos: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

# Uso do Duncan
PostHocTest(anova_BC, method = "duncan")

# Uso do TukeyHSD
PostHocTest(anova_BC, method = "hsd")

# Uso do Bonferroni
PostHocTest(anova_BC, method = "bonf")

# É o mesmo que:
pairwise.t.test(dados$BC, dados$Grupo, p.adj="bonferroni", paired=F)


## Exemplo de como resumir em uma tabela mais de um post-hoc
round(
  cbind(duncan = PostHocTest(anova_BC, method="duncan")$Grupo[,"pval"],
        bonf = PostHocTest(anova_BC, method="bonf")$Grupo[,"pval"],
        hsd = PostHocTest(anova_BC, method="hsd")$Grupo[,"pval"]),
  6)

# Uso do TukeyHSD para a Pressão:
PostHocTest(anova_Pressao, method = "hsd")


# Passo 8 (opcional): Análise descritiva dos dados
describeBy(dados$BC, group = dados$Grupo)
describeBy(dados$Pressao, group = dados$Grupo)
