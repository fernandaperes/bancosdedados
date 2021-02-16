
######################### ANCOVA #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                  
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(car)) install.packages("car")   
library(car)             
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(multcomp)) install.packages("multcomp") 
library(multcomp)
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 8.csv') # Carregamento do arquivo csv
View(dados) 
glimpse(dados)                             # Visualização de um resumo dos dados


# VD: Salário
# VI: Grau de instrução
# Covariável: Idade


# Passo 3: Verificar se há efeito da VI sobre a covariável (cov ~ VI)
# Pressuposto: "independência entre a VI e a covariável"
# Se rompido: não há outro modelo - é um problema de delineamento
mod_cov <- aov(Idade ~ Grau_Instrução, data = dados)
summary(mod_cov)


# Passo 4: Verificar se a relação entre a covariável e a VD é linear (VD ~ cov)
ggplot(data = dados, aes(x = Idade, y = Salário, group = Grau_Instrução,
                         color = Grau_Instrução)) +
  geom_point(size = 2) +
  xlab('Idade') +
  ylab('Salário') +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)


# Passo 5: Verificar se o efeito da covariável é o mesmo para todos níveis da VI (VD ~ VI*cov)
# Pressuposto: "homogeneidade dos parâmetros de regressão"
# Compara as inclinações das retas para cada grupo da VI
mod_int <- aov(Salário ~ Grau_Instrução*Idade, data = dados)
Anova(mod_int, type = 'III')



# Passo 6: Verificar se há homogeneidade de variâncias (VD ~ VI)
# Se rompido: versão robusta da ANCOVA
leveneTest(Salário ~ Grau_Instrução, center = mean, data = dados)



# Passo 7: Ajustar o modelo de ANCOVA (VD ~ cov + VI)
## Se os resultados forem avaliados pelo tipo I da soma dos quadrados
## é obrigatório que a covariável seja inserida antes no modelo
options(contrasts = c("contr.sum", "contr.poly"))

mod_ANCOVA <- aov(Salário ~ Idade + Grau_Instrução, data = dados)
Anova(mod_ANCOVA, type = 'III')



# Passo 8: Verificar a normalidade dos resíduos
# Se rompido: versão robusta da ANCOVA
shapiro.test(mod_ANCOVA$residuals)


# Passo 9: Verificar se há homocedasticidade e outliers
boxplot(mod_ANCOVA$residuals)

par(mfrow=c(1,2))
plot(mod_ANCOVA, which=c(1,3))


leveneTest(mod_ANCOVA$residuals ~ dados$Grau_Instrução)


# Passo 10: Realização das comparações entre grupos

## Pelo pacote multcomp
posthoc <- glht(mod_ANCOVA, linfct = mcp(Grau_Instrução = "Tukey"))
summary(posthoc)
## Outra opção: Dunnett


## Pelo rstatix
comparacoes <- dados %>% emmeans_test(Salário ~ Grau_Instrução, covariate = Idade,
                       p.adjust.method = "bonferroni")



# Passo 11: Obtenção das médias ajustadas
## Opção 1:
medias_ajustadas <- emmeans(mod_ANCOVA, ~ Idade:Grau_Instrução)
medias_ajustadas


## Opção 2:
get_emmeans(comparacoes)


####### Médias reais
dados %>% group_by(Grau_Instrução) %>% 
  get_summary_stats(Salário, type = "mean_sd")


