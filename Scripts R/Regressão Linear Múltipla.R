
######################### Regressão Linear Múltipla #########################

# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 12.csv') # Carregamento do arquivo csv
View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados




## Construção do modelo:
mod <- lm(Notas ~ Tempo_Rev + Tempo_Sono, dados)


## Análise gráfica:
par(mfrow=c(2,2))

plot(mod)

par(mfrow=c(1,1))


## Normalidade dos resíduos:
shapiro.test(mod$residuals)


## Outliers nos resíduos:
summary(rstandard(mod))


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


## Ausência de Multicolinearidade

pairs.panels(dados)
### Multicolinearidade: r > 0.9 (ou 0.8)

vif(mod)
### Multicolinearidade: VIF > 10


###### Criação de um segundo modelo
mod2 <- lm(Notas ~ Tempo_Rev, dados)


# Passo 4: Análise do modelo
summary(mod)
summary(mod2)


## Obtenção dos coeficientes padronizados
lm.beta(mod)
lm.beta(mod2)


## Obtenção do IC 95% para os coeficientes
confint(mod)
confint(mod2)


# Comparação de modelos

## AIC e BIC - Comparação entre quaisquer modelos
AIC(mod, mod2)
BIC(mod, mod2)


## Para comparação entre modelos aninhados

anova(mod, mod2)

### O melhor será o com menor valor de RSS (residual sum of squares)



# Passo 5: Gráfico de dispersão
graph <- scatterplot3d(dados$Notas ~ dados$Tempo_Rev + dados$Tempo_Sono,
                       pch = 16, angle = 30, color = "steelblue", box = FALSE,
                       xlab="Tempo de revisão", ylab="Tempo de sono", zlab="Notas")
graph$plane3d(mod, col="black", draw_polygon = TRUE)



##################### Métodos de seleção de modelos ###########################

pacman::p_load(MASS)


mod.inicial <- lm(Notas ~ Tempo_Rev + Tempo_Sono, data = dados)
mod.simples <- lm(Notas ~ 1, data = dados)

stepAIC(mod.inicial, scope = list(upper = mod.inicial,
                                  lower = mod.simples), direction = "backward")


# Material consultado: https://rpubs.com/bensonsyd/385183
