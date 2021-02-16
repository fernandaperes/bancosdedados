
######################### Regressão Logística Binária #########################

# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 13.csv', stringsAsFactors = TRUE)

View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados



# Passo 3: Análise das frequências das categorias da VD

table(dados$Cancer)

summary(dados)


# Passo 4: Checagem das categorias de referência

levels(dados$Cancer)  # Não = categoria de referência

levels(dados$Hab_Fumar)  # Não = categoria de referência


# Passo 5: Checagem dos pressupostos

## 1. Variável dependente dicotômica (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)


## Construção do modelo:

mod <- glm(Cancer ~ Estresse + Hab_Fumar,
           family = binomial(link = 'logit'), data = dados)


## 3. Ausência de outliers/ pontos de alavancagem

plot(mod, which = 5)

summary(stdres(mod))


## 4. Ausência de multicolinearidade

pairs.panels(dados)
### Multicolinearidade: r > 0.9 (ou 0.8)


vif(mod)
### Multicolinearidade: VIF > 10


## 5. Relação linear entre cada VI contínua e o logito da VD


### Interação entre a VI contínua e o seu log não significativa (Box-Tidwell)

intlog <- dados$Estresse * log(dados$Estresse)

dados$intlog <- intlog

modint <- glm(Cancer ~ Hab_Fumar + Estresse + intlog,
              family = binomial(link = 'logit'), data = dados)

summary(modint)


### Outra opção:

#### Cálculo do logito

logito <- mod$linear.predictors

### Outra opção para o cálculo do logito:
# prob <- predict(mod, type = "response")
# logito <- log(prob/(1-prob))

dados$logito <- logito


#### Análise da relação linear

ggplot(dados, aes(logito, Estresse)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_classic()


# Passo 6: Análise do modelo

## Overall effects

Anova(mod, type = 'II', test = "Wald")


## Efeitos específicos

summary(mod)


## Obtenção das razões de chance com IC 95% (usando log-likelihood)

exp(cbind(OR = coef(mod), confint(mod)))


## Obtenção das razões de chance com IC 95% (usando erro padrão = SPSS)

exp(cbind(OR = coef(mod), confint.default(mod)))



# Passo 7: Criação e análise de um segundo modelo

mod2 <- glm(Cancer ~ Hab_Fumar,
            family = binomial(link = 'logit'), data = dados)


## Overall effects

Anova(mod2, type="II", test="Wald")


## Efeitos específicos

summary(mod2)

## Obtenção das razões de chance com IC 95% (usando log-likelihood)

exp(cbind(OR = coef(mod2), confint(mod2)))


## Obtenção das razões de chance com IC 95% (usando erro padrão = SPSS)

exp(cbind(OR = coef(mod2), confint.default(mod2)))



# Passo 8: Avaliação da qualidade e comparação entre modelos

## Pseudo R-quadrado

PseudoR2(mod, which = "Nagelkerke")

PseudoR2(mod2, which = "Nagelkerke")


# Comparação de modelos
## AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)


## Qui-quadrado
anova(mod2, mod, test="Chisq")


# Tabela de classificação
ClassLog(mod, dados$Cancer)
ClassLog(mod2, dados$Cancer)



####### Como modificar as categorias de referência? ########

levels(dados$Hab_Fumar)

dados$Hab_Fumar <- relevel(dados$Hab_Fumar, ref = "Sim")


### ATENÇÃO: é necessário rodar o modelo novamente!


levels(dados$Cancer)

dados$Cancer <- relevel(dados$Cancer, ref = "Sim")

