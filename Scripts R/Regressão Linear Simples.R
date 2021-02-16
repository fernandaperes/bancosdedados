
######################### Regressão Linear Simples #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 11.csv') # Carregamento do arquivo csv
View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados



# Passo 3: Verificação dos pressupostos para a regressão linear


## Relação linear entre a VD e a VI:
### VD: Vendas
### VI: Publicidade

plot(dados$Publicidade, dados$Vendas)


## Construção do modelo:
mod <- lm(Vendas ~ Publicidade, dados)


## Análise gráfica:

par(mfrow=c(2,2))

plot(mod)

### Interpretação: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos resíduos:
shapiro.test(mod$residuals)


## Outliers nos resíduos:
summary(rstandard(mod))


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


# Passo 4: Análise do modelo
summary(mod)





ggplot(data = dados, mapping = aes(x = Publicidade, y = Vendas)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 400) +
  theme_classic()


# https://pt.stackoverflow.com/questions/6979/como-colocar-a-equa%C3%A7%C3%A3o-da-regress%C3%A3o-em-um-gr%C3%A1fico