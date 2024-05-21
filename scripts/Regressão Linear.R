### Regressão Linear ###
## Metodologia
# Cria o modelo
# Análise de significância
# Teste Anova à Regressão
# Análise de Resíduos: Independência, Normalidade, Homocedasticidade
# Coeficiente de Determinação: R^2

library(lmtest)

# Exemplo Livro c14/10 (p.351, livro de texto)
# Inserir os dados
dados = data.frame(
  x = c(12,8,14,10,6),
  y = c(33,24,39,31,23)
)

summary(dados)

# teste de correlação linear

cor.test(dados$x,dados$y, 'spearman', alternative='two.sided')

plot(dados$x,dados$y)

# Criar o modelo

modelo = lm(y~x, data=dados)

summary(modelo)

anova(modelo) # inferência aos parâmetros

# Análise dos resíduos: IN(0, sigma)

residuals(modelo)
# Variância dos resíduos

# Normalidade
# Por exemplo, através do QQ-plot

qqnorm(residuals(modelo), pch=20, main='QQ-plot: resíduos')
qqline(residuals(modelo), col='red', lwd=2)

# por exemplo, através do teste de shapiro-wilk

shapiro = shapiro.test(residuals(modelo)) # n<30

show(shapiro)

plot(modelo$residuals)

# Exercício 4.2 #

dados4.2 = data.frame(
  x = c(12.43, 13.52, 14.42, 15.24, 16.06, 16.88, 17.70, 17.84, 18.76),
  y = c(0.049, 0.064, 0.059, 0.051, 0.073, 0.095, 0.245, 0.222, 0.350)
)

cor.test(dados4.2$x, dados4.2$y, 'spearman', alternative = 'two.sided')

# p-valor < 0.05, as variáveis estão correlacionadas

plot(dados4.2$x, dados4.2$y)

# Cria o modelo
modelo4.2 = lm(y~x, data = dados4.2)

# Inferência aos parâmetros
summary(modelo4.2)
# H0: B0 = 0
# H1: B0 != 0
# VP = 0.0151 < 0.05, rejeitar H0, B0 é diferente de 0

# H0: B1 = 0
# H1: B1 != 0
# VP = 0.0051 < 0.05, rejeitar H0, B1 é diferente de 0

# B0 e B1 != 0

# Teste de normalidade dos resíduos
residuals(modelo4.2)

qqnorm(residuals(modelo4.2), pch=20, main='QQ-plot: resíduos')
qqline(residuals(modelo4.2), col='red', lwd=2)

shapiro4.2 = shapiro.test(residuals(modelo4.2))
shapiro4.2
# p-valor = 0.7267 > 0.05, não rejeitamos H0, a normalidade dos resíduos é assumida

# Homocedasticidade da variância
bptest(modelo4.2)
# H0: G^2 é homogêneo
# H1: G^2 não é homogêneo
# VP = 0.35 > 0.05, logo o pressuposto é validado

# Independência dos resíduos com variância constante
plot(fitted(modelo4.2),
     residuals(modelo4.2),
     xlab='valores preditos', ylab='resíduos', pch=20)
abline(h=0, lty=2, lwd=1.5)
lines(smooth.spline(fitted(modelo4.2),
                    residuals(modelo4.2)),
      col='red', lwd=2)

# A independência dos resíduos não é assumida, pois os dados seguem uma tendência



#### Modelo de Regressão Linear Múltipla ####
library(lmtest)

# Inserir os dados

dados2 = data.frame(
  x1 = c(5.0, 5.8, 4.2, 6.0, 4.8, 5.6, 4.4, 5.2, 5.4, 4.6),
  x2 = c(7.2, 7.8, 8.1, 8.7, 6.6, 7.5, 9.0, 6.3, 8.4, 6.9),
  y = c(51.7, 56.4, 49.3, 60.7, 48.9, 54.1, 54.9, 49.8, 57.9, 50.4)
)

dados2

## Criar o modelo

modelo_RLM = lm(y ~ x1 + x2, data=dados2)

summary(modelo_RLM)

# Teste de significância estatística global (anova)

summary(modelo_RLM)
# F-statistic: 71.34, p-value: 2.212e-05

# Teste de significância estatística aos parâmetros (anova)

summary(modelo_RLM)
# B1, ET = 7.919, p-value: 9.73e-05
# B2, ET = 8.186, p-value: 7.87e-05

# Análise ao coeficiente de determinação R^2

R2 = 0.9532
# 95% da variação de y é explicada pelo modelo

# Análise de significância aos Resíduos do modelo:

modelo_RLM$residuals

# Indepedência

plot(modelo_RLM$residuals)
# Os resíduos são independentes pois eles seguem um padrão aleatório

# Homocedasticidade

bptest(modelo_RLM)
# p-valor > 0.05, não rejeitar H0, os dados são homogêneos

# Normalidade

shapiro.test(modelo_RLM$residuals)
# p-valor > 0.05, não rejeitar H0, os dados são normalmente distribuídos

# Efetuando previsão de Y para (X1, X2) = (6.1, 8.0)

previsao = predict(modelo_RLM,
                   newdata =data.frame(x1 = 6.1, x2 = 8),
                   interval = 'prediction',
                   level = 0.95)
previsao
