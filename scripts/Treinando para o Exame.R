# Exame modelo AD - Regressão Linear
library(lmtest)

# Inserir os dados

dados = data.frame(
  x = c(20, 40, 48, 60, 80, 100),
  y = c(14.5, 31, 36, 45.5, 59, 73.5)
)

# Criar o modelo

modelo = lm(y ~ x, data=dados)

plot(dados$x, dados$y)

# y = 0.91845 + 0.72958.x

# Inferência aos parâmetros

summary(modelo)

# H0: B0 = 0
# H1: B0 != 0
# ET = 1.127
# VP = 0.323 > 0.05, não rejeitar H0, B0 é igual a 0

# H0: B1 = 0
# H1: B1 != 0
# ET = 56.990
# VP = 0.00 < 0.05, não rejeitar H0, B1 é diferente de 0 

# Como B1 é diferente de 0, a função é válida para definir o valor de y
# B1 é estatísticamente significativo

# Teste de normalidade dos resíduos

shapiro.test(residuals(modelo))
# H0: Os dados são normais
# H1: Os dados não são normais
# p-valor = 0.6227, não rejeitamos H0, a normalidade dos dados é assumida

# Homocedasticidade da variância

bptest(modelo)
# H0: G^2 é homogêneo
# H1: G^2 não é homogêneo
# VP = 0.08 > 0.05, logo o pressuposto é validado

# Independência dos resíduos

plot(modelo$residuals)
# De acordo com o gráfico de dispersão dos resíduos, não há nenhum padrão atípico perceptivo
# Então o pressuposto de independência é garantido.

# Efetuando previsão de Y se x = 60

previsao = predict(modelo,
                   newdata =data.frame(x = 60),
                   interval = 'prediction',
                   level = 0.95)
previsao

# Valor da previsão = 44.70
# Valor máx = 47.17
# Valor mín = 42.24

# Coeficiente de determinação amostral R^2

summary(modelo)
# R^2:  0.99
# 99% da variação de y é explicada pelo modelo

# Fazer teste de significância global do modelo se necessário