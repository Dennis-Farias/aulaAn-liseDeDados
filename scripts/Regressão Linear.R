### Regressão Linear ###
## Metodologia
# Cria o modelo
# Análise de significância
# Teste Anova à Regressão
# Análise de Resíduos: Independência, Normalidade, Homocedasticidade
# Coeficiente de Determinação: R^2

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

# Exercício 4.2

dados4.2 = data.frame(
  x = c(12.43, 13.52, 14.42, 15.24, 16.06, 16.88, 17.70, 17.84, 18.76),
  y = c(0.049, 0.064, 0.059, 0.051, 0.073, 0.095, 0.245, 0.222, 0.350)
)

cor.test(dados4.2$x, dados4.2$y, 'spearman', alternative = 'two.sided')

# p-valor < 0.05, as variáveis estão correlacionadass

plot(dados4.2$x, dados4.2$y)

# Cria o modelo