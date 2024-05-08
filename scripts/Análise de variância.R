## Metodologia ANOVA - Análise de variância ##
# Exemplo (p. 301, livro de texto, c13/1)

# Passo 1: Introduzir os dados num data.frame

dados = data.frame(
  instituto = c(rep('A',5), rep('B',6), rep('C',6), rep('D',5)),
  custo = c(1.0, 0.8, 1.9, 1.1, 2.7,
            1.7, 2.5, 3, 2.2, 3.7, 1.9,
            1.0, 1.3, 3.2, 1.4, 1.3, 2.0,
            3.8, 2.8, 1.9, 3.0, 2.5)
            )

dados

# Passo 2: Construir o modelo ANOVA

modelo = aov(custo~instituto, data=dados)
summary(modelo)

# Exercício 3.2

dados3.2 = data.frame(
  tratamento = c(rep('A',10), rep('B', 10), rep('C', 10)),
  produtividade = c(14, 13, 20, 15, 13, 15, 19, 18, 11, 13,
                    13, 14, 13, 18, 15, 15, 16, 15, 16, 15,
                    19, 16, 17, 20, 19, 16, 18, 21, 19, 16)
)
dados3.2

modelo3.2 = aov(produtividade~tratamento, data=dados3.2)
summary(modelo3.2)

sum(residuals(modelo3.2))

# Verificando os pressupostos do modelo ANOVA

# 1. Independência:
# Como as 3 amostras são equilibradas há garantia de independência mútua dos resíduos do modelo.

# 2. Testes à normalidade

shapiro.test(dados3.2$produtividade) # dos Dados

shapiro.test(residuals(modelo3.2)) # dos Resíduos

# 3. Averiguar a homocedastidade da variância

bartlett.test(produtividade~tratamento, data=dados3.2)

# 4. Comparações múltiplas com método de tukey

TukeyHSD(modelo3.2)
