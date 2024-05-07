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