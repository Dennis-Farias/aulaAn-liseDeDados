# Exercício 1.1

dados11 = c(82, 81.5, 82.9, 80.5, 79.5, 80.5, 80.7)

mean(dados11)
sd(dados11)

qt(0.95, 6, FALSE)

pt(2.54, 6, lower.tail = FALSE)

t.test(dados11, mu=80, alt='g', conf.level = 0.95)

# Rejeitar H0 pois p-valor < 0.05 que é o nível de significância

# Exercício 2

dados12 = read.csv('1.2.csv', header = TRUE)

qt(0.95, 29, FALSE)

t.test(dados12, mu=40, alt='g', conf.level = 0.95)

# Rejeitar H0 pois t > qt

# Teste a razão de variância: var.test()

# H0
# H1

dados1.3 = read.csv('1.3.csv', header = TRUE, sep = ';')

var.test(dados1.3$FornecedorA, dados1.3$FornecedorB, ratio = 1, alternative = 'g')

# Outra forma de escrever
var.test(dados1.3$FornecedorA, dados1.3$FornecedorB, mu=0, alternative = 'g')

# p-valor > 0.05, devemos rejeitar H1

## Exercício 1.4
# Proporção binomial
# H0: p (probabilidade) = 1/6
# n = 12, y = 5, p = 5/12
# y = número de vezes que foi a face 6, em 12 tentativas
# H1: p > 1/6
# ET = y = 5
# Se H0 for verdade -> ET -> B (n=12, p = 1/6)
# VP = P(B > ET) = P(B > 5) = 3.63% < & = 5%, logo H0 deve ser rejeitado
# Há evidência estatística para afirmar que o dado está viciado a um níve de 5% de significância

pbinom(4, 12, 1/6, lower.tail = FALSE, log.p = FALSE)

binom.test(5, 12, p = 1/6, alternative = "greater", conf.level = 0.95)

## Exercício 1.5
# Proporção binomial
# H0: p = 5/10
# n = 10, y = 7, p = 7/10
# H1: p > 0.5
# ET = y = 7
# Se H0 for verdade -> ET -> B(n=10, p=0.5)
# VP = P(B > 7) = 0.1719 > 0.05, logo não rejeitar H0
# Não há evidência estatística para afirmar que a maioria das pessoas tenha ficado negativamente impressionado

pbinom(6, 10, 0.5, lower.tail = FALSE, log.p = FALSE)

binom.test(7, 10, p = 0.5, alternative = "greater")

## Exercício 1.7
# Razão de variância (G)
# H0 = G1 = G2
# H1 = G1 > G2 [Melhora a precisão -> Menor variância = maior precisão]
# ET = S1/S2 = 2.033
# Se H0 for verdadeiro -> ET -> F 13,9
#VP = P(F 13,9 > ET) = 0.1446 > 0.05
# Não rejeitar H0, pois p-valor 0.14 > 0.05
# Não há evidência estatística para afirmar que a experiência melhora a precisão da análise

amostra1 = c(4.40, 4.56, 4.42, 4.59, 4.61, 4.45, 4.58, 4.39, 4.77, 4.72, 4.69, 4.53, 4.90, 4.50)
amostra2 = c(4.42, 4.47, 4.70, 4.72, 4.53, 4.55, 4.60, 4.64, 4.71, 4.52)
var.test(amostra1, amostra2, ratio = 1, alternative = 'g')

## Exercício 1.14
# Proporção binomial
# H0: p = 0.2
# n = 500, y = 130, p = 130/500
# H1: p > 0.2
# ET = y = 130
# Se H0 for verdade -> ET -> B(n=500, p=0.2)
# Devemos rejeitar H0 se VP < &
# VP = P(B > 130) = 0.00068 < 0.01, logo rejeitar H0
# Há evidência estatística para afirmar que mais de 20% das faturas tem erro

binom.test(130, 500, p = 0.2, alternative = "greater", conf.level = 0.99)

## Exercício 1.9 a)
# Média
# H0: x = 14
# H1: x > 14
# Se H0 for verdade -> ET -> t9
# Devemos rejeitar H0 se ET > t9
# p-valor = 0.04 < 0.05, logo rejeitar H0
# Há evidência estatística para afirmar que o prazo médio de pagamento é mais de 14 dias
dados1.9 = c(34,3,4,44,29,17,32,14,28,16)
t.test(dados1.9, mu=14, alt='g', conf.level = 0.95)

# b)


