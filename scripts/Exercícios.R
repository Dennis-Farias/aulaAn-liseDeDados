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