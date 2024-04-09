### Inferência não-paramétrica ###
## Teste de Indepêndencia do qui-quadrado
## Exemplo c2/72
## (p.294, livro de texto)

# 1º passo inserir os dados observados
#help('matrix')

m = matrix(c(12,8,7,3,9,10,7,4,2,9,8,11),
           nrow = 3,
           ncol = 4, byrow = T)
m

# 2º passo: código R
chisq.test(m)
# p-valor < 0.05, rejeitar H0

## Exercício 2.1
# H0: O jornal desportivo é independenete do clube do qual o indivíduo é adepto
# H1: O jornal desportivo é dependente do clube do qual o indivíduo é adepto
# dados observados
m2 = matrix(c(64, 37, 38, 25, 29, 26, 50, 21, 36, 55, 26, 27, 12, 17, 18, 19),
            nrow = 4,
            ncol = 4, byrow = T)
colnames(m2) = c('Verm', 'Verdes', 'Azuis', 'Outros')
rownames(m2) = c('A Tola', 'A Joga', 'Record', 'Outros')
m2

# Teste de independência do qui-quadrado
chisq.test(m2)

# p-valor < 0.05, logo rejeitar H0
# Então o jornal desportivo está associada com o clube de eleição do adepto


## Exercício 2.2
# H0: O rendimento familiar é independente da região
# H1: O rendimento familiar é dependente da região
# dados observados:
m3 = matrix(c(28,42,30,24,44,78,78,76),
            nrow = 2,
            ncol = 4, byrow = T)
colnames(m3) = c('BAIXO', 'MÉDIO', 'ALTO', 'MUITO ALTO')
rownames(m3) = c('Norte', 'Sul')
m3

# Teste de independência do qui-quadrado
chisq.test(m3)

# p-valor > 0.05, logo não rejeitar H0
# Há evidência estatística para afirmar que o rendimento familiar é independente da região
# Não há evidência estatísitca para afirmar que o rendimento familiar é dependente da região

## Teste de correlação bivariada: dados quantitativos contínuos ##

## Exercício 2.9
x = c(6.93, 6.76, 5.94, 7.7, 5.61, 6.32, 7.08, 5.3, 5.86, 6.04, 7.13, 6.76)
y = c(12.1, 12.4, 13, 11.9, 14.2, 13.6, 12.7, 14.2, 13.7, 13.3, 12.8, 13.4)


plot(x,y, pch=20) # Diagrama de dispersão

cor(x, y) # coef. correlação amostral

cor.test(x, y, alternative = 'l', method = 'spearman',
         exact = FALSE, continuity = TRUE)

## Exemplo c12/15 (p. 268, livro de texto)
x = c(11.9, 10.6, 13.3, 11.6, 12.9, 10.4, 11.3, 13.5, 9.1, 8.2,
      11.6, 10.0, 11.3, 10.3, 8.4, 9.9, 11.0, 10.3, 13.2, 9.9)

ks.test(x, 'pnorm', 10, 1, alternative = 't')

## Exercício 2.4
x = c(130, 150, 150, 150, 140, 140, 160, 140, 140, 150, 160)
ks.test(x, 'pnorm', 147, sqrt(90), alternative = 't')
# p-valor = 0.63 < 0.05, logo não rejeitar H0

## Exercício 2.6
x = c(2326, 694, 1524, 1955, 490, 208, 1839, 641,
      1181, 2375, 2839, 141, 2181, 3375, 363, 944)
mean(x) # Estimativa para Beta = 1442.25
ks.test(x, y = 'pexp', rate = 1/1442.25, alternative = 't')

## Exercício 2.5
# Os parametros (média e desvio padrão) da dist. normal são estimados a partir da amostra
# Eles não são dados no exercício
library(nortest)
dados2.5 = c(6.0,2.3,4.8,5.6,4.5,3.4,3.3,1.9,4.8,4.5)
lillie.test(dados2.5)

## Exercício 2.8
# Teste do qui-quadrado 
dados2.8 = c(64,29,36,12)
chisq.test(dados2.8, p = c(9/16, 3/16, 3/16, 1/16))
# Teste para diferentes probabilidades

## Exercício 2.10
# Teste do qui-quadrado
dados2.10 = c(37,110,80,162,111)
chisq.test(dados2.10, p=c(1/6, 1/4, 1/6, 1/4, 1/6))
# p-valor < 0.05, logo rejeitar H0, há evidência estatística para afirmar que os dados estão viciados

