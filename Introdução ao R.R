idade <- c(17, 20, 18, 19, 19)

idade[4]

idade[-4]

idade[c(1,3,5)]

idade[idade >= 18]

nomes <- c("Ana", "Bento", "Chico", "Dani", "Eva")

names(idade) <- nomes
idade

idade[c("Eva", "Bento", "Ana")]

help(seq)

seq(1, 5, 2)

help(rep)

help(sort)

x <- 2^seq(0, 10)
x

y <- rep(c(1, -1), time = 11)
y

z <- sort(seq(1, 100, 2), decreasing = TRUE)
z

#Selecionar os elementos 4,7 e 10 de cada vetor
x[c(4, 7, 10)]
y[c(4, 7, 10)]
z[c(4, 7, 10)]

# Calcular a soma dos elementos de cada vetor
sum(x)
sum(y)
sum(z)

# Divisão de vetores
x/2
y/2
z/2

# Soma de vetores
y+z
length(y)
length(z)
# Só pode somar vetores do mesmo tamanho

# Ordenar os elementos de z
w <- sort(z)
w

# Múltiplos de 3 no vetor z
u <- z[z %% 3 == 0]
u

u <- z[c(z*3 & z*5)]
u

u <- z[z %% 3 == 0 & z<50]
u

# Gerar números aleatórios
sample(1:10)
help(sample)
sample(10, 8, replace=TRUE)
# De 1  a 10, 8 números, com repetição

# Criar Data.Frames
# Passo 1: Criar as variáveis da tabela

idade <- c(17, 20, 18, 19, 19)
idade

nomes <- c("Ana", "Bento", "Chico", "Dani", "Eva")

nota <- c(15, 14, 17, 12, 14)

# Passo 2: Escrever o código para criar a tabela de dados

alunos <- data.frame(idade, nota)
alunos

row.names(alunos) <- nomes # acrescenta mais uma variável (coluna)
alunos

# Acrescentar uma variável qualitativa nominal

alunos$sexo <- c("F", "M", "M", "M", "F")
alunos

alunos$sexo <- factor(alunos$sexo) # Informa que sexo é uma var.
# qualitativa nominal (factor)
alunos

# Acrescentar uma variável qualitativa ordinal (Fator ordenado)
aluns$rend <- c("A", "M", "A", "B", "M") # criar variável
alunos$rend <- ordered(alunos$rend, levels = )


