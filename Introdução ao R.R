# Criando um vetor
idade <- c(17, 20, 18, 19, 19)

# Procurando o número na posição 4
idade[4]

# Todos os números menos o que está na posição 4
idade[-4]

# Números na posição 1, 3, 5
idade[c(1,3,5)]

# Idade maior ou igual a 18
idade[idade >= 18]

# Vetor
nomes <- c("Ana", "Bento", "Chico", "Dani", "Eva")

# Atribuindo nomes aos elementos de um vetor
names(idade) <- nomes
idade

# Acessando posições
idade[c("Eva", "Bento", "Ana")]

# seq é uma função de sequência, parecido com um for
help(seq)

# Começa no 1, vai até o 5, de 2 em 2
seq(1, 5, 2)

# Três números começando em 2 e terminando em 12
seq(2, 12, length.out=3)

# rep é uma função de repetição
help(rep)

# sort é uma função de ordenamento
help(sort)

# Vetor: 1,2,4,8,16,...,1024
x <- 2^seq(0, 10)
x

# Vetor: 1,-1, 1, -1, ... (22 elementos)
y <- rep(c(1, -1), time = 11)
y

# Vetor: números ímpares menores que 100, do maior para o menor
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

# Múltiplos de 3 ou 5 no vetor z
u <- z[z %% 3 == 0 | z %% 5 == 0]
u

# Múltiplos de 3 e menores que 50
u <- z[z %% 3 == 0 & z<50]
u

# samp é a função para gerar números aleatórios
help(sample)

# Gerar números aleatórios de 1 a 10
sample(1:10)

# De 1  a 10, 8 números, com repetição
sample(10, 8, replace=TRUE)

# Criar Data.Frames
# Passo 1: Criar as variáveis da tabela

idade <- c(17, 20, 18, 19, 19)
idade

nomes <- c("Ana", "Bento", "Chico", "Dani", "Eva")
nomes

nota <- c(15, 14, 17, 12, 14)
nota

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
alunos$sexo

# Acrescentar uma variável qualitativa ordinal (Fator ordenado)
alunos$rend <- c("A", "M", "A", "B", "M") # criar variável
alunos$rend <- ordered(alunos$rend, levels = c("B", "M", "A"))

alunos
alunos$rend
