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

# A função attach expõe as colunas de um data frame
attach(alunos)

alunos$idade

# Tirar a média das notas, combinadas por sexo e rendimento (dados qualitativos)
tapply(nota, list(sexo, rend), mean) # mean é média

tapply(nota, list(sexo, rend), median) # median é a mediana

data()

help(HairEyeColor)

# A função head mostra as 6 primeiras linhas
head(HairEyeColor)

# A função summary mostra algumas estatísticas para cada variável
summary(HairEyeColor)

# Lendo ficheiros/arquivos

# O diretório precisa ser escolhido para poder achar o arquivo
wq = read.table("winequality.csv", # nome do ficheiro
                header = TRUE, # a linha 1 tem os nomes das variáveis
                sep = ";" # separador de variáveis
                )

# Outra forma de ler aquivos csv
wq2 = read.csv("winequality.csv",
               header = TRUE, sep = ';')

head(wq)
str(wq)
summary(wq)

# Salvando ficheiros no computador
write.csv(alunos, "alunos.csv", row.names = TRUE)

# Lendo o ficheiro anterior
read.csv("alunos.csv", header = TRUE, sep = ",", row.names = 1)
read.table("alunos.csv", header = TRUE, sep = ",", row.names = 1)

# Separando registros
wq_r = wq[wq$type == "red", ] # seleciona os tintos
wq_w = wq[wq$type == "white", ] # seleciona os brancos

# Salvando os registros separados
write.csv(wq_r, "winequality_red.csv", row.names = FALSE)
write.csv(wq_w, "winequality_white.csv", row.names = FALSE)

# Salvando apenas 300 linhas
wq_300 = read.csv("winequality.csv",
                  sep = ";",
                  stringsAsFactors = TRUE,
                  nrows = 300)

# Criar gráficos

colors() # Todas as cores disponíveis

plot(wq_300$type)

plot(wq_300$alcohol,
     main = "Teor Alcoólico",     # título
     xlab = "Índice",             # rótulo horizontal (eixo x)
     ylab = "Teor alcoólico (%)", # rótulo vertical (eixo y)
     col = "red")                 # cor

plot(wq_300$type,
     main = "Tipo de Vinho",           # título
     xlab = "Tipo",                    # rótulo horizontal (eixo x)
     ylab = "Quantidade",              # rótulo vertical (eixo y)
     col = c("maroon", "yellowgreen")  # cor
     )

# Gráfico de 2 variáveis
cores = c("maroon", "yellowgreen") # Criando vetor com cores
plot(wq_300$pH, wq_300$alcohol,
     main = "Relação pH-teor alcoólico",
     xlab = "pH",
     ylab = "Teor alcoólico (%)",
     col = cores[wq_300$type]
     )
legend(
  "bottomright",        # posição da legenda
  levels(wq_300$type),  # texto da legenda
  pch = 1,              # símbolos da legenda
  col = cores           # cores da legenda
)
# Adicionar tendência linear e algum texto
abline(reg = lm(wq_300$alcohol ~ wq_300$pH), lty = "dashed")
text(3.8, 13.5, "Se beber, não conduza!", cex = 0.8)

# Para ciração de gráficos além do plot(), existem o hist() [histograma], barplot() [gráfico de barras], boxplot() [gráfico de caixas]
# Posso salvar os gráficos


