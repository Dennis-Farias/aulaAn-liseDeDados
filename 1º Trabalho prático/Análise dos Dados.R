# Análise dos preços dos combustíveis no Brasil por região
dados = read.csv('combustiveis-regioes.csv', sep = ',')
dados
hist(dados$gasolina_comum_preco_revenda_avg)
boxplot(dados$gasolina_comum_preco_revenda_avg, outline = F, horizontal = T)
# Decidir quais dados serão trabalhados, eliminar os que são desnecessários
# Começar a fazer as análises
