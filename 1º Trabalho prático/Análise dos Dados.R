# Análise dos preços dos combustíveis no Brasil por região
dados = read.csv('combustiveis-regioes.csv', sep = ',')
dados
# Removendos valores NA
summary(dados$gas_natural_veicular_gnv_preco_revenda_max)
dados_final = dados
dados_final = dados[, -c(6,7,8,9,10,12,13,15,16,17,18,19,21,22,23,24,25)]
dados_final
if (any(is.na(dados_final))) {
  print("Existem valores NA no conjunto de dados.")
} else {
  print("Não existem valores NA no conjunto de dados.")
}
# Agrupando
dados_agrupados = dados_final %>%
  group_by(ano)

## Variáveis
# Gasolina comum, etanol hidratado, óleo diesel, gas cozinha

# Estatísticas descritivas
summary(dados_final$gasolina_comum_preco_revenda_avg)
summary(dados_final$etanol_hidratado_preco_revenda_avg)
summary(dados_final$oleo_diesel_preco_revenda_avg)
summary(dados_final$gas_cozinha_glp_preco_revenda_avg)

# Boxplot
boxplot(dados_final$gasolina_comum_preco_revenda_avg, outline = F, horizontal = T)
boxplot(dados_final$etanol_hidratado_preco_revenda_avg, outline = F, horizontal = T)

# Histograma
hist(dados$gasolina_comum_preco_revenda_avg)