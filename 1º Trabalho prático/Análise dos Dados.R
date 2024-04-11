# Análise dos preços dos combustíveis no Brasil por região
dados = read.csv('combustiveis-regioes.csv', sep = ',')
dados
# Removendos valores NA
dados_final = dados[, -c(6,7,8,9,10,12,13,15,16,17,18,19,21,22,23,24,25)]
dados_final

# Verificando se existem valores NA
if (any(is.na(dados_final))) {
  print("Existem valores NA no conjunto de dados.")
} else {
  print("Não existem valores NA no conjunto de dados.")
}

## Variáveis
# Gasolina comum, etanol hidratado, óleo diesel, gas cozinha

### Análise dos Dados Quantitativos ###

## Estatísticas descritivas ##

summary(dados_final$gasolina_comum_preco_revenda_avg)
summary(dados_final$etanol_hidratado_preco_revenda_avg)
summary(dados_final$oleo_diesel_preco_revenda_avg)
summary(dados_final$gas_cozinha_glp_preco_revenda_avg)

## Boxplot ##

# Cores
cores_regioes = c('orange', 'red', 'green', 'yellow', 'blue')

boxplot(dados_final$gasolina_comum_preco_revenda_avg ~ dados_final$regiao,
        outline = F, 
        main = 'Preço da Gasolina Comum por região do Brasil',
        ylab = 'Preço R$',
        xlab = 'Região', 
        col = cores_regioes)
boxplot(dados_final$etanol_hidratado_preco_revenda_avg ~ dados_final$regiao, 
        outline = F,
        main = 'Preço do Etanol Hidratado por região do Brasil',
        ylab = 'Preço R$',
        xlab = 'Região',
        col = cores_regioes)
boxplot(dados_final$oleo_diesel_preco_revenda_avg ~ dados_final$regiao, 
        outline = F,
        main = 'Preço do Óleo Diesel por região do Brasil',
        ylab = 'Preço R$',
        xlab = 'Região',
        col = cores_regioes)
boxplot(dados_final$gas_cozinha_glp_preco_revenda_avg ~ dados_final$regiao, 
        outline = F,
        main = 'Preço do Gás de Cozinha por região do Brasil',
        ylab = 'Preço R$',
        xlab = 'Região',
        col = cores_regioes)

## Histogramas ##

hist(dados_final$gasolina_comum_preco_revenda_avg, 
     main = 'Preço da Gasolina Comum no Brasil',
     ylab = 'Frequência',
     xlab = 'Preço R$',
     col = 'brown')

hist(dados_final$etanol_hidratado_preco_revenda_avg, 
     main = 'Preço do Etanol Hidratado no Brasil',
     ylab = 'Frequência',
       xlab = 'Preço R$',
     col = 'darkolivegreen3')

hist(dados_final$oleo_diesel_preco_revenda_avg, 
     main = 'Preço do Óleo Diesel no Brasil',
     ylab = 'Frequência',
     xlab = 'Preço R$',
     col = 'darkblue')

hist(dados_final$gas_cozinha_glp_preco_revenda_avg, 
     main = 'Preço do Gás de Cozinha no Brasil',
     ylab = 'Frequência',
     xlab = 'Preço R$',
     col = 'cyan')

## Diagramas de dispersão ##

# Cores para cada variável
cores_variaveis = c('brown', 'darkolivegreen3', 'darkblue', 'cyan')
# Gasolina Comum: 'brown'; Etanol Hidratado: 'darkolivegreen3'; Óleo Diesel: 'darkblue'; Gás de Cozinha: 'cyan'

# Relação entre gasolina comum e etanol hidratado

plot(dados_final$gasolina_comum_preco_revenda_avg, dados_final$etanol_hidratado_preco_revenda_avg,
     main = 'Relação entre a Gasolina Comum e o Etanol Hidratado', 
     col = cores_variaveis[c(1,2)],
     xlab = 'Preço da Gasolina Comum R$',
     ylab = 'Preço do Etanol Hidratado R$'
)
legend(
  'bottomright',
  legend = c('Gasolina Comum', 'Etanol Hidratado'),
  pch = 1,
  col = cores_variaveis[c(1,2)]
)
abline(reg = lm(dados_final$etanol_hidratado_preco_revenda_avg ~ dados_final$gasolina_comum_preco_revenda_avg,
                lty = 'dashed'))

# Relação entre gasolina comum e óleo diesel

plot(dados_final$gasolina_comum_preco_revenda_avg, dados_final$oleo_diesel_preco_revenda_avg,
     main = 'Relação entre a Gasolina Comum e o Óleo Diesel', 
     col = cores_variaveis[c(1,3)],
     xlab = 'Preço da Gasolina Comum R$',
     ylab = 'Preço do Óleo Diesel R$')
legend(
  'bottomright',
  legend = c('Gasolina Comum', 'Óleo diesel'),
  pch = 1,
  col = cores_variaveis[c(1,3)]
)
abline(reg = lm(dados_final$oleo_diesel_preco_revenda_avg ~ dados_final$gasolina_comum_preco_revenda_avg,
                lty = 'dashed'))

# Relação entre gasolina comum e gás de cozinha

plot(dados_final$gasolina_comum_preco_revenda_avg, dados_final$gas_cozinha_glp_preco_revenda_avg,
     main = 'Relação entre a Gasolina Comum e o Gás de Cozinha', 
     col = cores_variaveis[c(1,4)],
     xlab = 'Preço da Gasolina Comum R$',
     ylab = 'Preço do Gás de Cozinha R$')
legend(
  'bottomright',
  legend = c('Gasolina Comum', 'Gás de Cozinha'),
  pch = 1,
  col = cores_variaveis[c(1,4)]
)
abline(reg = lm(dados_final$gas_cozinha_glp_preco_revenda_avg ~ dados_final$gasolina_comum_preco_revenda_avg,
                lty = 'dashed'))

# Relação entre etanol hidratado e óleo diesel

plot(dados_final$etanol_hidratado_preco_revenda_avg, dados_final$oleo_diesel_preco_revenda_avg,
     main = 'Relação entre o Etanol Hidratado e o Óleo Diesel', 
     col = cores_variaveis[c(2,3)],
     xlab = 'Preço do Etanol Hidratado R$',
     ylab = 'Preço do Óleo Diesel R$')
legend(
  'bottomright',
  legend = c('Etanol Hidratado', 'Óleo Diesel'),
  pch = 1,
  col = cores_variaveis[c(2,3)]
)
abline(reg = lm(dados_final$oleo_diesel_preco_revenda_avg ~ dados_final$etanol_hidratado_preco_revenda_avg,
                lty = 'dashed'))

# Relação entre etanol hidratado e gás de cozinha

plot(dados_final$etanol_hidratado_preco_revenda_avg, dados_final$gas_cozinha_glp_preco_revenda_avg,
     main = 'Relação entre o Etanol Hidratado e o Gás de Cozinha', 
     col = cores_variaveis[c(2,4)],
     xlab = 'Preço do Etanol Hidratado R$',
     ylab = 'Preço do Gás de Cozinha R$')
legend(
  'bottomright',
  legend = c('Etanol Hidratado', 'Gás de Cozinha'),
  pch = 1,
  col = cores_variaveis[c(2,4)]
)
abline(reg = lm(dados_final$gas_cozinha_glp_preco_revenda_avg ~ dados_final$etanol_hidratado_preco_revenda_avg,
                lty = 'dashed'))

# Relação entre óleo diesel e gás de cozinha

plot(dados_final$oleo_diesel_preco_revenda_avg, dados_final$gas_cozinha_glp_preco_revenda_avg,
     main = 'Relação entre o Óleo Diesel e o Gás de Cozinha', 
     col = cores_variaveis[c(3,4)],
     xlab = 'Preço do Óleo Diesel R$',
     ylab = 'Preço do Gás de Cozinha R$')
legend(
  'bottomright',
  legend = c('Óleo Diesel', 'Gás de Cozinha'),
  pch = 1,
  col = cores_variaveis[c(3,4)]
)
abline(reg = lm(dados_final$gas_cozinha_glp_preco_revenda_avg ~ dados_final$oleo_diesel_preco_revenda_avg,
                lty = 'dashed'))


# Análise de variância
an = aov(etanol_hidratado_preco_revenda_avg ~ regiao, data=dados_final)
summary(an)
# Não há diferença significativa entre o preço de gasolina por região, p-valor = 0.49
# Teste de tukey compara região por região, as regiões que mais existem diferenças de preços são sudeste e norte
TukeyHSD(an)

