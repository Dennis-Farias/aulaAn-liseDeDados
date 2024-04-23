### Métodos de Previsão ###
install.packages('fpp2')

library(fpp2)
library(Mcomp)
library(expsmooth)
library(fma)
library(forecast)
library(ggplot2)
library(graphics)

help(beer)
plot(beer)
beer %>% ses(PI=F) %>% autoplot

## Capítulo 5 ##
## Exercício 5.1
## a)
dole %>% ses(PI=F) %>% autoplot
autoplot(dole)
# Tendência estacionária, sazonalidade: mista

cowtemp %>% ses(PI=F) %>% autoplot
autoplot(cowtemp)
# Tendência estacionária, sazonalidade: não tem, ciclo: não tem

lynx %>% ses(PI=F) %>% autoplot
autoplot(lynx)
# Tendência estacionária, sazonalidade: aditiva, ciclo: tem

USAccDeaths %>% ses(PI=F) %>% autoplot
autoplot(USAccDeaths)
# Tendência estacionária, sazonalidade: aditiva, ciclo: não tem

bricksq %>% ses(PI=F) %>% autoplot
autoplot(bricksq)
# Tendência mista, sazonalidade: multiplicativa, ciclo: não tem

# Função de autocorrelação
acf(beer, 36)

## b) 
acf(dole,40)
acf(cowtemp, 40)
acf(lynx, 40)
acf(USAccDeaths, 40)
acf(bricksq, 40)

# Exercício 5.4
shipex
autoplot(shipex)
acf(shipex)

MM_3 = ma(shipex, 3, centre = FALSE)
MM_5 = ma(shipex, 5, centre = FALSE)
MM_7 = ma(shipex, 7, centre = FALSE)

MM_3x3 = ma(MM_3, 3, centre = TRUE) # Média móvel centrada 3x3
MM_5x5 = ma(MM_5, 5, centre = TRUE) # Média móvel centrada 5x5

print(cbind(MM_3, MM_5, MM_7, MM_3x3, MM_5x5))

# Exercício 5.7
y = c(99, 120, 139, 160,
      88, 108, 127, 148,
      93, 111, 131, 150,
      111, 130, 152, 170)

# Informar o R que o vetor de dados z representa uma série temporal trimestal

z = ts(y, frequency = 4)
autoplot(z)

# a) Estimar Tendência Média Móvel Centrada: ma()

MM_4 = ma(y, 4, centre = FALSE)
MM_4
MM_4x2 = ma(MM_4, 2, centre = TRUE)
MM_4x2

library(graphics)
autoplot(z, series='Dados') +
  autolayer(ma(z,4), series = '4-MA') +
  xlab('Year') + ylab('') +
  ggtitle('Exercício 3.7') +
  scale_colour_manual(values=c('Data'='grey50', '4-MA'='red'),
                      breaks = c('Data', '4-MA'))

# b) Estimar a Sazonalidade pela decom.  Aditiva: decompose()

decomp.adit = decompose(z, 'additive')

decomp.adit

plot(decomp.adit)

summary(decomp.adit)

# Exercício 5.8

# a)

dados = plastics

autoplot(dados)

# Existem flutações sazonais multiplicativas e tendência crescente

# Exercício 5.12

AESimples_12 = ses(eknives, h=1)
AESimples_12
summary(AESimples_12)

# Exemplo carregamentos de abre-latas eléctricos
 
dados =  ts(c(200,135, 195, 197.5, 310, 175, 155, 130, 220, 277.5, 235))

AESimples_0.1 = ses(dados, h=1, alpha = 0.1)
AESimples_0.1
summary(AESimples_0.1)

AESimples_0.5 = ses(dados, h=1, alpha = 0.5)
AESimples_0.5
summary(AESimples_0.5)

AESimples_0.9 = ses(dados, h=1, alpha = 0.9)
AESimples_0.9
summary(AESimples_0.9)

# Exercício 5.13

# a)

dados5.13 = ts(c(2,4,6,8,10,12,14,16,18,20))
autoplot(dados5.13)

AESimples_13 = ses(dados5.13, h=1)
summary(AESimples_13)

# b)

holt_13 = holt(dados5.13, h=1)
summary(holt_13)

# Exercício 5.16

dados5.16 = AirPassengers
dados5.16
summary(dados5.16)
autoplot(dados5.16)

# Amortecimento exponencial simples 
AESimples_16 = ses(dados5.16, h=24)
summary(AESimples_16)
autoplot(AESimples_16)

# Método de holt
holt_16 = holt(dados5.16, h=24)
summary(holt_16)
autoplot(holt_16)

# Método de holt-winters aditivo
holt_winters_ad_16 = hw(dados5.16, seasonal = 'additive', h=24)
summary(holt_winters_ad_16)
autoplot(holt_winters_ad_16)

# Método de holt-winters multiplicativo
holt_winters_mul_16 = hw(dados5.16, seasonal = 'multiplicative', h=24)
summary(holt_winters_mul_16)
autoplot(holt_winters_mul_16)
