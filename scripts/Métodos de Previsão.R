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
