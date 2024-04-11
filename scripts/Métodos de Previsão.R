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
## Exercício 1
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
