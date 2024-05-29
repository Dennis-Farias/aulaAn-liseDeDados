# Regressão Logística

library(carData)

data('TitanicSurvival', package = 'carData')

show(TitanicSurvival)
summary(TitanicSurvival)
head(TitanicSurvival)

plot(TitanicSurvival$survived,
     TitanicSurvival$sex,
     col='blue',
     pch=20)

plot(TitanicSurvival$sex,
     TitanicSurvival$age,
     col='red',
     pch=10)

# Código para definir o modelo

modelo_log = glm(survived ~ age + sex + passengerClass,
                 data = TitanicSurvival, family = binomial)

summary(modelo_log)

# Interpretação dos coeficientes

exp(coef(modelo_log))

## Verificação dos pressupostos do Modelo Logístico

# Visualização Gráficas

plot(TitanicSurvival$survived ~ TitanicSurvival$passengerClass,
     data = TitanicSurvival,
     col='blue')

## Análise Inferencial aos pressupostos do modelo
# Pressuposto da Independência: Teste de Breusch-Pagan (bptest())

modelo_log$residuals

plot(modelo_log$residuals)

library(lmtest)

## Pressuposto da homogeneidade da Variância dos resíduos:
# teste de Durbin-Watson (dwtest())
dwtest(modelo_log)

# Pressuposto da Independência
bptest(modelo_log)

