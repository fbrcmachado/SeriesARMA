# Ativando o módulo de séries temporais
library(tseries)


serie<-read.ts("telesp.txt", header = FALSE, sep = "", skip = 0)

# Gráfico da serie

ts.plot(serie)

# teste para verificar se a serie e estacionaria

adf.test(serie)

# Gráfico do ACF e PACF

par(mfrow=c(1,2))
acf(serie)
pacf(serie)



# AR com média diferente de zero

arima(serie, order = c(2,0,0) )


# AR com média ZERO

arima(serie, order = c(2,0,0) , include.mean=FALSE)


# Análise dos resíduos do modelo

modelo <- arima(serie, order = c(2,0,0) , include.mean=FALSE)

par(mfrow=c(1,2))
acf(residuals(modelo))
pacf(residuals(modelo))


# AR Incompleto

# AR com média zero

modelo<-arma(serie, order = c(1,1), lag = list(ar=c(5,14,20)),include.intercept = FALSE)
modelo
modelo$asy.se.coef

for(i in 1:length(modelo$coef)) print(modelo$coef[i]/modelo$asy.se.coef[i])

# Análise dos resíduos do modelo

res<- modelo$res

n <- length(serie)

par(mfrow=c(1,2))
acf(res[21:n])
pacf(res[21:n])




# AR com média diferente de zero

modelo<-arma(serie, order = c(1,1), lag = list(ar=c(5,14)))
modelo
modelo$asy.se.coef

for(i in 1:length(modelo$coef)) print(modelo$coef[i]/modelo$asy.se.coef[i])

# Análise dos resíduos do modelo

res<- modelo$res

n <- length(serie)

par(mfrow=c(1,2))
acf(res[21:n])
pacf(res[21:n])





################# 
## Media Movel ##
#################


# Importando o arquivo com a série temporal para o R

# Salvar a série temporal no Excel com o formato texto(separado por tabulações) 

serie<-read.ts("serie23.txt", header = FALSE, sep = "", skip = 0)

# Gráfico da serie

ts.plot(serie)

# teste para verificar se a serie e estacionaria

adf.test(serie)


# Gráfico do ACF e PACF

par(mfrow=c(1,2))
acf(serie)
pacf(serie)

# Modelo ARMA com intercepto

modelo <- arima(serie, order = c(0,0,2))
acf(residuals(modelo))
pacf(residuals(modelo))

# Modelo ARMA sem intercepto

modelo <- arima(serie, order = c(0,0,2), include.mean=FALSE)
acf(residuals(modelo))
pacf(residuals(modelo))

# Análise dos resíduos do modelo

modelo <- arima(serie, order = c(0,0,1), include.mean=FALSE)
acf(residuals(modelo))
pacf(residuals(modelo))
