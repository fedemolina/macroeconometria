
# Fijar el directorio o crear un proyecto y poner allí los datos. Se trabaja creando un proyecto.

###################### Carga, limpieza de datos y Est.Desciptiva ###################
library(xts)
library(reshape2)
library(ggplot2)
library(forecast)
library(purrr)
library(seasonal)
library(urca)
library(dlm)
library(VARsignR)
library(vars)
                    ####### Carga de datos
series <- readxl::read_excel("C:/Users/Usuario/Documents/MAESTRIA/MacroEconometria/svar/tcr_bcu.xls",
                             range="C9:N231")
# Voy a sacar a Argentina y, por lo tanto, dejo de trabajar con regional y tambien con Global.
# Me quedo con Extrarregional y Brasil

                    ####### Limpieza de datos
series <- readxl::read_excel("C:/Users/Usuario/Documents/MAESTRIA/MacroEconometria/svar/tcr_bcu.xls",
                             range="C9:N231")
series$Argentina <-  NULL #Sale Argentina porque ensucia todo.
series$Global <- NULL
series$Regional <- NULL
names(series)[7] <- "ReinoUnido"
series2 <- series[c("EEUU","China","Alemania","ReinoUnido","Italia","España","México","Brasil")]
series <- ts(series, start=c(2000,1), end=c(2018,6), frequency=12)
series2 <- ts(series2,start=c(200,1),end=c(2018,6), frequency=12)

                    ####### Estad?stica Descriptiva
summary(series)
apply(series,MARGIN=2,sd)

                    ####### Test de Ra?ces Unitarias ###############

summary(ur.df(series[,]))
lapply(apply(series, MARGIN=2,FUN=ur.df,type="drift",selectlags="AIC",lags=12),summary) 
lapply(apply(diff(series), MARGIN=2,FUN=ur.df,type="drift",selectlags="AIC",lags=12),summary) 
#Se agrega el drift para comenzar el test sino estoy sesgando el test hacia la hip.nula
#En ningún caso el t cae dentro de la regi?n de rechazo. Por lo tanto, no se rechaza Ho)
#Es decir, no se rechaza alfa=0 & mu=0
#Notar como si se diferencia la serie si se rechaza la hip?tesis nula de ra?z unitaria.
# Los resultados no cambian si hago la transformaci?n logaritmica.

# Se  realizo tambi?n la regresi?n sin intercepto y el resultado no cambio.
#lapply(apply(series, MARGIN=2,FUN=ur.df,type="none",selectlags="AIC",lags=12),summary)
#lapply(apply(series, MARGIN=2,FUN=ur.df,type="trend",selectlags="AIC",lags=12),summary)

                ########## Me restrinjo a Brasil y el Extrarregional ####

################## Cointegración ################

###### Dinámica de largo plazo y transitoria. Con eigen y con trace. Y con los 3 tipos.
for(i in 2:12){
    for(spec in c("longrun","transitory")){
        for (test in c("eigen","trace")){
            for (name in c("none","const","trend")){
                print("")
                print(c(spec,name,test))
                print("")
                print(ca.jo(series[,-1], type = test, ecdet = name, spec = spec)@teststat)
                print(ca.jo(series[,-1], type = test, ecdet = name, spec = spec)@cval)
            }
        }
    }
}
# No se rechaza H0) con r=2, se rechazo con r=1. No se rechaza que hayan 2 relaciones de coint

# Ahora agrego posible estacionalidad, por las dudas. Dado que algunas series mostraron posible existencia.
for(i in 2:12){
    for(spec in c("longrun","transitory")){
        for (test in c("eigen","trace")){
            for (name in c("none","const","trend")){
                print("")
                print(c(spec,name,test))
                print("")
                print(ca.jo(series[,-1], type = test, ecdet = name, spec = spec)@teststat)
                print(ca.jo(series[,-1], type = test, ecdet = name, spec = spec)@cval)
            }
        }
    }
}
# No se rechaza H0) con r=2, se rechazo con r=1. No se rechaza que hayan 2 relaciones de coint


# Brasil y Extrarregional
for (i in 2:12){
    for(spec in c("longrun","transitory")){
        for (test in c("eigen","trace")){
            for (name in c("none","const","trend")){
                print("")
                print(c(spec,name,test))
                print("")
                print(ca.jo(series[,1:2], type = test, ecdet = name, spec = spec,K=i)@teststat)
                print(ca.jo(series[,1:2], type = test, ecdet = name, spec = spec,K=i)@cval)
            }
        }
    }
}

# No se rechaza H0). No se rechaza que no haya relaciones de cointegracion
# En este caso puedo plantear un VAR com?n y corriente con las series diferenciadas.

# Agrego estacionalidad. Pero no cambia los resultados
for (i in 2:12){
    for(spec in c("longrun","transitory")){
        for (test in c("eigen","trace")){
            for (name in c("none","const","trend")){
                print("")
                print(c(spec,name,test))
                print(ca.jo(series[,1:2], type = test, ecdet = name, spec = spec,K=i,season=12)@teststat)
                print(ca.jo(series[,1:2], type = test, ecdet = name, spec = spec,K=i,season=12)@cval)
            }
        }
    }
}

##################################################################################
############################### Estimación Multivariante VAR/VEC #################

# Diferencio todas las series.
series.diff.log <- diff(log(series),d=1)
plot(series.diff.log)
series.diff <- diff((series),d=1)
plot(series.diff)

# Var entre Brasil y extrarregional
VAR <- list()
VAR[[1]] <- VAR(series.diff[,1:2],type="const",lag.max=15, ic="AIC")
VAR[[2]] <- VAR(series.diff[,1:2],type="trend",lag.max=15, ic="AIC")
VAR[[3]] <- VAR(series.diff[,1:2],type="both",lag.max=15, ic="AIC")
VAR[[4]] <- VAR(series.diff[,1:2],type="none",lag.max=15, ic="AIC")

for(i in 1:4){
    for(varcitos in c("Brasil","Extrarregional")){
        print(causality(VAR[[i]], cause=varcitos))
    }
}
# El caso de tendencia no es necesario porque las series ya son estacionarias. Por las dudas.
# En todos los casos se rechaza la hip?tesis nula de que la serie no causa a la granger a la otra.
# En todos los casos se rechaza la hip?tesis nula de que la serie no causa instantaneamente a la otra.
# O sea que esto resulta bidireccional.
# Yo tender?a a pensar que deber?a ir solamente desde extrarregional a Brasil

# Lo hago para todos los países desagregados.
var <- list()
var[[1]] <- VAR(series.diff[,-1],type="const",lag.max=15, ic="AIC")
var[[2]] <- VAR(series.diff[,-1],type="trend",lag.max=15, ic="AIC")
var[[3]] <- VAR(series.diff[,-1],type="both",lag.max=15, ic="AIC")
var[[4]] <- VAR(series.diff[,-1],type="none",lag.max=15, ic="AIC")

for(i in 1:4){
    for(varcitos in c("China","EEUU")){
        print(causality(var[[i]], cause=varcitos))
    }
}
# En todos los casos no se rechaza la hipótesis nula de que EEUU o China no causan a la granger al resto.
# En todos los casos se rechaza la hipótesis nula de que EEUU o China no causan instantaneamente al resto.


##################################################
############## Test de diagn?stico ###############


lapply(VAR,arch.test,multivariate.only=TRUE)        # Rechazo Ho en todos los casos
lapply(VAR,normality.test,multivariate.only=TRUE)   # Rechazo Ho en todos los casos
lapply(VAR,serial.test)                             # Rechazo Ho al l?mite
lapply(VAR,stability,multivariate.only=TRUE)
# O sea tengo problemas en todos los casos. Que desgracia!

# claramente lo que deber?a hacer es hacer an?lisis de intervenci?n, buscar variables omitidas
# ? tambi?n darle una forma arma(p,q) a los residuos del modelo y, de esa forma reconocer la autocorrelaci?n.


###################################################################################################
########################## Mismo análisis con las series en logaritmos ############################

# Lo puedo analizar en logaritmos, ya que, las diferencias van a ser aproximadamente tasas de crecimiento.
# Sin embargo, se debe notar que la aproximaci?n logar?tmica solo es buena cuando la variaci?n es peque?a,
# claramente no es una buena aproximaci?n para algunos saltos de la serie.

# Var entre Brasil y extrarregional
VAR.log <- list()
VAR.log[[1]] <- VAR(series.diff.log[,1:2],type="const",lag.max=15, ic="AIC")
VAR.log[[2]] <- VAR(series.diff.log[,1:2],type="trend",lag.max=15, ic="AIC")
VAR.log[[3]] <- VAR(series.diff.log[,1:2],type="both",lag.max=15, ic="AIC")
VAR.log[[4]] <- VAR(series.diff.log[,1:2],type="none",lag.max=15, ic="AIC")

for(i in 1:4){
    for(varcitos in c("Brasil","Extrarregional")){
        print(causality(VAR.log[[i]], cause=varcitos))
    }
}
# Importante!!!
# Notar que en TODOS los casos se rechazo la hip?tesis nula. Tanto para causalidad a la granger, como para
# causalidad instant?nea. Es decir, cambiaron los resultados que ten?amos previamente d?bido a la transformaci?n
# logar?tmica.

var.log <- list()
var.log[[1]] <- VAR(series.diff.log[,-1],type="const",lag.max=15, ic="AIC")
var.log[[2]] <- VAR(series.diff.log[,-1],type="trend",lag.max=15, ic="AIC")
var.log[[3]] <- VAR(series.diff.log[,-1],type="both",lag.max=15, ic="AIC")
var.log[[4]] <- VAR(series.diff.log[,-1],type="none",lag.max=15, ic="AIC")

for(i in 1:4){
  for(varcitos in c("China","EEUU")){
    print(causality(var.log[[i]], cause=varcitos))
  }
}


##################################################
############## Test de diagnóstico ###############


lapply(VAR.log,arch.test,multivariate.only=TRUE)        # Rechazo Ho en todos los casos
lapply(VAR.log,normality.test,multivariate.only=TRUE)   # Rechazo Ho en todos los casos
lapply(VAR.log,serial.test)                             # No rechazo Ho. OK.
lapply(VAR.log,stability,multivariate.only=TRUE)
# Ahora solo son problemas de normalidad. Se ve que hay heterogeneidad. Y no es de extra?ar al estar
# trabajando con series mensuales que var?an con los tipos de cambio, hay muchos quiebres.

### Para todos el var que contiene a todos los países.
lapply(var.log,arch.test,multivariate.only=TRUE)        # No rechazo en ningún caso. Ok
lapply(var.log,normality.test,multivariate.only=TRUE)   # Rechazo Ho en todos los casos
lapply(var.log,serial.test)                             # No rechazo Ho. OK.
lapply(var.log,stability,multivariate.only=TRUE)
# Ahora solo son problemas de normalidad. Se ve que hay heterogeneidad. Y no es de extra?ar al estar
# trabajando con series mensuales que var?an con los tipos de cambio, hay muchos quiebres.
# Pero mejoro drámaticamente el arch.test no rechazandose la homocedasticidad.

#Brasil y extrarregional (no se presenta en el pdf porque tiene problemas de convergencia en el fp.target)
###########
library(VARsignR)
set.seed(12345)
series3 <- series[,c("Extrarregional","Brasil")]
# Restricción de shock extrarregional (positivo) sobre Brasil (positivo)
constr <- c(+1,+2)

modelo1 <- uhlig.reject(Y=series3, nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1,
                       KMAX=6, constrained=constr, constant=FALSE, steps=60)

summary(modelo1)

irfs1 <- modelo1$IRFS

irfplot(irfdraws=irfs1, type="median", save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

fevd1 <- modelo1$FEVDS
fevdplot(fevd1, save=FALSE, bands=c(0.16, 0.84), grid=TRUE,
         bw=FALSE, table=FALSE, periods=NULL)

fevd.table <- fevdplot(fevd1, table=TRUE, periods=c(1,10,20,30,40,50,60))
print(fevd.table)

shocks <- modelo1$SHOCKS
ss <- ts(t(apply(shocks,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=12, start=c(2000,1))
plot(ss[1:dim(ss)[1],1], type="l", col="blue", ylab="Interest rate shock", ylim=c(min(ss), max(ss)))
abline(h=0, col="black")
lines(ss[1:dim(ss)[1],2], col="red")
lines(ss[1:dim(ss)[1],3], col="red")

modelo3 <- uhlig.penalty(Y=series3, nlags=12, draws=2000, subdraws=1000,
                        nkeep=1000, KMIN=1, KMAX=6, constrained=constr,
                        constant=FALSE, steps=60, penalty=100, crit=0.001)

irfs3 <- model3$IRFS
irfplot(irfdraws=irfs3, type="median", save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

fp.target(Y=series3, irfdraws=irfs1, nlags=12, constant=F, target=TRUE,
          type="median", bands=c(0.16, 0.84), save=FALSE, grid=TRUE, bw=FALSE,
          legend=TRUE, maxit=1000)
fp.target(Y=series3, irfdraws=irfs1, nlags=24, constant=F, target=TRUE,
          type="median", bands=c(0.16, 0.84), save=FALSE, grid=TRUE, bw=FALSE,
          legend=TRUE, maxit=1000)


# Todos los países. Desde EEUU hacia el resto

#Planteamiento para todas las series, con shock desde el TCRE desde EEUU hacie el resto de los paises
########### 
constr <- c(+1, +7, +8)
model1 <- uhlig.reject(Y=series2, nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1,
                       KMAX=6, constrained=constr, constant=FALSE, steps=60)

summary(model1)

irfs1 <- model1$IRFS

irfplot(irfdraws=irfs1, type="median", save=TRUE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

fevd1 <- model1$FEVDS
fevdplot(fevd1, save=TRUE, bands=c(0.16, 0.84), grid=TRUE,
         bw=FALSE, table=FALSE, periods=NULL)

fevd.table <- fevdplot(fevd1, table=TRUE, periods=c(1,12,24,36,48,60))
print(fevd.table)

shocks <- model1$SHOCKS
ss <- ts(t(apply(shocks,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=12, start=c(2000,1), end = c(2018,6))
plot(ss[,1], type="l", col="blue", ylab="Shock en TCRE con EEUU",xlab="Tiempo" ,ylim=c(min(ss), max(ss)))
abline(h=0, col="black")
lines(ss[,2], col="red")
lines(ss[,3], col="red")

model3 <- uhlig.penalty(Y=series2, nlags=12, draws=2000, subdraws=1000,
                        nkeep=1000, KMIN=1, KMAX=6, constrained=constr,
                        constant=FALSE, steps=60, penalty=100, crit=0.001)

irfs3 <- model3$IRFS
irfplot(irfdraws=irfs3, type="median", save=TRUE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

fp.target(Y=series2, irfdraws=irfs1, nlags=12, constant=F, target=TRUE,
          type="median", bands=c(0.16, 0.84), save=TRUE, grid=TRUE, bw=FALSE,
          legend=TRUE, maxit=1000)


