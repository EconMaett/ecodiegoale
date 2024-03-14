# 09 - Output Gap Estimation in R ----
# GitHub: https://github.com/aun-antonio/Output-Gap-Estimation-in-R
library(neverhpfilter)
library(x12)
library(x12GUI)
library(x13binary)
library(seasonal)
library(trend)
library(Kendall)
library(tseries)
library(xlsx)
library(readxl)

#Objetivos:

#1) Prueba Filtro H-P con correccion de Hamilton (neverhpfilter)
#2) Prueba Filtro clasico H-P luego de estimar Arima con metodo X-12-ARIMA (x12)

setwd("C:/Users/Rodrigo_diaz/Desktop/outputgap/data")

#Indicador mensual de actividad económica, Imacec,  volumen a precios del año anterior encadenado, series empalmadas, (promedio 2013=100)
imacec <- read_excel("C:/Users/Rodrigo_diaz/Desktop/outputgap/Data/imacec.xlsx")



#DEFINIR SERIE: 

base1<-imacec
IMACEC<-ts(base1$Imacec,start=c(1996,1),end=c(2019,1),frequency=12)
serie<-ts(base1,start=c(1996,1),end=c(2019,1),frequency=12)


##############################################ANALISIS GRAFICO############################################

#Grafico imacec
plot(IMACEC,main="IMACEC , index level 2013=10") 

#grafico imacec por componentes
plot(serie,main="IMACEC DESAGREGADO , index level 2013=10")  #mas de lejos


#Grafico IMACEC: descomposicion aditiva  Y=T+E+R
serie_desc <- decompose(serie[,2])
plot(serie_desc)

#Visualmente se puede identificar una serie con tendencia y estacionalidad (No estacionaria)

##############################################Tests############################################

#Test estacionariedad
kpss.test(IMACEC) #H0: Estacionariedad de tendencia ..
#Test tendencia
cs.test(IMACEC) #H0: Serie no presenta tendencia...
#Augmented Dickey-Fuller test. H0:Existe raíz unitaria de una serie de tiempo univariada
adf.test(IMACEC) #  No rechazo.  Existe en la FAC de la serie un comportamiento de caminata aleatoria


##############################################Diferenciando ############################################
#Diferenciamos la serie para hacerla estacionaria (1-DIFF) 

Imacec_diff <- diff(IMACEC, 12)
ts.plot(Imacec_diff,main="Serie IMACEC diferenciada")

#Test estacionariedad serie diferenciada
kpss.test(Imacec_diff) #H0: Estacionariedad de tendencia. 


acf(Imacec_diff,lag.max=60,main="FAC serie diff")
pacf(Imacec_diff,lag.max=60,main="PFAC Imacec Diff")

#2-DIFF

Imacec_diff2 <- diff(Imacec_diff)
ts.plot(Imacec_diff2,main="Serie IMACEC diferenciada 2")

#Test estacionariedad serie diferenciada
kpss.test(Imacec_diff2) #H0: Estacionariedad de tendencia. Se se comporta como un proceso estacionario. p-value = 0.1

#Análisis ACF y PACF
acf(IMACEC,lag.max=100,main="FAC serie")
pacf(IMACEC,lag.max=100,main="PFAC serie Imacec")
acf(Imacec_diff2,lag.max=60,main="ACF serie diff")
pacf(Imacec_diff2,lag.max=60,main="PACF Imacec Diff")



#################################################Modelos candidatos##################################################
#SARIMA
modelo5<-arima(IMACEC, order=c(1,1,2), seasonal=list(order=c(0,1,0)))  
## AIC=794************************************************************
#Desarima*****
modelo4<-arima(IMACEC, order=c(1,0,3), seasonal=list(order=c(0,1,1))) # resultados comprobados con "Intervalos de confianza con probabilidad de cobertura (0.95000)  en la escala original en X13ARIMAseats
----------------
  #arima recomendado
  modelo<-arima(IMACEC, order=c(2,1,0), seasonal=list(order=c(0,1,1)))


#Forecast utilizando modelo 4: TRAIN VS TEST
Imacec4_train<-ts(IMACEC[1:228],start=c(1996,1),end=c(2014,12),frequency=12)
modelo4_train<-arima(Imacec4_train, order=c(1,0,3), seasonal=list(order=c(0,1,1)))

b<-IMACEC
pred4<-predict(modelo4_train, n.ahead =72)
fit4<-modelo4_train$residuals+b
plot.ts(b,lw=1.5, col=1,xlim=c(2005,2020), ylim=c(20,150),main="Predicción arima fuera de muestra")
lines(fit4,col='red')
Pred4 = pred4$pred
Sd4  <- pred4$se
lines(Pred4, col="orange",lwd=1.5)
lines(Pred4+1.96*Sd4, lty="solid", col=5,lwd=1)   
lines(Pred4-1.96*Sd4, lty="solid", col=5,lwd=1)


# NECESITO PREDECIR PERIODOS HACIA ATRAS (BACKASTING) PARA PODER APLICAR FILTRO:
#PREDICCIÓN a 60 periodos hacia atras utilizando total data (modelo4) BACKASTING


library(forecast)
x <- IMACEC
h <- 60
f <- frequency(x)
# Reverse time
revx <- ts(rev(x), frequency=f)
# Forecast
fore_back <- forecast(auto.arima(revx), h)
plot(fore_back)

fore_back$mean <- ts(rev(fore_back$mean),end=tsp(x)[1] - 1/f, frequency=f)
fore_back$upper <- fore_back$upper[h:1,]
fore_back$lower <- fore_back$lower[h:1,]
fore_back$x <- x
# Plot result
plot(fore_back, xlim=c(1990,2025),ylim=c(20,150), tsp(x)[2])
lines(fit4,col='red')
lines(Pred4, col="blue4",lwd=2)
lines(Pred4+1.96*Sd4, lty="dashed", col=8,lwd=2)   
lines(Pred4-1.96*Sd4, lty="dashed", col=8,lwd=2)

#Juntar -60 + base imacec + 60
#Prediccion a 60 meses:
mod4_60<-Pred4
View(mod4_60)
#Prediccion a -60 meses:
modelo4_back<-arima(revx, order=c(1,0,3), seasonal=list(order=c(0,1,1)))
Pred4_back<-predict(modelo4_back, n.ahead =60)
mod4_60back<-Pred4_back$pred
mod4_60back<-rev(mod4_60back)
mod4_60back

Ima_tot<-append(mod4_60back,IMACEC)
Ima_tot<-append(Ima_tot,mod4_60)
View(Ima_tot)

IMACEC_estimation <-ts(Ima_tot,start=c(1991,1),end=c(2024,1),frequency=12)
View(IMACEC_estimation)


write.xlsx(IMACEC_estimation, 'IMACEC_EST.xlsx', sheetName="Sheet1")
#Agregar fecha en excel a IMACEC_estimation

###############################################################################

#Aplicar Filtro
library (mFilter)


#######Desestacionalizar manualmente para aplicar filtro


library(seasonal)

serie.ajuste <-seas(IMACEC_estimation, xreg = NULL, xtrans = NULL, seats.noadmiss = "yes", transform.function = "auto", regression.aictest = c("td", "easter"),
                    outlier = "")
season_adj<-serie.ajuste$data[,4]


#APLICO FILTRO
IMACEC_HP<-hpfilter(season_adj, freq =114600)
Y_hp129<-IMACEC_HP$trend


#PLOTEO "IMACEC_DES" -TREND DE FILTRO IMAEC_DES
IMACECdes<-seas(IMACEC, xreg = NULL, xtrans = NULL, seats.noadmiss = "yes", transform.function = "auto", regression.aictest = c("td", "easter"),
                outlier = "")
IMACECdes<-IMACECdes$data
IMACECdes <- IMACECdes[,4]

#PLOT 
plot(Y_hp129, xlim=c(1996,2018))
lines(IMACECdes, lty="dashed", col=3,lwd=2)

IMACEC_filtro<-Y_hp129[61:337,]
outputgap129<-IMACECdes-IMACEC_filtro
plot(outputgap129, xlab="Fecha", ylab="IMACEC", xlim=c(2000,2017), ylim=c(-8,14),col=3,lw=5, main="II. Actividad")
lines(Y_hp129*0, lty="solid", col="black",lwd=3) 

### exportamos con el nombre 
write.xlsx(outputgap129, 'outputgap.xlsx', row.names=F, sheetName="Sheet1")
