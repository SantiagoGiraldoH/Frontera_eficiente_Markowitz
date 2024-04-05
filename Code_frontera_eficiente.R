#primera parte
datos <- read.csv("Datos ejercicio Teoria de portafolios.csv", sep = ";", dec= ",", header=T)
precios <- datos[, 2:5]
precios <- ts(precios)

COLCAP <- datos[,6]
COLCAP <- ts(COLCAP)

nombres<- colnames(precios)
numerop<- nrow(precios)

#Rendimientos y Volatilidades 
rendimientos <- diff(log(precios))
rendimientos_mercado<- diff(log(COLCAP))

rendimientos_esperados <- apply(rendimientos, 2, mean)
rendimientos_esperados_mercado <- mean(rendimientos_mercado)

rendimientos_esperados
rendimientos_esperados_mercado

volatilidades <- apply(rendimientos, 2, sd)
volatilidad_mercado <- sd(rendimientos_mercado)

volatilidades
volatilidad_mercado

#Grafico de cada accion
plot(volatilidades, rendimientos_esperados, pch = 19, cex=2, xlab="Volatilidad", ylab="Rendimiento", col=c(1:5), ylim = c(-0.004, 0.012))
points(volatilidad_mercado, rendimientos_esperados_mercado, pch=19, cex=2, col=5)
legend("topleft", c(nombres, "COLCAP"), pch =19, bty = "n", cex=1.5, col=c(1:5))

#Segunda parte

#grafico de la forntera eficiente
library(fPortfolio)
frontera = portfolioFrontier(as.timeSeries(rendimientos), constraints = "longOnly")

frontierPlot(frontera, cex=2, pch=19)
monteCarloPoints(frontera, col="blue", mcSteps = 500, cex= 0.5, pch=19)
minvariancePoints(frontera, col="darkred", pch=19, cex=2)
equalWeightsPoints(frontera, col="darkgreen", pch=19, cex=2)

#Graficos de proporciones de los portafolios de la frontera

weightsPlot(frontera)
colores <- qualiPalette(ncol(rendimientos),"Dark2")
weightsPlot(frontera, col=colores)

#proporciones portafolio de minima varianza
minima_varianza <- minvariancePortfolio(as.timeSeries(rendimientos), constraints= "longOnly")
portafolio_minima_varianza<- c(0.1675, 0.3309, 0.4975, 0.0041)
#rendimeinto esperado 
rendimiento_minima_varianza <- sum(portafolio_minima_varianza*rendimientos_esperados)
rendimiento_minima_varianza

covarianzas <- cov(rendimientos)
volatilidad_minima_varianza <- sqrt(sum(portafolio_minima_varianza%*%covarianzas*t(portafolio_minima_varianza)))
#indicador de diversificacion portafolio de minima varianza
diversifica_portafolio<- 1 - volatilidad_minima_varianza/sum(volatilidades[1:4])
diversifica_portafolio

#grafico de la frontera eficiente con CML
rf <- 0.055
rf <- log(1+rf)
rf <- log(1+rf/12)


especificaciones <- portfolioSpec()
`setRiskFreeRate<-`(especificaciones, rf)
frontera <- portfolioFrontier(as.timeSeries(rendimientos), constraints="longOnly", spec= especificaciones)

frontierPlot(frontera, cex=2, pch=19)
monteCarloPoints(frontera, col="blue", mcSteps = 500, cex= 0.5, pch=19)
minvariancePoints(frontera, col="darkred", pch=19, cex=2)
equalWeightsPoints(frontera, col="darkgreen", pch=19, cex=2)
tangencyLines(frontera)
tangencyPoints(frontera, col="green", cex=2, pch=19)

#proporciones portafolio tangente
portafolio_tangente <- c(0.8253, 0.1747, 0, 0)

#rendimiento esperado mensual portafolio tangente
rendimiento_esperado_portafolioTangente<- sum(portafolio_tangente*rendimientos_esperados)
rendimiento_esperado_portafolioTangente
#volatilidad mensual portafolio tangente
volatilidad_portafolioTangente <- sqrt(sum(portafolio_tangente%*%covarianzas*t(portafolio_tangente)))
volatilidad_portafolioTangente

#indicador de divercificación portafolio tangente
diversifica_portafolioTangente<- 1 - volatilidad_portafolioTangente/sum(volatilidades[1:2])
diversifica_portafolioTangente

#tercera parte
rendimientos2 <- diff(precios)/precios[-numerop,]
rendimientos_mercado2 <- diff(COLCAP)/COLCAP[-numerop]
nombres<- colnames(rendimientos2)

#beta de cada accion 
regresion <- lm(rendimientos2 ~ rendimientos_mercado2)
beta <- regresion$coefficients[2,]

beta
#correlacion e cada accion con el mercado
correlacion_ca <- cor(rendimientos2, rendimientos_mercado2)
correlacion_ca

#rendimientos esperados de cada accion anualizados
rendimientos_anual<- rendimientos_esperados*12
rendimientos_anual

#volatilidad de cada accion anualizadas
volatilidades_anual <- volatilidades*sqrt(12)
volatilidades_anual

#rendimientos esperado del mercado anualizado
rendimientos_esperados_mercado_anual<- rendimientos_esperados_mercado*12
rendimientos_esperados_mercado_anual

#beta portafolio minima varianza
beta_minimavar <- sum(portafolio_minima_varianza*beta)
beta_minimavar

#CAPM anualizado portafolio de minima varianza
rf <- 0.055
rf <- log(1+rf)
CAPM_minvar <- rf + beta_minimavar*(rendimientos_esperados_mercado_anual-rf)
CAPM_minvar

#Beta portafolio tangente
beta_tangente <- sum(portafolio_tangente*beta)
beta_tangente

#CAPM anualizado portafolio tangente
CAPM_tangente <- rf + beta_tangente*(rendimientos_esperados_mercado_anual-rf)
CAPM_tangente

#cuarta parte 
# indicadores de desempeño anualizado portafolio de minima varianza
#Sharpe
sharpeminvar <- (rendimiento_minima_varianza*12 - rf)/(volatilidad_minima_varianza*sqrt(12))
sharpeminvar
#Treynor
treynorminvar <- (rendimiento_minima_varianza*12-rf)/beta_minimavar
treynorminvar
#Jensen
jensenminvar<- (rendimiento_minima_varianza*12-rf)-(rendimientos_esperados_mercado_anual- rf)*beta_minimavar
jensenminvar

# indicadores de desempeño anualizado portafolio tangente
#Sharpe
sharpetangente <- (rendimiento_esperado_portafolioTangente*12 - rf)/(volatilidad_portafolioTangente*sqrt(12))
sharpetangente
#Treynor
treynortangente <- (rendimiento_esperado_portafolioTangente*12-rf)/beta_tangente
treynortangente
#Jensen
jensentangente<- (rendimiento_esperado_portafolioTangente*12-rf)-(rendimientos_esperados_mercado_anual- rf)*beta_tangente
jensentangente

#quinta parte: portafolio especifico

proporciones<- c(0.4,0.3,0.1,0.2)

#rendimiento esperado anualizado portafolio especifico
rendimientos_portafolio <- vector()
for (i in 1:nrow(rendimientos)){
        rendimientos_portafolio[i]= sum(rendimientos[i,]*proporciones)
}
rendimiento_esperado_portafolio <- mean(rendimientos_portafolio*12)

rendimiento_esperado_portafolio

#volatilidad anualizada portafolio especifico

volatilidad_portafolio <- sd(rendimientos_portafolio)
volatilidad_portafolio <- volatilidad_portafolio*sqrt(12)

volatilidad_portafolio

#beta portafolio 
beta_portafolio <- sum(proporciones*beta)
beta_portafolio

#CAPM portafolio especifico 
rf <- 0.055
rf <- log(1+rf)
CAPM_portafolio <- rf + beta_portafolio*(rendimientos_esperados_mercado_anual - rf)
CAPM_portafolio

#indicadores de desempeño anualizado portafolio especifico 
#Sharpe

sharpeport <- (rendimiento_esperado_portafolio - rf)/(volatilidad_portafolio)
sharpeport
#Treynor
treynorport <- (rendimiento_esperado_portafolio-rf)/beta_portafolio
treynorport
#Jensen
jensenport<- (rendimiento_esperado_portafolio-rf)-(rendimientos_esperados_mercado_anual- rf)*beta_portafolio
jensenport

#grafico de las acciones, del mercado, portafolio minima varianza, portafolio tangente y portafolio especifico

plot(volatilidades_anual, rendimientos_anual, pch = 19, cex=2, xlab="Volatilidad", ylab="Rendimiento", col=c(1:5), ylim = c(-0.05, 0.15), xlim=c(0.12,0.28))
points(volatilidad_mercado*sqrt(12), rendimientos_esperados_mercado_anual, pch=19, cex=2, col=5)
points(volatilidad_minima_varianza*sqrt(12), rendimiento_minima_varianza*12, pch=19, cex=2, col=6)
points(volatilidad_portafolioTangente, rendimiento_esperado_portafolioTangente, pch=19, cex=2, col=7)
points(volatilidad_portafolio, rendimiento_esperado_portafolio, pch=19, cex=2, col=8)
legend("topleft", c(nombres, "COLCAP", "Min. Var.", "Tangente", "Especifico"), pch =19, bty = "n", cex=1.5, col=c(1:8))