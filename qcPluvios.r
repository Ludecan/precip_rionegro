##### 0 - Descarga y preparación de los datos
dt_ini <- '2017-02-01'
dt_fin <- '2020-03-01'
estacionesADescartar <- NULL
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- FALSE
borrarDatosOriginales <- FALSE
plotDatos <- FALSE

source('cargaDatos.r', encoding = 'WINDOWS-1252')

##### 1 - Tablas estadísticas
iFechas <- rownames(valoresObservaciones) >= '2017-02-01' & rownames(valoresObservaciones) <= '2020-01-31'
valoresObservaciones <- valoresObservaciones[iFechas,]

diff_meses <- function(d1, d2) {
  a1 <- as.integer(substr(d1, 1, 4))
  a2 <- as.integer(substr(d2, 1, 4))
  m1 <- as.integer(substr(d1, 6, 7))
  m2 <- as.integer(substr(d2, 6, 7))
  
  return ((a2 - a1) * 12 + (m2 - m1))
}

clases <- diff_meses(rownames(valoresObservaciones)[1], rownames(valoresObservaciones)) %/% 12

max_run_length <- function(x, conditionFunc=function(x) { is.na(x) })  {
  enc <- rle(conditionFunc(x))
  if (any(enc$values, na.rm = T)) {
    return(max(enc$lengths[enc$values], na.rm = T))
  } else {
    return(0)
  }
}

umbral0 <- 0.2
x <- valoresObservaciones[clases == 0, 1]
my_agg <- function(x, umbral0=0.2) {
  xNoNa <- na.omit(x)
  
  n <- length(x)
  nNoNa <- length(xNoNa)
  
  pctFaltantes <- (1 - (nNoNa / n)) * 100
  pctPrecip <- sum(xNoNa > umbral0) / nNoNa * 100
  if (nNoNa >= (n * 0.8)) {
    acumulado <- sum(xNoNa) * n / nNoNa
  } else {
    acumulado <-NA_real_
  }
  
  maximo <- max(xNoNa)
  
  rachaLluviosa <- max_run_length(x, conditionFunc=function(x) { x >= umbral0 })
  rachaSeca <- max_run_length(x, conditionFunc=function(x) { x < umbral0 })
  
  return(c(pctFaltantes=pctFaltantes, pctPrecip=pctPrecip, acumulado=acumulado, 
           maximo=maximo, rachaLluviosa=rachaLluviosa, rachaSeca=rachaSeca))
}

t(aggregate(x = valoresObservaciones, by=list(anio=clases), FUN=my_agg))

obsStats <- apply(valoresObservaciones, MARGIN = 2, 
                  FUN = function(x) {
                    return(aggregate(x = x, by=list(anio=clases), FUN=my_agg))
                  })

obsStatsOverall <- t(sapply(X = obsStats, FUN = function(x) { 
  return(c(colMeans(x[,2][,1:3], na.rm=T), apply(x[,2][,4:6], MARGIN = 2, FUN = max)))
}))

obsStatsOverall[, 'rachaLluviosa'] <- apply(
  valoresObservaciones, MARGIN = 2, FUN = function(x) {
    return(max_run_length(x, conditionFunc=function(x) { x >= umbral0 }))
  })
obsStatsOverall[, 'rachaSeca'] <- apply(
  valoresObservaciones, MARGIN = 2, FUN = function(x) {
    return(max_run_length(x, conditionFunc=function(x) { x < umbral0 }))
  })
obsStatsOverall[, 1:4] <- round(obsStatsOverall[, 1:4], 1)
obsStatsOverall

t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='pctFaltantes'))
t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='pctPrecip'))
t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='acumulado'))
t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='maximo'))
t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='rachaLluviosa'))
t(sapply(obsStats, function(x, columna) { x[, 2][, columna]}, columna='rachaSeca'))

iRaras <- obsStatsOverall[, 'pctFaltantes'] > 40 |
          obsStatsOverall[, 'pctPrecip'] < 23 | obsStatsOverall[, 'pctPrecip'] > 40 |
          obsStatsOverall[, 'acumulado'] < 1000 | obsStatsOverall[, 'acumulado'] > 1800 |
          obsStatsOverall[, 'maximo'] > 450 |
          obsStatsOverall[, 'rachaLluviosa'] > 15 |
          obsStatsOverall[, 'rachaSeca'] > 45

##### 2 - Correlación VS Distancia
source(paste0(pathSTInterp, 'Graficas/graficas.r'), encoding = 'WINDOWS-1252')
dist <- rdist(coordinates(coordsObservaciones))
corr <- cor(valoresObservaciones, use="pairwise.complete.obs")
# Cuantas veces la estación es la menos correlacionada con otra
bajaCorr <- table(as.character(lapply(apply(corr, MARGIN = 1, FUN = which.min), FUN = names)))
corrNA <- apply(corr, MARGIN = 1, FUN = function(x) { all(is.na(x)) })
estacionesRaras <- unique(c(names(bajaCorr)[bajaCorr >= 3], names(corrNA[corrNA])))

# estacionesRaras <- c("PUENTE.NUEVO.DURAZNO..RHT.")

clasesEstaciones <- rep('General', nrow(estaciones))
for (estacion in estacionesRaras) clasesEstaciones[which(estaciones$Nombre == estacion)] <- estacion

graficoCorrVsDistancia(dist, corr, clasesEstaciones = clasesEstaciones, 
                       nomArchSalida = 'Resultados/1-Exploracion/corrVSDist.png')

estacionesRaras


##### 3 - Ubicación Estaciones
iRaras <- iRaras | estaciones$Nombre %in% estacionesRaras
estacionesRaras <- estaciones$Nombre[iRaras]

#estacionesRaras <- c('ANSINA.Paso.BORRACHO.RHT', 'PASO.MAZANGANO.RHT', 'PASO.LAGUNA.I.RHT',
#                     'PASO.LAGUNA.II.RHT', 'PASO.AGUIAR.RHT', 'PASO.PEREIRA.RHT', 
#                     'BARRA.DE.PORONGOS.RHT', 'PASO.NOVILLOS.RHT', 'VILLA.SORIANO.RHT')
#estacionesRaras <- c('ANSINA.Paso.BORRACHO.RHT', 'PASO.MAZANGANO.RHT', 'PASO.LAGUNA.I.RHT', 
#                     'PASO.AGUIAR.RHT', 'PASO.PEREIRA.RHT', 'PASO.NOVILLOS.RHT', 'VILLA.SORIANO.RHT')

clasesEstaciones <- rep('General', nrow(estaciones))
for (estacion in estacionesRaras) clasesEstaciones[which(estaciones$Nombre == estacion)] <- estacion

colores <- rep('#4FA9FF', nrow(estaciones))
colores[clasesEstaciones != 'General'] <- '#FFCF23'

xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase, coordsObservaciones), ejesXYLatLong = T)

distanciaAObservaciones <- gDistance(coordsObservaciones, as(coordsAInterpolar, 'SpatialPoints'), byid = T)
distanciasAObservaciones <- rowMins(distanciaAObservaciones)
mapaDistancias <- SpatialPixelsDataFrame(points=coordsAInterpolar, data=data.frame(value=distanciasAObservaciones))
sobreUy <- over(geometry(mapaDistancias), geometry(shpBase))
mapaDistancias$value[is.na(sobreUy)] <- NA  

png('Resultados/1-Exploracion/histDistanciaEstaciones.png', height=500, width=800, type='cairo')
tryCatch(expr=print(
  hist(mapaDistancias$value, main='Distribución de Distancias a la Estación Más Cercana', 
       xlab='Distancia[Km]', ylab='Proporción [p.u.]', freq=FALSE)
  ), finally = dev.off())
coordsObservaciones$value <- coordsObservaciones$Nombre

widthPx <- 800
heightPx <- widthPx
DPI <- 90

coloresEscala <- c("#1A9641", "#A6D96A", "#FFFFBF", "#f46d43", "#D7191C")
escala <- crearEscala(
  escala=c(0, 10, 25, 50, ceiling(max(mapaDistancias$value))), 
  colores=coloresEscala, brewerPal='RdYlGn', continuo=T)
mapearGrillaGGPlot(grilla = mapaDistancias, shpBase = shpBase, xyLims = xyLims, escala = escala, 
                   titulo = 'Distancias a la Observación Más Cercana [Km]',
                   # dibujarPuntosObservaciones = T, coordsObservaciones = coordsObservaciones,
                   subtitulo = paste('Media: ', round(mean(mapaDistancias$value, na.rm=T), 1), ' Km. Máximo: ', 
                                     round(max(mapaDistancias$value, na.rm=T), 1), ' Km.', sep=''),
                   nomArchResultados = 'Resultados/1-Exploracion/mapaDistanciaAEstaciones.png',
                   widthPx = widthPx, heightPx = heightPx, DPI = DPI)

firstCharsToUpper <- function(
    x, wordDelimiter='[^[:alnum:]]', minLengthForUpperCase=0, noUpperFirstWords=NULL, noLowerWords=NULL) {
  wordsInX <- unlist(strsplit(x=x, split = wordDelimiter))
  
  i <- 3
  for (i in seq_along(wordsInX)) {
    word_i <- wordsInX[i]
    if (!word_i %in% noLowerWords) {
      word_i <- tolower(word_i)  
    }
    
    if (nchar(word_i) >= minLengthForUpperCase && (!word_i %in% noUpperFirstWords)) {
      wordsInX[i] <- paste0(toupper(substr(word_i, 1, 1)), substr(word_i, 2, nchar(word_i)))
    } else {
      wordsInX[i] <- word_i
    }
  }
  return(paste(wordsInX, collapse = ' '))
}


coordsObservaciones$etiqueta <- paste0(
  sapply(sub('RHT', '', coordsObservaciones$Nombre), FUN = firstCharsToUpper, 
         noUpperFirstWords=c("las", "del", "de", "los"), noLowerWords=c("I", "II"), 
         USE.NAMES = F), ' (',
  apply(valoresObservaciones, MARGIN = 2, function(x) sum(!is.na(x))), ')')
 
mapaEstaciones <- mapearPuntosConEtiquetasGGPlot(
  puntos = coordsObservaciones, shpBase = shpBase, xyLims = xyLims, coloresPuntos = colores, 
  zcol='etiqueta', titulo = 'Red de Observación Disponible', tamaniosPuntos = 3, 
  tamanioFuentePuntos = 3, nomArchResultados = 'Resultados/1-Exploracion/mapaEstaciones.png', 
  widthPx = widthPx, heightPx = heightPx, DPI = DPI)

iMasLejano <- which.max(mapaDistancias$value)
distMax <- achicarToNDigitos(mapaDistancias$value[iMasLejano], 0) 
puntoMasLejano <- SpatialPoints(coordinates(coordsAInterpolar)[iMasLejano,,drop=F], proj4string = CRS(proj4string(coordsAInterpolar)))
circuloPuntoMasLejano <- gBuffer(puntoMasLejano, width = distMax, quadsegs = 32)

dfRadio <- data.frame(x1=coordinates(puntoMasLejano)[,1], y1=coordinates(puntoMasLejano)[,2],
                      x2=coordinates(puntoMasLejano)[,1]+distMax, y2=coordinates(puntoMasLejano)[,2], value=paste(distMax, 'Km'))

#circuloPuntoMasLejano <- SpatialPolygonsDataFrame(Sr = circuloPuntoMasLejano, data = data.frame(id=1:length(circuloPuntoMasLejano)))
shpF <- fortify(circuloPuntoMasLejano, region="id")
mapaEstacionesConDistMax <- mapaEstaciones + 
  geom_path(data=shpF, mapping=aes(x=long, y=lat, group=group, z=NULL), color=rgb(25, 25, 25, maxColorValue=255), size=0.7) +
  geom_point(data=dfRadio, aes(x=x1, y=y1), colour="black", size=2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = dfRadio, arrow = arrow(length=unit(0.30,"cm")), size=0.7) +
  geom_text(data=dfRadio, aes(x=x1 + distMax / 2, y=y1 + 20, label=value), size=5, colour="black")

ggsave(mapaEstacionesConDistMax, file='Resultados/1-Exploracion/mapaEstacionesConDistMax.png', 
       dpi=DPI, width = widthPx / DPI, height = heightPx / DPI, units = 'in', type='cairo')

source('aplicaQC.r', encoding = 'WINDOWS-1252')

valoresObservaciones <- applyQCTests(
  coordsObservaciones, fechasObservaciones, valoresObservaciones, 
  paramsInterpolacion = paramsInterpolacionQCTests, pathsRegresores = pathsRegresores, 
  plotMaps = TRUE)




