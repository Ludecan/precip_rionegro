##### 0 - Descarga y preparación de los datos
dt_ini <- '2017-02-01'
dt_fin <- '2021-12-31'
#estacionesADescartar <- c(
#  'ANSINA.Paso.BORRACHO.RHT', 'PASO.MAZANGANO.RHT', 'PASO.LAGUNA.I.RHT', 'PASO.AGUIAR.RHT',
#  'PASO.PEREIRA.RHT', 'PASO.NOVILLOS.RHT', 'VILLA.SORIANO.RHT')
estacionesADescartar <- NULL
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- FALSE
borrarDatosOriginales <- FALSE
plotDatos <- FALSE

source('cargaDatos.r')

valoresObservaciones[valoresObservaciones > 450] <- NA_real_

##### 1 - Correlación VS Distancia
source(paste0(pathSTInterp, 'Graficas/graficas.r'))
dist <- rdist(sp::coordinates(coordsObservaciones))
corr <- cor(valoresObservaciones, use="pairwise.complete.obs")
# Cuantas veces la estación es la menos correlacionada con otra
bajaCorr <- table(
  estaciones$NombreEstacionR[apply(corr, MARGIN = 1, FUN = which.min)]
)
sort(bajaCorr, decreasing = T)
# bajaCorr <- table(as.character(lapply(apply(corr, MARGIN = 1, FUN = which.min), FUN = names)))
corrNA <- apply(corr, MARGIN = 1, FUN = function(x) { all(is.na(x)) })
estacionesRaras <- unique(c(names(bajaCorr)[bajaCorr >= 10], names(corrNA[corrNA])))

graficoCorrVsDistancia(
  dist, corr, 
  clasesEstaciones=estaciones$redOrigen, 
  nomArchSalida='Resultados/1-Exploracion/corrVSDist_redes.png',
  tamaniosPuntos=2.5, figurasPuntos=20
)

clasesEstaciones <- rep('Ok', nrow(estaciones))
for (estacion in estacionesRaras) clasesEstaciones[which(estaciones$NombreEstacionR == estacion)] <- estacion
graficoCorrVsDistancia(
  dist, corr,
  clasesEstaciones=clasesEstaciones,
  defaultClass='Ok',
  nomArchSalida='Resultados/1-Exploracion/corrVSDist.png',
  widthPx = 3000
)


clasesEstaciones <- rep('Ok', nrow(estaciones))
clasesEstaciones[estaciones$NombreEstacionR %in% estacionesRaras] <- 'Sospechosa'
graficoCorrVsDistancia(
  dist=dist, corr=corr,
  clasesEstaciones=clasesEstaciones,
  nomArchSalida='Resultados/1-Exploracion/corrVSDist_sinOutliers.png',
  widthPx = 2500
)
iRaras <- estaciones$NombreEstacionR %in% estacionesRaras


##### 2 - Tablas estadísticas
max_run_length <- function(x, conditionFunc=function(x) { is.na(x) })  {
  enc <- rle(conditionFunc(x))
  if (any(enc$values, na.rm = T)) {
    return(max(enc$lengths[enc$values], na.rm = T))
  } else {
    return(0)
  }
}

x <- valoresObservaciones[, which(rownames(estaciones) == 'TPUENTES')] 
eliminarRachaLluviosa <- function(x, umbral=0.2, maxLargoRacha=15L) {
  enc <- rle(x >= umbral)
  iAEliminar <- which(enc$lengths > maxLargoRacha)
  # iRachaAEliminar <- iAEliminar[1]
  for (iRachaAEliminar in iAEliminar) {
    idxRacha <- seq.int(from=enc$values[iRachaAEliminar], length.out=enc$lengths[iRachaAEliminar])
    x[idxRacha] <- NA
  }
  return(x)
}

eliminarRachaSeca <- function(x, umbral=0.2, maxLargoRacha=45L) {
  enc <- rle(x < umbral)
  iAEliminar <- which(enc$lengths > maxLargoRacha)
  # iRachaAEliminar <- iAEliminar[1]
  for (iRachaAEliminar in iAEliminar) {
    idxRacha <- seq.int(from=enc$values[iRachaAEliminar], length.out=enc$lengths[iRachaAEliminar])
    x[idxRacha] <- NA
  }
  return(x)
}

umbral0 <- 0.2
x <- valoresObservaciones[, 5]
my_agg <- function(x, umbral0=0.2) {
  idx <- which(!is.na(x))
  primerNoNa <- idx[1]
  ultimoNoNa <- idx[length(idx)]
  xNoNa <- x[idx]
  
  nPeriodoTotal <- length(x)
  nPeriodoActividad <- as.integer(ultimoNoNa - primerNoNa + 1)
  nNoNa <- length(xNoNa)
  
  if (nNoNa > 0) {
    pctFaltantesPeriodoActiva <- (1 - (nNoNa / nPeriodoActividad)) * 100
    pctPrecipPeriodoActiva <- sum(xNoNa > umbral0) / nPeriodoActividad * 100
    pctCerosPeriodoActiva <- sum(xNoNa <= umbral0) / nPeriodoActividad * 100
    
    pctFaltantesPeriodoTotal <- (1 - (nNoNa / nPeriodoTotal)) * 100
    pctPrecipDatosReportados <- sum(xNoNa > umbral0) / nNoNa * 100
    pctCerosDatosReportados <- sum(xNoNa <= umbral0) / nNoNa * 100
  } else {
    pctFaltantesPeriodoActiva <- 100
    pctPrecipPeriodoActiva <- -Inf
    pctCerosPeriodoActiva <- -Inf
    
    pctFaltantesPeriodoTotal <- 100
    pctPrecipDatosReportados <- -Inf
    pctCerosDatosReportados <- -Inf
  }
  if (nNoNa >= (nPeriodoActividad * 0.8)) {
    acumulado <- sum(xNoNa) * nPeriodoActividad / nNoNa
  } else {
    acumulado <- NA_real_
  }

  promedioDiario <- mean(xNoNa)
  desviacionEstandar <- sd(xNoNa)
  maximo <- max(xNoNa)
  
  rachaLluviosa <- max_run_length(x, conditionFunc=function(x) { x >= umbral0 })
  rachaSeca <- max_run_length(x, conditionFunc=function(x) { x < umbral0 })
  
  return(
    data.frame(
      pctFaltantesPeriodoActiva=pctFaltantesPeriodoActiva, 
      pctPrecipPeriodoActiva=pctPrecipPeriodoActiva, 
      pctCerosPeriodoActiva=pctCerosPeriodoActiva, 
      pctFaltantesPeriodoTotal=pctFaltantesPeriodoTotal, 
      pctPrecipDatosReportados=pctPrecipDatosReportados,
      pctCerosDatosReportados=pctCerosDatosReportados,
      acumulado=acumulado, 
      promedioDiario=promedioDiario, 
      desviacionEstandar=desviacionEstandar, 
      maximo=maximo, 
      rachaLluviosa=rachaLluviosa, 
      rachaSeca=rachaSeca, 
      nPeriodoTotal=nPeriodoTotal,
      nPeriodoActividad=nPeriodoActividad,
      nNoNa=nNoNa
    )
  )
}

obsStats <- do.call(rbind.data.frame, apply(valoresObservaciones, MARGIN=2, FUN=my_agg))
limitesMedianaMAD <- function(x, factorMedianaMAD=3) {
  mediana <- median(x, na.rm=T)
  medianAbsDeviation <- mad(x, center=mediana, na.rm=T)
  return(c(mediana - medianAbsDeviation * factorMedianaMAD, 
           mediana + medianAbsDeviation * factorMedianaMAD))
}

t(apply(obsStats[!iRaras, 1:9], 2, limitesMedianaMAD, factorMedianaMAD=3))

limites <- as.data.frame(t(data.frame(
  pctFaltantesPeriodoActiva=c(NA, 30),
  pctPrecipPeriodoActiva=c(15, 35),
  pctCerosPeriodoActiva=c(65, 82),
  pctFaltantesPeriodoTotal=c(NA, 20),
  pctPrecipDatosReportados=c(15, 30),
  pctCerosDatosReportados=c(60, 82),
  promedioDiario=c(1.7, 4.6),
  desviacionEstandar=c(6, 15.5),
  acumulado=c(3300, 8000),
  rachaLluviosa=c(NA, 15),
  rachaSeca=c(NA, 45)
)))
colnames(limites) <- c('min', 'max')
limites$nNoCumplen <- NA

x <- obsStats
verificarLimites <- function(x, limites, iRaras) {
  x$reglas <- ''
  
  iColumna <- 7
  for (iColumna in seq.int(1, nrow(limites))) {
    columna <- rownames(limites)[iColumna]
    
    noCumplenInf <- !is.na(limites[iColumna, 1]) & (is.na(x[, columna]) | limites[iColumna, 1] > x[, columna])
    noCumplenSup <- !is.na(limites[iColumna, 2]) & (is.na(x[, columna]) | x[, columna] > limites[iColumna, 2])
    
    x$reglas[noCumplenInf] <- paste0(
      x$reglas[noCumplenInf], colnames(x)[iColumna], ' menor que ', limites[iColumna, 1], ', '
    )
    x$reglas[noCumplenSup] <- paste0(
      x$reglas[noCumplenSup], colnames(x)[iColumna], ' mayor que ', limites[iColumna, 2], ', '
    )
               
    noCumplen <- noCumplenInf | noCumplenSup
    
    iRaras <- iRaras | noCumplen
    limites$nNoCumplen[iColumna] <- sum(noCumplen)
  }
  return(list(x, limites, iRaras))
}
res <- verificarLimites(obsStats, limites, iRaras)
obsStats <- res[[1]]
limites <- res[[2]]
iRaras <- res[[3]]
rm(res)

write.table(
  obsStats, paste0(pathResultadosQC, 'obsStats.tsv'), 
  sep = '\t', na = '-99', dec = '.', row.names = T, col.names = T
)
write.table(
  limites, paste0(pathResultadosQC, 'limites.tsv'), 
  sep = '\t', na = '-99', dec = '.', row.names = T, col.names = T
)

estacionesDeReferencia <- estaciones$Nombre[!iRaras]
estacionesRaras <- estaciones$Nombre[iRaras]
iEstacionesDeReferencia <- estaciones$NombreEstacionR %in% estacionesDeReferencia
iEstacionesNoReferencia <- which(!iEstacionesDeReferencia)
iEstacionesDeReferencia <- which(iEstacionesDeReferencia)

writeLines(text=estacionesDeReferencia, con=paste0(pathResultadosQC, 'estacionesDeReferencia.txt'))


##### 3 - Ubicación Estaciones
clasesEstaciones <- rep('General', nrow(estaciones))
for (estacion in estacionesRaras) clasesEstaciones[which(estaciones$NombreEstacionR == estacion)] <- estacion

colores <- rep('#4FA9FF', nrow(estaciones))
colores[clasesEstaciones != 'General'] <- '#FFCF23'

xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase, coordsObservaciones), ejesXYLatLong = T)

distanciaAObservaciones <- gDistance(coordsObservaciones, as(coordsAInterpolar, 'SpatialPoints'), byid = T)
distanciasAObservaciones <- rowMins(distanciaAObservaciones)
mapaDistancias <- SpatialPixelsDataFrame(points=coordsAInterpolar, data=data.frame(value=distanciasAObservaciones))
sobreUy <- over(geometry(mapaDistancias), geometry(shpBase))
mapaDistancias$value[is.na(sobreUy)] <- NA  

png('Resultados/1-Exploracion/histDistanciaEstaciones.png', height=500, width=800)
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
mapearGrillaGGPlot(
  grilla=mapaDistancias, shpBase=shpBase, xyLims=xyLims, escala=escala,
  titulo='Distancias a la Observación Más Cercana [Km]',
  # dibujarPuntosObservaciones=T, coordsObservaciones=coordsObservaciones,
  subtitulo=paste0(
    'Media: ', round(mean(mapaDistancias$value, na.rm=T), 1), 
    ' Km. Máximo: ', round(max(mapaDistancias$value, na.rm=T), 1), ' Km.'),
  nomArchResultados = 'Resultados/1-Exploracion/mapaDistanciaAEstaciones.png',
  widthPx = widthPx, heightPx = heightPx, DPI = DPI
)

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
puntoMasLejano <- SpatialPoints(
  sp::coordinates(coordsAInterpolar)[iMasLejano,,drop=F], proj4string=coordsAInterpolar@proj4string)
circuloPuntoMasLejano <- gBuffer(puntoMasLejano, width = distMax, quadsegs = 32)

dfRadio <- data.frame(
  x1=sp::coordinates(puntoMasLejano)[,1], y1=sp::coordinates(puntoMasLejano)[,2],
  x2=sp::coordinates(puntoMasLejano)[,1]+distMax, y2=sp::coordinates(puntoMasLejano)[,2], value=paste(distMax, 'Km'))

#circuloPuntoMasLejano <- SpatialPolygonsDataFrame(Sr = circuloPuntoMasLejano, data = data.frame(id=1:length(circuloPuntoMasLejano)))
shpF <- fortify(circuloPuntoMasLejano, region="id")
mapaEstacionesConDistMax <- mapaEstaciones + 
  geom_path(data=shpF, mapping=aes(x=long, y=lat, group=group, z=NULL), color=rgb(25, 25, 25, maxColorValue=255), size=0.7) +
  geom_point(data=dfRadio, aes(x=x1, y=y1), colour="black", size=2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = dfRadio, arrow = arrow(length=unit(0.30,"cm")), size=0.7) +
  geom_text(data=dfRadio, aes(x=x1 + distMax / 2, y=y1 + 20, label=value), size=5, colour="black")

ggsave(mapaEstacionesConDistMax, file='Resultados/1-Exploracion/mapaEstacionesConDistMax.png', 
       dpi=DPI, width = widthPx / DPI, height = heightPx / DPI, units = 'in')

source('aplicaQC.r')

valoresObservaciones <- applyQCTests(
  coordsObservaciones, fechasObservaciones, valoresObservaciones, 
  paramsInterpolacion=paramsInterpolacionQCTests, pathsRegresores=pathsRegresores, 
  plotMaps=TRUE)

