##### 0 - Descarga y preparación de los datos
dt_ini <- '2017-02-01'
dt_fin <- '2020-03-01'
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- FALSE
borrarDatosOriginales <- FALSE
plotDatos <- FALSE

source('cargaDatos.r', encoding = 'WINDOWS-1252')

##### 1 - Correlación VS Distancia
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

##### 2 - Ubicación Estaciones
colores <- rep('red', nrow(estaciones))
colores[clasesEstaciones != 'General'] <- 'green'

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
