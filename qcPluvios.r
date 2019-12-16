##### 0 - Descarga y preparación de los datos
dt_ini='2017-09-01'
dt_ini='2018-10-21'
dt_fin = '2019-12-07'
horaUTCInicioAcumulacion = 10
horaLocalInicioAcumulacion = horaUTCInicioAcumulacion - 3

source('cargaDatos.r')

##### 1 - Correlación VS Distancia
source(paste(pathSTInterp, 'Graficas/graficas.r', sep=''))
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



##### 2 - Ubicación Estaciones
colores <- rep('red', nrow(estaciones))
colores[clasesEstaciones != 'General'] <- 'green'

xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase, coordsObservaciones), ejesXYLatLong = T)

distanciaAObservaciones <- gDistance(coordsObservaciones, as(coordsAInterpolar, 'SpatialPoints'), byid = T)
distanciasAObservaciones <- rowMins(distanciaAObservaciones)
mapaDistancias <- SpatialPixelsDataFrame(points = coordsAInterpolar, data = data.frame(value=distanciasAObservaciones))
sobreUy <- over(geometry(mapaDistancias), geometry(shpBase))
mapaDistancias$value[is.na(sobreUy)] <- NA  

png('Resultados/1-Exploracion/histDistanciaEstaciones.png', height=500, width=800, type='cairo')
tryCatch(expr = print(hist(mapaDistancias$value, main='Distribución de Distancias a la Estación Más Cercana', xlab='Distancia[Km]', 
                           ylab='Proporción [p.u.]', freq=FALSE)), finally = dev.off())
coordsObservaciones$value <- coordsObservaciones$Nombre

widthPx <- 800
heightPx <- widthPx
DPI <- 90

escala <- crearEscalaEquiespaciada(datos = mapaDistancias$value, nDigitos = 0, nIntervalos = 10, brewerPal = 'RdYlGn', continuo = T)
mapearGrillaGGPlot(grilla = mapaDistancias, shpBase = shpBase, xyLims = xyLims, escala = escala, 
                   titulo = 'Distancias a la Observación Más Cercana [Km]',
                   # dibujarPuntosObservaciones = T, coordsObservaciones = coordsObservaciones,
                   subtitulo = paste('Media: ', round(mean(mapaDistancias$value, na.rm=T), 1), ' Km. Máximo: ', 
                                     round(max(mapaDistancias$value, na.rm=T), 1), ' Km.', sep=''),
                   nomArchResultados = 'Resultados/1-Exploracion/mapaDistanciaAEstaciones.png',
                   widthPx = widthPx, heightPx = heightPx, DPI = DPI)

mapaEstaciones <- mapearPuntosConEtiquetasGGPlot(
  puntos = coordsObservaciones, shpBase = shpBase, xyLims = xyLims, coloresPuntos = colores, 
  zcol='Nombre', titulo = 'Red de Observación Disponible', tamaniosPuntos = 3, 
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


# qcTests
source(paste(pathSTInterp, 'qc/qcTests.r', sep=''))
test <- testEspacialPrecipitacion(
  coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
  valoresObservaciones = valoresObservaciones)

test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,]

mapearResultadosDeteccionOutliersV2(
  test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
  coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
  tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
  carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/', shpBase = shpBase)

test[test$tipoOutlier == TTO_OutlierPorLoBajo, ]
test[test$tipoOutlier == TTO_OutlierPorLoAlto, ]
test[test$tipoOutlier == TTO_PrecipitacionAislada, ]
test[test$tipoOutlier == TTO_SequedadAislada, ]
