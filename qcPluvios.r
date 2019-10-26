pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'


##### 0 - Descarga y preparación de los datos

###### 0.1 - Area Interpolación
source(paste(pathSTInterp, 'grillas/uIOGrillas.r', sep=''))
proj4StringAInterpolar <- "+proj=utm +zone=21 +south +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"
coordsAInterpolar <- leerGrillaGDAL(nombreArchivo = paste(pathDatos, 'grilla_uy.tiff', sep=''))
coordsAInterpolar <- as(geometry(coordsAInterpolar), 'SpatialPixels')

###### 0.2 - Mapa Base
shpBase <- cargarSHP(paste(pathDatos, 'CartografiaBase/CuencasPrincipales.shp', sep=''))
shpBase <- spTransform(shpBase, proj4string(coordsAInterpolar))

shpBase[3,]

i <- !is.na(over(coordsAInterpolar, shpBase[3,])[,1])
coordsAInterpolar <- coordsAInterpolar[i]

###### 0.3 - Pluviometros
source('descargaDatos.r')
localFile <- descargaPluviosADME(dt_ini='2018-11-10', pathSalida = paste(pathDatos, 'pluviometros/', sep=''))

source(paste(pathSTInterp, 'SeriesTemporales/leerSeriesTemporales.r', sep=''))
datos <- leerSeriesXLSX(localFile)
estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
valoresObservaciones <- datos$datos

source(paste(pathSTInterp, 'interpolar/interpolarEx.r', sep=''))
# Convertimos el data.frame de estaciones en un objeto espacial de tipo SpatialPointsDataFrame, es 
# un objeto espacial con geometrías tipo puntos y con una tabla de valores asociados
coordsObservaciones <- estaciones
coordinates(coordsObservaciones) <- c('Longitud', 'Latitud')

# Las coordenadas de las estaciones están sin proyectar, es decir directamente en latitud/longitud. 
# Le asignamos al objeto una proyección que represente esto
proj4string(coordsObservaciones) <- "+proj=longlat +datum=WGS84"
# Reproyectamos las estaciones a la misma proyección que la grilla a interpolar
coordsObservaciones <- spTransform(x = coordsObservaciones, CRS(proj4string(coordsAInterpolar)))



##### 1 - Correlación VS Distancia
source(paste(pathSTInterp, 'Graficas/graficas.r', sep=''))
dist <- rdist(coordinates(coordsObservaciones))
corr <- cor(valoresObservaciones, use="pairwise.complete.obs")
# Cuantas veces la estación es la menos correlacionada con otra
bajaCorr <- table(as.character(lapply(apply(corr, MARGIN = 1, FUN = which.min), FUN = names)))
estacionesRaras <- names(bajaCorr)[bajaCorr >= 4]

clasesEstaciones <- rep('General', nrow(estaciones))
for (estacion in estacionesRaras)
  clasesEstaciones[which(estaciones$Nombre == estacion)] <- estacion

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
       dpi=dpi, width = widthPx / DPI, height = heightPx / DPI, units = 'in', type='cairo')



# qcTests
source(paste(pathSTInterp, 'qc/qcTests.r', sep=''))
test <- testEspacialPrecipitacion(
  coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
  valoresObservaciones = valoresObservaciones)

mapearResultadosDeteccionOutliersV2(
  test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
  tiposOutliersDeInteres = c(TTO_OutlierPorLoBajo, TTO_OutlierPorLoAlto, TTO_PrecipitacionAislada, TTO_SequedadAislada),
  carpetaSalida = 'Resultados/2-QC/mapas', shpBase = shpBase)

test[test$tipoOutlier == TTO_OutlierPorLoBajo, ]
test[test$tipoOutlier == TTO_OutlierPorLoAlto, ]
test[test$tipoOutlier == TTO_PrecipitacionAislada, ]
test[test$tipoOutlier == TTO_SequedadAislada, ]
