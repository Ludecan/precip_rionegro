setwd('F:/ADME/precip_rionegro')

# Índice:
# 0 - Comentarios iniciales
# 1 - Instalación de paquetes que seguro vamos a necesitar
# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
# 3 - Lectura de rasters del satélite en formato geoTiff y definición de la grilla a interpolar
# 4 - Convierto los dataframes del paso 2 a objetos espaciales del paquete SP
# 5 - Preparación de Parámetros
# 6 - Interpolación de los datos
# 7 - Cross Validation
pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'

dt_ini='2018-10-21'
dt_fin = '2019-12-07'
horaUTCInicioAcumulacion = 10
horaLocalInicioAcumulacion = horaUTCInicioAcumulacion - 3

source('descargaDatos.r')
localFile <- descargaPluviosADME(dt_ini=dt_ini, dt_fin = dt_fin, 
                                 pathSalida = paste(pathDatos, 'pluviometros/', sep=''))
# 0 - Comentarios iniciales
# Este script sirve como plantilla base para llevar a cabo todos los pasos del pipeline de 
# interpolación.

# 1 - Instalación de paquetes que seguro vamos a necesitar
# Este fuente tiene una función instant_pkgs que busca si un paquete está instalado, si no lo está 
# lo instala y lo carga con require
# sp es un paquete para manejo de datos espaciales, tiene un montón de clases spatialXXX, en 
# particular SpatialPointsDataFrame y SpatialPixelsDataFrame que son las que venimos usando 
# gstat es el que tiene la implementación de Kriging 
# Cairo es para hacer gráficos más lindos, con antialiasing
# rgdal es una librería open source de objetos geográficos que permite leer los archivos raster 
# como los de MODIS
# Los fuentes que están en la librería hacen instant_pkgs de los paquetes que precisan así que 
# seguramente se instale alguno más
source(paste(pathSTInterp, 'instalarPaquetes/instant_pkgs.r', sep=''))
instant_pkgs(c('sp', 'gstat', 'Cairo', 'rgdal', 'devEMF', 'h5', 'ncdf4'))


# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
source(paste(pathSTInterp, 'SeriesTemporales/leerSeriesTemporales.r', sep=''))
datos <- leerSeriesXLSX(pathArchivoDatos = localFile, hojaDatos = 'MedidasHorarias', fileEncoding = 'UTF-8')
estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
valoresObservaciones <- datos$datos

triHourlyUpTo <- list(PASO.MAZANGANO.RHT=ymd_hm("2019-11-06 06:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.I.RHT=ymd_hm("2019-12-03 09:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.II.RHT=ymd_hm("2019-12-03 12:00", tz = tz(fechasObservaciones[1])),
                      PASO.PEREIRA.RHT=ymd_hm("2019-12-04 15:00", tz = tz(fechasObservaciones[1])),
                      BARRA.DE.PORONGOS.RHT=NA,
                      VILLA.SORIANO.RHT=NA)
valoresObservaciones_i <- valoresObservaciones[, 6]

colsToSplit <- which(sapply(colnames(valoresObservaciones), FUN = function(x) x %in% names(triHourlyUpTo)))
rowsToSplit <- sapply(triHourlyUpTo, function(x, fechasObservaciones) {
  if (!is.na(x)) { iDatesToConsider <- which(fechasObservaciones <= x)
  } else { iDatesToConsider <- seq_along(fechasObservaciones) }
  
  return(iDatesToConsider[seq(1, length(iDatesToConsider), 3)])
}, fechasObservaciones=fechasObservaciones)


splitAccumulated <- function(valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
  splitAccumulated_i <- function(i, valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
    # i <- colsToSplit[1]
    rowsToSplit_i <- rowsToSplit[[i]]
    idx_i <- colsToSplit[i]
    j <- 2
    for (j in seq_along(rowsToSplit_i)) {
      endRow <- rowsToSplit_i[j]
      if (j > 1) { startRow <- rowsToSplit_i[j - 1] + 1
      } else { startRow <- 1 }
      
      n <- endRow - startRow + 1
      
      if (!is.null(rowWeights)) { rowWeightsJ <- rowWeights[j]
      } else { rowWeightsJ <- rep(1 / n, n)  }
      
      valoresObservaciones[startRow:endRow, idx_i] <- valoresObservaciones[endRow, idx_i] * rowWeightsJ
    }
    
    return(valoresObservaciones[, idx_i])
  }
  
  
  valoresObservaciones[, colsToSplit] <- sapply(
    seq_along(colsToSplit), splitAccumulated_i, valoresObservaciones=valoresObservaciones, 
    colsToSplit=colsToSplit, rowsToSplit=rowsToSplit, rowWeights=rowWeights)
  return(valoresObservaciones)
}

valoresObservaciones <- splitAccumulated(
  valoresObservaciones, colsToSplit, rowsToSplit, rowWeights = NULL)


iStartHours <- grep(pattern = sprintf('%02d:00', horaLocalInicioAcumulacion + 1), 
                    x = rownames(valoresObservaciones), fixed = T)
iStartHour <- iStartHours[1]
if (iStartHours[length(iStartHours)] + 23 < nrow(valoresObservaciones)) {
  iEndHour <- iStartHours[length(iStartHours)] + 23
} else {
  iEndHour <- iStartHours[length(iStartHours) - 1] + 23
}
idx <- iStartHour:iEndHour
fechasObservaciones <- fechasObservaciones[idx]
valoresObservaciones <- valoresObservaciones[idx, ]
clases <- (seq_along(fechasObservaciones) - 1) %/% 24L
clases <- fechasObservaciones[(clases * 24L) + 1]
clases <- parse_date_time(substr(as.character(clases), 1, 10), 
                          orders = 'Ymd', tz=tz(fechasObservaciones), truncated = 0)

# max_gap_lengths <- aggregate(valoresObservaciones, by=list(day=clases), FUN=max_run_length)
nNoNa <- aggregate(valoresObservaciones, by=list(day=clases), FUN=function(x) sum(!is.na(x)))[, -1]
valoresObservaciones <- aggregate(valoresObservaciones, by=list(day=clases), FUN=sum, na.rm=T)[, -1]
fechasObservaciones <- clases

valoresObservaciones[nNoNa <= 21] <- NA
valoresObservaciones[nNoNa > 21] <- valoresObservaciones[nNoNa > 21] * 24 / nNoNa[nNoNa > 21]

max_run_length <- function(x, conditionFunc=function(x) { is.na(x) })  {
  enc <- rle(conditionFunc(x))
  if (any(enc$values, na.rm = T)) {
    return(max(enc$lengths[enc$values], na.rm = T))
  } else {
    return(0)
  }
}

max_dry_spell <- apply(valoresObservaciones, MARGIN = 2, FUN=max_run_length, conditionFunc=function(x) { x == 0 })
max_wet_spell <- apply(valoresObservaciones, MARGIN = 2, FUN=max_run_length, conditionFunc=function(x) { x > 0 })


# 3 - Lectura de rasters del satélite en formato geoTiff y definición de la grilla a interpolar
# Leemos la grilla en el archivo fname
source(paste(pathSTInterp, 'grillas/uIOGrillas.r', sep=''))
coordsAInterpolar <- leerGrillaGDAL(nombreArchivo = paste(pathDatos, 'grilla_uy.tiff', sep=''))
coordsAInterpolar <- as(geometry(coordsAInterpolar), 'SpatialPixels')

# 4 - Convierto los dataframes del paso 2 a objetos espaciales del paquete SP
source(paste(pathSTInterp, 'interpolar/interpolarEx.r', sep=''))
# Convertimos el data.frame de estaciones en un objeto espacial de tipo SpatialPointsDataFrame, es 
# un objeto espacial con geometrías tipo puntos y con una tabla de valores asociados
coordsObservaciones <- estaciones
coordinates(coordsObservaciones) <- c('Longitud', 'Latitud')
class(coordsObservaciones)
# Las coordenadas de las estaciones están sin proyectar, es decir directamente en latitud/longitud. 
# Le asignamos al objeto una proyección que represente esto
proj4string(coordsObservaciones) <- "+proj=longlat +datum=WGS84"

# Reproyectamos las estaciones a la misma proyección que la grilla a interpolar
coordsObservaciones <- spTransform(x = coordsObservaciones, CRS(proj4string(coordsAInterpolar)))


# 5 - Preparación de Parámetros
# La función que tenemos implementada para hacer la interpolación se llama interpolarYMapear en 
# interpolarYMapearEx.r
# Tiene muchos parámetros que están pensados para ser llenados por software pero la podemos 
# llamar "a mano" créandole los objetos de entrada que precisa
# universalGridding implementa la "ecuación maestra" 
# Z(s, t) = F(U1((s, t), U2(s, t), ... Un(s, t))) + Z*({Zi(s,t) - F(U1((s, t), U2(s, t), ... Un(s, t)))} s, t) + eps"(s, t)

# El método a usar en las funciones F y Z* se pasa en un objeto de parámetros en los campos
# "metodoIgualacionDistribuciones" para F e "interpolationMethod" para Z*
# Los valores de metodoIgualacionDistribuciones que venimos manejando en la tesis son:
# 1- 'ninguna'  sin ajuste de distribuciones, F(s, t) = U1(s, t), los demás regresores se ignoran
# 2- 'regresionLineal' regresión con MCO  F(s, t) = a0 + a1 * U1(s, t) + a2 * U2(s, t) + ... + an * Un(s, t) 
# 3- 'regresionLinealRobusta' regresión robusta, igual que arriba pero con RLM

# Los valores de interpolationMethod que venimos manejando en la tesis son:
# 1-'automap' Kriging. Según lo que se haya elegido en metodoIgualacionDistribuciones será KO, RK, RRK, etc.

# Estos fuentes tienen varias funciones necesarias para la interpolación. 
# Tiene las funciones para hacer kriging, universal kriging, la CV, SRT, etc...
source(paste(pathSTInterp, 'interpolar/interpolarEx.r', sep=''))
source(paste(pathSTInterp, 'interpolar/interpolarYMapearEx.r', sep=''))
source(paste(pathSTInterp, 'interpolar/funcionesAuxiliares.r', sep=''))

# Este tiene una función de ayuda que crea el objeto de parámetros para universalGridding
source(paste(pathSTInterp, 'interpolar/parsearParamsInterpolarYMapear.r', sep=''))
params <- createParamsInterpolarYMapear(baseNomArchResultados = 'ResultadosEjemplo/',
                                        proj4StringObservaciones=proj4string(coordsObservaciones),
                                        proj4StringAInterpolar=proj4string(coordsAInterpolar),
                                        coordsAInterpolarSonGrilla=TRUE, 
                                        interpolationMethod='automap',
                                        mLimitarValoresInterpolados='LimitarMinimoyMaximo',
                                        minimoLVI=0, maximoLVI=450,
                                        factorDesvEstLVI=3.5,
                                        metodoIgualacionDistribuciones='GLS',
                                        formulaRegresion='',
                                        ventanaIgualacionDistribuciones=0,
                                        usarNugget=FALSE,
                                        block=NA,
                                        inverseDistancePower=NA,
                                        umbralMascaraCeros=0.2,
                                        metodoRemocionDeSesgo='IDW_ResiduosPositivos',
                                        modelosVariograma=c("Exp", "Sph", "Pen", "Pow"),
                                        cutoff=quantile(spDists(coordsObservaciones, longlat=!is.projected(coordsObservaciones)), probs = 0.75),
                                        tlags=0:5, 
                                        nTsST=0,#max(tlags), 
                                        tlagsAR=NULL,
                                        tryFixNugget=T,
                                        nPuntosIniciales=2,
                                        modelosVariogramaST=c('Separable', 'ProductSum', 'Metric', 'SimpleSumMetric'),
                                        fit.methodST=6,
                                        verbose=FALSE,
                                        pathSHPMapaBase=paste(pathDatos, 'CartografiaBase/CuencasPrincipales.shp', sep=''),
                                        nCoresAUsar=0, 
                                        modoDiagnostico = TRUE)

# Shapefile con el contorno del país y máscara para los píxeles de la grilla que son internos al contorno
shpMask <- cargarSHPYObtenerMascaraParaGrilla(
  pathSHP=params$pathSHPMapaBase, grilla=coordsAInterpolar, encoding = 'UTF-8')
# Objeto auxiliar con los ejes de la grilla y el área de mapeo, para que sea igual para todos los 
# mapas independientemente de los datos que tenga
xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpMask$shp), ejesXYLatLong = T)

# La otra parte de la función F a definir son los valores de U1, U2, ... Un.
# Esto se define en el parámetro pathsRegresores. pathsRegresores es una matriz con una columna por
# regresor y con una fila por fecha
# Una cosa a tener en cuenta y muy importante, los valores en las filas de pathsRegresores deben 
# coincidir con los valores en las filas de tempAireMin$datos, 
# es decir la fila i debe corresponder a la misma fecha en pathsRegresores que en tempAireMin$datos, 
# para poder matchear los valores de fechas correspondientes.
# Otra cosa útil es que la columna j tenga un nombre descriptivo del regresor j, para después poder 
# saber de cual se trata

# La función cargar regresor se encarga de esto. Para cargar un dato de satélite  se debe llamar 
# cambiando el path a la carpeta de datos en cuestión
pathsRegresores <- NULL
shpBase = shpMask$shp

pathsRegresores <- descargaGSMaP(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpMask$shp)

pathsRegresores2 <- descargaGPM(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpMask$shp)

pathsRegresores <- cargarRegresores(carpetaRegresores = paste(pathDatos, 'satelites', sep=''), 
                                    fechasRegresando = fechasObservaciones)
pathsRegresores <- pathsRegresores[, apply(X = pathsRegresores, MARGIN = 2, FUN = function(x) {!all(is.na(x))}) ]


grillaRegresor <- as(object = geometry(readGDAL(pathsRegresores[1, 1])), Class = 'SpatialPixels')
shpRioNegro <- shpBase[shpBase$CUENCA == 'RÍO NEGRO', ]
shpBufferRioNegro <- spTransform(gBuffer(shpRioNegro, width = 60), proj4string(grillaRegresor))
i <- !is.na(over(grillaRegresor, shpBufferRioNegro))
coordsQC <- grillaRegresor[i, ]

i <- !is.na(over(coordsAInterpolar, geometry(shpRioNegro)))
coordsAInterpolar <- coordsAInterpolar[i, ]

# mapearPuntosGGPlot(SpatialPointsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), shpBase = shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar, data = data.frame(rep(1, length(coordsAInterpolar)))), shpBase = shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar[shpMask$mask], data = data.frame(rep(1, length(coordsAInterpolar[shpMask$mask])))), shpBase = shpBase)

# guardarSHP(as(SpatialPixelsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), 'SpatialPointsDataFrame'), 'lala.shp')
GPM = extraerValoresRegresorSobreSP(objSP = coordsQC, pathsRegresor = pathsRegresores[, 1])
GSMaP = extraerValoresRegresorSobreSP(objSP = coordsQC, pathsRegresor = pathsRegresores[, 2])

source(paste(pathSTInterp, 'qc/qcTests.r', sep=''))
test <- testEspacialPrecipitacion(
  coordsObservaciones = coordsQC, fechasObservaciones = rownames(pathsRegresores),
  valoresObservaciones = GPM, maxDistKm=11 * sqrt(2),
  ispMax=0.3, ispObs=8, 
  isdMin=1, isdObs=0.3, isdEstMin=5,
  fInf=1.5, fSup=3, amplitudMin=1)

nrow(test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,])

mapearResultadosDeteccionOutliersV2(
  test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], coordsObservaciones = coordsQC, 
  valoresObservaciones = GPM,
  tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
  carpetaSalida = 'Resultados/2-QC/mapas/GPM/', shpBase = shpBase)


test <- testEspacialPrecipitacion(
  coordsObservaciones = coordsQC, fechasObservaciones = rownames(pathsRegresores),
  valoresObservaciones = GSMaP, maxDistKm=11 * sqrt(2),
  ispMax=0.3, ispObs=8, 
  isdMin=1, isdObs=0.3, isdEstMin=5,
  fInf=1.5, fSup=3, amplitudMin=1)

nrow(test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,])

mapearResultadosDeteccionOutliersV2(
  test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], coordsObservaciones = coordsQC, 
  valoresObservaciones = GSMaP,
  tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
  carpetaSalida = 'Resultados/2-QC/mapas/GSMaP/', shpBase = shpBase)

plotObservacionesYRegresores(coordsObservaciones, fechasObservaciones, valoresObservaciones, 
                             pathsRegresoresAEvaluar = pathsRegresores, shpBase = shpBase, nColsPlots = 3, 
                             carpetaSalida = 'Resultados/1-Exploracion/mapas', 
                             grillaAlternativaRegresores=coordsAInterpolar, replot = TRUE)

# Las climatologías son un poco diferentes y por ahora las cargo "a mano". 
# Al pasarle por arriba a pathsRegresores, estoy dejando como regresor solo las climatologías, 
# dejo los dos ejemplos por si quieren probar otras cosas
#diasDelAnio <- yday(tempAireMin$fechas)
#diasDelAnio[diasDelAnio == 366] <- 365
#pathsRegresores <- matrix(paste(pathDatos, sprintf('LST_Night_Combinada_Clim_mean/%03d.tif', diasDelAnio), sep=''), ncol = 1)
#colnames(pathsRegresores) <- 'LST_Night_Combinada_Clim_mean'

# Otros valores que determinan los regresores a usar son estos 3. Para precipitación en principio
# ninguno de ellos es relevante asI que los desactivamos
params$incorporarCoordenadas <- FALSE
params$incorporarAltitud <- FALSE
params$incorporarDistanciaAlAgua <- FALSE
# Y estos determinan como se usan los de arriba, el valor por defecto es un gradiente pero con 
# otras formulas se pueden usar polinomios, etc
params$formulaCoordenadas <- 'x + y'


# Otros parámetros adicionales. listaMapas dice como se deben llamar los archivos de salida y que
# guardar. geoTiff, png con escala fija/ajustada, etc. Los valores por defecto están bien
listaMapas <- createDefaultListaMapas(paramsIyM = params, fechasObservaciones = fechasObservaciones,
                                      dibujarEscalaFija=F, salvarGeoTiff=T, recalcularSiYaExiste=F)


# Una vez que está todo armado, la función nombreModelo sirve para ver que el modelo que
# especificamos sea efectivamente el que queremos
nombreModelo(params = params, pathsRegresores = pathsRegresores)

# 6 - Interpolación de los datos
# Cambiando tsAinterpolar se puede elegir la fecha que se quiera
# tsAinterpolar <- which(fechasObservaciones == as.POSIXct('2012-01-20', tz=tz(fechasObservaciones[1])))
tsAInterpolar=1:nrow(valoresObservaciones)
interpolarYMapear(coordsObservaciones=coordsObservaciones, 
                  fechasObservaciones=fechasObservaciones, 
                  valoresObservaciones=valoresObservaciones, 
                  pathsRegresores=pathsRegresores, 
                  coordsAInterpolar=coordsAInterpolar, 
                  paramsIyM=params,
                  shpMask=shpMask,
                  xyLims=xyLims,
                  listaMapas=listaMapas,
                  returnInterpolacion=FALSE,  # Para ahorrar memoria
                  paramsParaRellenoRegresores=NULL,
                  pathsRegresoresParaRellenoRegresores=NULL,
                  espEscalaFija=NULL,
                  espEscalaAdaptada=NULL,
                  tsAInterpolar=tsAInterpolar)


# 7 - Cross Validation
# La función universalGriddingCV retorna una matriz de las mismas dimensiones que valoresObservaciones con 
# cv[i, j] el valor de la LOOCV de la estacion j en la fecha i. Es decir el valor de cv[i, j] es la estimación LOOCV de valoresObservaciones[i, j]
cv <- universalGriddingCV(coordsObservaciones=coordsObservaciones, 
                          fechasObservaciones=fechasObservaciones, 
                          valoresObservaciones=valoresObservaciones, 
                          params=params, 
                          pathsRegresores=pathsRegresores, 
                          longitudesEnColumnas=T,
                          iesAEstimar=1:ncol(valoresObservaciones),  # Esto permite hacer la cv solo para las columnas en iesAEstimar
                          eliminarSerieTemporalCompleta=TRUE,        # Esto permite elegir si se saca toda la columna de una estación para hacer la CV o solo el valor de la fecha
                          estimarNAs=FALSE)                          # Esto es una optimización donde si valoresObservaciones[i, j] es NA no se estima cv[i, j] porque igual no hay nada para comparar
