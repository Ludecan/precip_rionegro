# Índice:
# 0 - Comentarios iniciales
# 1 - Instalación de paquetes que seguro vamos a necesitar
# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
# 3 - Lectura de rasters del satélite en formato geoTiff y definición de la grilla a interpolar
# 4 - Convierto los dataframes del paso 2 a objetos espaciales del paquete SP

pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'
pathResultados <- 'Resultados/'
pathSHPMapaBase <- paste0(pathDatos, 'CartografiaBase/uruguay_mas_cuenca_rio_negro.shp')
pathSHPSubCuencas <- paste0(pathDatos, 'CartografiaBase/SubcuencasModelo/mini_para_modelo_RioNegro.shp')

# Actualizo la definición de CRS de los objetos espaciales para ser compatible con PROJ6/GDAL3
proj4stringLatLong <- "+proj=longlat +datum=WGS84 +no_defs"
proj4stringAInterpolar <- "+proj=utm +zone=21 +south +datum=WGS84 +units=km +no_defs"
wktLatLong <- "EPSG:4326"
wktAInterpolar <- readLines(paste0(pathDatos, 'CRS/utm21s_km.wkt'))
  
# La grilla resultante tendrá factorEscaladoGrillaInterpolacion píxeles en cada dirección por cada
# pixel de la grilla de los regresores
factorEscaladoGrillaInterpolacion <- 2


# Descarga de datos de pluviometros
source('descargaDatos.r', encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Descargando datos de pluviometros del ', dt_ini, ' al ', dt_fin))
localFile <- descargaPluviosADME(
  dt_ini=dt_ini, dt_fin=dt_fin, pathSalida = paste0(pathDatos, 'pluviometros/'), 
  forzarReDescarga=forzarReDescarga)

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
source(paste0(pathSTInterp, 'instalarPaquetes/instant_pkgs.r'), encoding = 'WINDOWS-1252')
instant_pkgs(c('sp', 'gstat', 'Cairo', 'rgdal', 'devEMF', 'ncdf4'))
library(sp)
library(gstat)
library(Cairo)
library(rgdal)
library(devEMF)
library(ncdf4)

# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
source(paste0(pathSTInterp, 'SeriesTemporales/leerSeriesTemporales.r'), encoding = 'WINDOWS-1252')
datos <- leerSeriesXLSX(pathArchivoDatos = localFile, hojaDatos = 'MedidasHorarias', fileEncoding = 'UTF-8')
estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
valoresObservaciones <- datos$datos

if (!is.null(estacionesADescartar)) {
  iAConservar <- !estaciones$Nombre %in% estacionesADescartar
  estaciones <- estaciones[iAConservar,]
  valoresObservaciones <- valoresObservaciones[, iAConservar]
  rm(iAConservar)
}

# Agregacion diaria
print(paste0(Sys.time(), ' - Agregando valores diarios...'))
triHourlyUpTo <- list(PASO.MAZANGANO.RHT=ymd_hm("2019-11-06 06:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.I.RHT=ymd_hm("2019-12-03 09:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.II.RHT=ymd_hm("2019-12-03 12:00", tz = tz(fechasObservaciones[1])),
                      PASO.PEREIRA.RHT=ymd_hm("2019-12-04 15:00", tz = tz(fechasObservaciones[1])),
                      BARRA.DE.PORONGOS.RHT=ymd_hm("2019-12-10 09:00", tz = tz(fechasObservaciones[1])),
                      VILLA.SORIANO.RHT=ymd_hm("2019-12-11 12:00", tz = tz(fechasObservaciones[1])))
triHourlyUpTo <- triHourlyUpTo[names(triHourlyUpTo) %in% estaciones$Nombre]

colsToSplit <- which(sapply(colnames(valoresObservaciones), FUN = function(x) x %in% names(triHourlyUpTo)))
# x <- triHourlyUpTo[[5]]
rowsToSplit <- sapply(triHourlyUpTo, function(x, fechasObservaciones) {
  hora <- as.integer(substr(fechasObservaciones, 12, 13))
  if (!is.na(x)) { 
    return(seq_along(fechasObservaciones)[hora %% 3 == 0 & fechasObservaciones <= x])
  } else { 
    return(seq_along(fechasObservaciones)[hora %% 3])
  }
}, fechasObservaciones=fechasObservaciones)

idx <- sapply(rowsToSplit, function(x) !is.null(x))
colsToSplit <- colsToSplit[idx]
rowsToSplit <- rowsToSplit[idx]
rm(idx)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

splitAccumulated <- function(valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
  splitAccumulated_i <- function(i, valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
    # i <- colsToSplit[1]
    rowsToSplit_i <- rowsToSplit[[i]]
    rowsPerPeriod <- getmode(diff(rowsToSplit_i))
    idx_i <- colsToSplit[i]
    j <- 2
    for (j in seq_along(rowsToSplit_i)) {
      endRow <- rowsToSplit_i[j]
      if (j > 1) { startRow <- rowsToSplit_i[j - 1] + 1
      } else { startRow <- 1 }
      
      if (!is.null(rowWeights)) { rowWeightsJ <- rowWeights[j]
      } else {
        n <- endRow - startRow + 1
        rowWeightsJ <- rep(1 / rowsPerPeriod, n)
      }
      
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

rm(rowsToSplit, colsToSplit)

iStartHours <- grep(pattern = sprintf('%02d:00', horaLocalInicioAcumulacion + 1), 
                    x = rownames(valoresObservaciones), fixed = T)
iStartHour <- iStartHours[1]
if (iStartHours[length(iStartHours)] + 23 <= nrow(valoresObservaciones)) {
  iEndHour <- iStartHours[length(iStartHours)] + 23
} else {
  iEndHour <- iStartHours[length(iStartHours) - 1] + 23
}

idx <- iStartHour:iEndHour
rm(iStartHours, iStartHour, iEndHour)

fechasObservaciones <- fechasObservaciones[idx]
valoresObservaciones <- valoresObservaciones[idx, ]
clases <- (seq_along(fechasObservaciones) - 1) %/% 24L
clases <- fechasObservaciones[(clases * 24L) + 1]
clases <- parse_date_time(substr(as.character(clases), 1, 10), 
                          orders = 'Ymd', tz=tz(fechasObservaciones), truncated = 0)

# max_gap_lengths <- aggregate(valoresObservaciones, by=list(day=clases), FUN=max_run_length)
nNoNa <- aggregate(valoresObservaciones, by=list(day=clases), FUN=function(x) sum(!is.na(x)))[, -1]
valoresObservaciones <- as.matrix(aggregate(valoresObservaciones, by=list(day=clases), FUN=sum, na.rm=T)[, -1])
fechasObservaciones <- unique(clases)
row.names(valoresObservaciones) <- as.character(fechasObservaciones)

maxNHorasParaRechazarDia <- 21
iAceptados <- nNoNa > maxNHorasParaRechazarDia
valoresObservaciones[!iAceptados] <- NA
valoresObservaciones[iAceptados] <- valoresObservaciones[iAceptados] * 24 / nNoNa[iAceptados]

# Descarga de datos de satelite
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'), encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Cargando shapefile con mapa base...'))
shpBase <- cargarSHP(pathSHPMapaBase, encoding = 'CP1252')
print(paste0(Sys.time(), ' - Descargando datos de GSMaP del ', dt_ini, ' al ', dt_fin))
pathsGSMaP <- descargaGSMaP(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpBase, forzarReDescarga=forzarReDescarga, borrarDatosOriginales=borrarDatosOriginales)
print(paste0(Sys.time(), ' - Descargando datos de GPM del ', dt_ini, ' al ', dt_fin))
pathsGPM <- descargaGPM(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpBase, forzarReDescarga=forzarReDescarga, borrarDatosOriginales=borrarDatosOriginales)

# La otra parte de la función F a definir son los valores de U1, U2, ... Un.
# Esto se define en el parámetro pathsRegresores. pathsRegresores es una matriz con una columna por
# regresor y con una fila por fecha
# Una cosa a tener en cuenta y muy importante, los valores en las filas de pathsRegresores deben 
# coincidir con los valores en las filas de valoresObservaciones, 
# es decir la fila i debe corresponder a la misma fecha en pathsRegresores que en valoresObservaciones, 
# para poder matchear los valores de fechas correspondientes.
# Otra cosa útil es que la columna j tenga un nombre descriptivo del regresor j, para después poder 
# saber de cual se trata
# La función cargarRegresor(es) se encarga de esto. Para cargar un dato de satélite  se debe llamar 
# cambiando el path a la carpeta de datos en cuestión
print(paste0(Sys.time(), ' - Preparando grilla de regresores y objetos espaciales...'))
pathsRegresores <- cargarRegresores(
  carpetaRegresores = paste0(pathDatos, 'satelites'), fechasRegresando = fechasObservaciones)
pathsRegresores <- pathsRegresores[
  , apply(X = pathsRegresores, MARGIN = 2, FUN = function(x) {!all(is.na(x))}), drop=F]

# Definición de la grilla de interpolación.
# Igual a la del primer regresor con factorEscaladoGrillaInterpolacion celdas en la nueva grilla por
# cleda del satelite
grillaRegresor <- geometry(readGDAL(pathsRegresores[1, 1]))
newNCeldasX <- as.integer(round(grillaRegresor@grid@cells.dim[1] * factorEscaladoGrillaInterpolacion))
coordsAInterpolar <- grillaPixelesSobreBoundingBox(objSP = grillaRegresor, nCeldasX = newNCeldasX)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar, data.frame(rep(1, length(coordsAInterpolar)))), spTransform(shpBase, CRSobj = CRS(proj4string(coordsAInterpolar))))


# Convierto los dataframes del paso 2 a objetos espaciales del paquete SP
# Convertimos el data.frame de estaciones en un objeto espacial de tipo SpatialPointsDataFrame, es 
# un objeto espacial con geometrías tipo puntos y con una tabla de valores asociados
coordsObservaciones <- estaciones
sp::coordinates(coordsObservaciones) <- c('Longitud', 'Latitud')
class(coordsObservaciones)
# Las coordenadas de las estaciones están sin proyectar, es decir directamente en latitud/longitud. 
# Le asignamos al objeto una proyección que represente esto
proj4string(coordsObservaciones) <- CRS(projargs = proj4stringLatLong, SRS_string = wktLatLong)

# Reproyectamos las estaciones a la misma proyección que la grilla a interpolar
coordsObservaciones <- spTransform(
  x = coordsObservaciones, CRS(projargs=proj4stringAInterpolar, SRS_string = wkt(coordsAInterpolar)))
coordsObservaciones$value <- rep(NA_real_, nrow(coordsObservaciones))
iValue <- which(colnames(coordsObservaciones@data) == 'value')
coordsObservaciones@data = coordsObservaciones@data[, c(iValue, (1:ncol(coordsObservaciones@data))[-iValue])]

# TODO: esto funciona pero el encoding debería ser uno solo
if (.Platform$OS.type == "windows") {
  shpEncoding <- 'UTF-8NoBOM'
} else {
  shpEncoding <- 'WINDOWS-1252'
}

# Shapefile con el contorno del país y máscara para los píxeles de la grilla que son internos al contorno
shpMask <- cargarSHPYObtenerMascaraParaGrilla(pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, 
                                              encoding = shpEncoding)
shpBase = shpMask$shp
# Objeto auxiliar con los ejes de la grilla y el área de mapeo, para que sea igual para todos los 
# mapas independientemente de los datos que tenga
xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase), ejesXYLatLong = T)

# Grilla para QC de los satelites
grillaRegresor <- as(object = grillaRegresor, Class = 'SpatialPixels')
shpRioNegro <- shpBase[shpBase$NOMBRE == 'CuencaRioNegro', ]
if (length(shpRioNegro) != 1) { stop('cargaDatos.r: error obteniendo polígono del Río Negro') }
shpBufferRioNegro <- spTransform(gBuffer(shpRioNegro, width = 60), proj4string(grillaRegresor))
i <- !is.na(over(grillaRegresor, shpBufferRioNegro))
coordsQC <- grillaRegresor[i, ]

i <- !is.na(over(coordsAInterpolar, geometry(shpRioNegro)))
coordsAInterpolar <- coordsAInterpolar[i, ]

shpMask <- cargarSHPYObtenerMascaraParaGrilla(pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, 
                                              encoding = shpEncoding)

print(paste0(Sys.time(), ' - Cargando shapefile con subcuencas...'))
shpSubCuencas <- cargarSHP(pathSHP = pathSHPSubCuencas)
if (!identicalCRS(coordsAInterpolar, shpSubCuencas)) {
  shpSubCuencas <- spTransform(shpSubCuencas, proj4string(coordsAInterpolar))
}

getCorrs <- function(valoresObservaciones, pathsRegresores, logTransforms=TRUE) {
  valoresRegresores <- extraerValoresRegresoresSobreSP(
    objSP = coordsObservaciones, pathsRegresores = pathsRegresores)
  
  if (logTransforms) {
    valoresObservaciones <- log1p(valoresObservaciones)
    valoresRegresores <- lapply(valoresRegresores, log1p)
  }
  
  j <- 1
  i <- 7
  corrs <- sapply(1:ncol(pathsRegresores), function(j) {
    return(
      sapply(1:nrow(valoresObservaciones), FUN = function(i) {
        idx <- !is.na(valoresObservaciones[i, ]) & !is.na(valoresRegresores[[j]][i, ])
        if (any(idx)) {
          if (max(abs(valoresObservaciones[i, idx] - valoresRegresores[[j]][i, idx])) <= 1e-3) {
            return(1)
          } else {
            return(cor(valoresObservaciones[i, idx], valoresRegresores[[j]][i, idx], 
                       use = "pairwise.complete.obs"))
          }        
        } else {
          return(NA)
        }})
    )
  })
  if (!is.matrix(corrs)) {
    corrs <- matrix(corrs, nrow = nrow(pathsRegresores), ncol=ncol(pathsRegresores), 
                    dimnames=dimnames(pathsRegresores))
  } else {
    dimnames(corrs) <- dimnames(pathsRegresores)
  }
  
  return(corrs)
}

