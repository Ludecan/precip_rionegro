# �ndice:
# 0 - Comentarios iniciales
# 1 - Instalaci�n de paquetes que seguro vamos a necesitar
# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
# 3 - Lectura de rasters del sat�lite en formato geoTiff y definici�n de la grilla a interpolar
# 4 - Convierto los dataframes del paso 2 a objetos espaciales del paquete SP

pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'
pathResultados <- 'Resultados/'
pathSHPMapaBase=paste(pathDatos, 'CartografiaBase/CuencasPrincipales.shp', sep='')
proj4stringAInterpolar <- "+proj=utm +zone=21 +south +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"
# La grilla resultante tendr� factorEscaladoGrillaInterpolacion p�xeles en cada direcci�n por cada
# pixel de la grilla de los regresores
factorEscaladoGrillaInterpolacion <- 2


# Descarga de datos de pluviometros
source('descargaDatos.r', encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Descargando datos de pluviometros del ', dt_ini, ' al ', dt_fin))
localFile <- descargaPluviosADME(
  dt_ini=dt_ini, dt_fin=dt_fin, pathSalida = paste(pathDatos, 'pluviometros/', sep=''), 
  forzarReDescarga=forzarReDescarga)

# 1 - Instalaci�n de paquetes que seguro vamos a necesitar
# Este fuente tiene una funci�n instant_pkgs que busca si un paquete est� instalado, si no lo est� 
# lo instala y lo carga con require
# sp es un paquete para manejo de datos espaciales, tiene un mont�n de clases spatialXXX, en 
# particular SpatialPointsDataFrame y SpatialPixelsDataFrame que son las que venimos usando 
# gstat es el que tiene la implementaci�n de Kriging 
# Cairo es para hacer gr�ficos m�s lindos, con antialiasing
# rgdal es una librer�a open source de objetos geogr�ficos que permite leer los archivos raster 
# como los de MODIS
# Los fuentes que est�n en la librer�a hacen instant_pkgs de los paquetes que precisan as� que 
# seguramente se instale alguno m�s
source(paste0(pathSTInterp, 'instalarPaquetes/instant_pkgs.r'), encoding = 'WINDOWS-1252')
instant_pkgs(c('sp', 'gstat', 'Cairo', 'rgdal', 'devEMF', 'ncdf4'))

# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
source(paste0(pathSTInterp, 'SeriesTemporales/leerSeriesTemporales.r'), encoding = 'WINDOWS-1252')
datos <- leerSeriesXLSX(pathArchivoDatos = localFile, hojaDatos = 'MedidasHorarias', fileEncoding = 'UTF-8')
estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
valoresObservaciones <- datos$datos

# Agregacion diaria
print(paste0(Sys.time(), ' - Agregando valores diarios...'))
triHourlyUpTo <- list(PASO.MAZANGANO.RHT=ymd_hm("2019-11-06 06:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.I.RHT=ymd_hm("2019-12-03 09:00", tz = tz(fechasObservaciones[1])),
                      PASO.LAGUNA.II.RHT=ymd_hm("2019-12-03 12:00", tz = tz(fechasObservaciones[1])),
                      PASO.PEREIRA.RHT=ymd_hm("2019-12-04 15:00", tz = tz(fechasObservaciones[1])),
                      BARRA.DE.PORONGOS.RHT=ymd_hm("2019-12-10 09:00", tz = tz(fechasObservaciones[1])),
                      VILLA.SORIANO.RHT=ymd_hm("2019-12-11 12:00", tz = tz(fechasObservaciones[1])))

colsToSplit <- which(sapply(colnames(valoresObservaciones), FUN = function(x) x %in% names(triHourlyUpTo)))
x <- triHourlyUpTo[[2]]
rowsToSplit <- sapply(triHourlyUpTo, function(x, fechasObservaciones) {
  if (!is.na(x)) { iDatesToConsider <- which(fechasObservaciones <= x)
  } else { iDatesToConsider <- seq_along(fechasObservaciones) }
  
  if (length(iDatesToConsider) > 0) {
    res <- iDatesToConsider[seq(from = 2, to = length(iDatesToConsider), by = 3)]
    if (fechasObservaciones[res[length(res)]] != x) stop('Error en la desagregaci�n de valores trihorarios')
    return(res)
  } else {
    return(NULL)
  }
}, fechasObservaciones=fechasObservaciones)


idx <- sapply(rowsToSplit, function(x) !is.null(x))
colsToSplit <- colsToSplit[idx]
rowsToSplit <- rowsToSplit[idx]

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

iStartHours <- grep(pattern = sprintf('%02d:00', horaLocalInicioAcumulacion + 1), 
                    x = rownames(valoresObservaciones), fixed = T)
iStartHour <- iStartHours[1]
if (iStartHours[length(iStartHours)] + 23 <= nrow(valoresObservaciones)) {
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
valoresObservaciones <- as.matrix(aggregate(valoresObservaciones, by=list(day=clases), FUN=sum, na.rm=T)[, -1])
fechasObservaciones <- unique(clases)
row.names(valoresObservaciones) <- as.character(fechasObservaciones)

maxNHorasParaRechazarDia <- 21
iAceptados <- nNoNa > maxNHorasParaRechazarDia
valoresObservaciones[!iAceptados] <- NA
valoresObservaciones[iAceptados] <- valoresObservaciones[iAceptados] * 24 / nNoNa[iAceptados]

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


# Descarga de datos de satelite
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'), encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Cargando shapefile con mapa base...'))
shpBase <- cargarSHP(pathSHPMapaBase, encoding = 'CP1252')
print(paste0(Sys.time(), ' - Descargando datos de GSMaP del ', dt_ini, ' al ', dt_fin))
pathsGSMaP <- descargaGSMaP(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpBase, forzarReDescarga=forzarReDescarga)
print(paste0(Sys.time(), ' - Descargando datos de GPM del ', dt_ini, ' al ', dt_fin))
pathsGPM <- descargaGPM(
  dt_ini = dt_ini, dt_fin = dt_fin, horaUTCInicioAcumulacion = horaUTCInicioAcumulacion, 
  shpBase = shpBase, forzarReDescarga=forzarReDescarga)

# La otra parte de la funci�n F a definir son los valores de U1, U2, ... Un.
# Esto se define en el par�metro pathsRegresores. pathsRegresores es una matriz con una columna por
# regresor y con una fila por fecha
# Una cosa a tener en cuenta y muy importante, los valores en las filas de pathsRegresores deben 
# coincidir con los valores en las filas de valoresObservaciones, 
# es decir la fila i debe corresponder a la misma fecha en pathsRegresores que en valoresObservaciones, 
# para poder matchear los valores de fechas correspondientes.
# Otra cosa �til es que la columna j tenga un nombre descriptivo del regresor j, para despu�s poder 
# saber de cual se trata
# La funci�n cargarRegresor(es) se encarga de esto. Para cargar un dato de sat�lite  se debe llamar 
# cambiando el path a la carpeta de datos en cuesti�n
print(paste0(Sys.time(), ' - Preparando grilla de regresores y objetos espaciales...'))
pathsRegresores <- cargarRegresores(carpetaRegresores = paste(pathDatos, 'satelites', sep=''), 
                                    fechasRegresando = fechasObservaciones)
pathsRegresores <- pathsRegresores[, apply(X = pathsRegresores, MARGIN = 2, FUN = function(x) {!all(is.na(x))}), drop=F]

# Definici�n de la grilla de interpolaci�n.
# Igual a la del primer regresor con factorEscaladoGrillaInterpolacion celdas en la nueva grilla por
# cleda del satelite
grillaRegresor <- geometry(readGDAL(pathsRegresores[1, 1]))
newNCeldasX <- as.integer(round(grillaRegresor@grid@cells.dim[1] * factorEscaladoGrillaInterpolacion))
coordsAInterpolar <- grillaPixelesSobreBoundingBox(objSP = grillaRegresor, p4string = proj4stringAInterpolar, nCeldasX = newNCeldasX)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar, data.frame(rep(1, length(coordsAInterpolar)))), spTransform(shpBase, CRSobj = CRS(proj4string(coordsAInterpolar))))


# Convierto los dataframes del paso 2 a objetos espaciales del paquete SP
# Convertimos el data.frame de estaciones en un objeto espacial de tipo SpatialPointsDataFrame, es 
# un objeto espacial con geometr�as tipo puntos y con una tabla de valores asociados
coordsObservaciones <- estaciones
coordinates(coordsObservaciones) <- c('Longitud', 'Latitud')
class(coordsObservaciones)
# Las coordenadas de las estaciones est�n sin proyectar, es decir directamente en latitud/longitud. 
# Le asignamos al objeto una proyecci�n que represente esto
proj4string(coordsObservaciones) <- "+proj=longlat +datum=WGS84"

# Reproyectamos las estaciones a la misma proyecci�n que la grilla a interpolar
coordsObservaciones <- spTransform(x = coordsObservaciones, CRS(proj4string(coordsAInterpolar)))
coordsObservaciones$value <- rep(NA_real_, nrow(coordsObservaciones))
iValue <- which(colnames(coordsObservaciones@data) == 'value')
coordsObservaciones@data = coordsObservaciones@data[, c(iValue, (1:ncol(coordsObservaciones@data))[-iValue])]

# Shapefile con el contorno del pa�s y m�scara para los p�xeles de la grilla que son internos al contorno
shpMask <- cargarSHPYObtenerMascaraParaGrilla(pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, 
                                              encoding = 'UTF-8NoBOM')
shpBase = shpMask$shp
# Objeto auxiliar con los ejes de la grilla y el �rea de mapeo, para que sea igual para todos los 
# mapas independientemente de los datos que tenga
xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase), ejesXYLatLong = T)

# Grilla para QC de los satelites
grillaRegresor <- as(object = grillaRegresor, Class = 'SpatialPixels')
shpRioNegro <- shpBase[shpBase$CUENCA == 'R�O NEGRO', ]
if (length(shpRioNegro) != 1) { stop('cargaDatos.r: error obtaining R�o Negro polygon') }
shpBufferRioNegro <- spTransform(gBuffer(shpRioNegro, width = 60), proj4string(grillaRegresor))
i <- !is.na(over(grillaRegresor, shpBufferRioNegro))
coordsQC <- grillaRegresor[i, ]

i <- !is.na(over(coordsAInterpolar, geometry(shpRioNegro)))
coordsAInterpolar <- coordsAInterpolar[i, ]

shpMask <- cargarSHPYObtenerMascaraParaGrilla(pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, 
                                              encoding = 'UTF-8NoBOM')

getCorrs <- function(valoresObservaciones, pathsRegresores, logTransforms=TRUE) {
  valoresRegresores <- extraerValoresRegresoresSobreSP(coordsObservaciones, pathsRegresores = pathsRegresores)
  
  if (logTransforms) {
    valoresObservaciones <- log1p(valoresObservaciones)
    valoresRegresores <- lapply(valoresRegresores, log1p)
  }
  
  j <- 1
  i <- 1
  corrs <- sapply(1:ncol(pathsRegresores), function(j) {
    return(
      sapply(1:nrow(valoresObservaciones), FUN = function(i) {
        idx <- !is.na(valoresObservaciones[i, ]) & !is.na(valoresRegresores[[j]][i, ])
        if (any(idx)) {
          if (max(abs(valoresObservaciones[i, idx] - valoresRegresores[[j]][i, idx])) <= 1e-3) {
            return(1)
          } else {
            return(cor(valoresObservaciones[i, idx], valoresRegresores[[j]][i, idx], use = "pairwise.complete.obs"))
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
