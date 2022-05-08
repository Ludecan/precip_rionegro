# �ndice:
# 0 - Comentarios iniciales
# 1 - Instalaci�n de paquetes que seguro vamos a necesitar
# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
# 3 - Lectura de rasters del sat�lite en formato geoTiff y definici�n de la grilla a interpolar
# 4 - Convierto los dataframes del paso 2 a objetos espaciales del paquete SP

if (file.exists('.env')) {
  # Leo variables de entorno del archivo
  envvars <- strsplit(readLines('.env'), split='=')
  envvars <- setNames(lapply(envvars, function(x) x[2]), sapply(envvars, function(x) x[1]))
  do.call(Sys.setenv, envvars)
  Sys.getenv("URL_MEDIDAS_PLUVIOS_TELEMEDIDA")
}

postFijoPluvios <- ''
nombreExperimento <- paste0('2021_12', postFijoPluvios)
pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'
pathResultados <- 'Resultados/'
pathResultadosQC <- paste0(pathResultados, '2-QC', nombreExperimento, '/')
pathSHPMapaBase <- paste0(pathDatos, 'CartografiaBase/uruguay_mas_cuenca_rio_negro.shp')
pathSHPSubCuencas <- paste0(pathDatos, 'CartografiaBase/SubcuencasModelo/mini_para_modelo_RioNegro.shp')

# Actualizo la definici�n de CRS de los objetos espaciales para ser compatible con PROJ6/GDAL3
proj4stringLatLong <- "+proj=longlat +datum=WGS84 +no_defs"
proj4stringAInterpolar <- "+proj=utm +zone=21 +south +datum=WGS84 +units=km +no_defs"
wktLatLong <- "EPSG:4326"
wktAInterpolar <- readLines(paste0(pathDatos, 'CRS/utm21s_km.wkt'))
  
# La grilla resultante tendr� factorEscaladoGrillaInterpolacion p�xeles en cada direcci�n por cada
# pixel de la grilla de los regresores
factorEscaladoGrillaInterpolacion <- 2

# El software asume que los datos de pluvi�metros seguir�n el criterio de guardado del "d�a i + 1":
# Un dato observado en el per�odo (d�a i a las 07:00 LT, d�a i + 1 a las 07:00 LT] ser� 
# almacenado con fecha del d�a i + 1. 
# Ie: el dato almacenado con fecha 2022-01-01 corresponde al per�odo 
# (2021-12-31 a las 07:00 LT, 2022-01-01 a las 07:00 LT]
# Los datos agregados, sean tanto de pluvi�metros como de sat�lites seguir�n tambi�n 
# este criterio, as� como los resultados producidos por la librer�a.

# Descarga de datos de pluviometros
source('descargaDatosPluviometros.r', encoding = 'WINDOWS-1252')

if (Sys.getenv(x='URL_MEDIDAS_PLUVIOS_CONVENCIONALES') == '') {
  stop('La variable de entorno URL_MEDIDAS_PLUVIOS_CONVENCIONALES no se encuentra definida. Defina su valor y vuelva a intentarlo')
}
if (Sys.getenv(x='URL_MEDIDAS_PLUVIOS_TELEMEDIDA') == '') {
  stop('La variable de entorno URL_MEDIDAS_PLUVIOS_TELEMEDIDA no se encuentra definida. Defina su valor y vuelva a intentarlo')
}
if (Sys.getenv(x='URL_MEDIDAS_PLUVIOS_RESPALDO') == '') {
  stop(paste0(
    'La variable de entorno URL_MEDIDAS_PLUVIOS_RESPALDO no se encuentra definida. ',
    'Defina su valor y vuelva a intentarlo'))
}

print(paste0(Sys.time(), ' - Descargando datos de pluviometros convencionales de ADME del ', dt_ini, ' al ', dt_fin))
datosConvencionales <- descargaPluviosADMEConvencionales(
  dt_ini=dt_ini, 
  dt_fin=dt_fin, 
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_CONVENCIONALES'),
  pathSalida=paste0(pathDatos, 'pluviometros/'), 
  forzarReDescarga=forzarReDescarga
)
logDatosObtenidosPluviometros(datosConvencionales, " convencionales de UTE")

print(paste0(Sys.time(), ' - Descargando datos de pluviometros de telemedida de ADME del ', dt_ini, ' al ', dt_fin))
datosTelemedida <- descargaPluviosADMETelemedida(
  dt_ini=dt_ini, 
  dt_fin=dt_fin, 
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_TELEMEDIDA'),
  pathSalida=paste0(pathDatos, 'pluviometros/'), 
  forzarReDescarga=forzarReDescarga)
logDatosObtenidosPluviometros(datosTelemedida, " de telemedida de UTE")

print(paste0(Sys.time(), ' - Descargando datos de pluviometros de respaldo del ', dt_ini, ' al ', dt_fin))
datosRespaldo <- descargaPluviosRespaldo(
  dt_ini=dt_ini, 
  dt_fin=dt_fin,
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_RESPALDO'),
  pathSalida=paste0(pathDatos, 'pluviometros/'), 
  forzarReDescarga=forzarReDescarga)
logDatosObtenidosPluviometros(datosRespaldo, " de respaldo")

datos <- concatenarDatos(datos1 = datosConvencionales, datos2 = datosTelemedida)
datos <- concatenarDatos(datos1 = datos, datos2 = datosRespaldo)

if (is.null(datos)) {
  stop(paste0(
    Sys.time(), 
    " - No se obtuvo datos de ninguna red pluviom�trica. El sistema no puede continuar."
  ))
} else {
  logDatosObtenidosPluviometros(
    datos, postFijoMsj=' en total tras concatenar las redes'
  )
}

iOrden <- order(datos$estaciones$NombreEstacionR)
datos$estaciones <- datos$estaciones[iOrden, , drop=F]
datos$datos <- datos$datos[, iOrden, drop=F]

rm(datosConvencionales, datosTelemedida, datosRespaldo)

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

estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
rownames(datos$datos) <- as.character(fechasObservaciones)
valoresObservaciones <- datos$datos

if (!is.null(estacionesADescartar)) {
  iAConservar <- !estaciones$NombreEstacionR %in% estacionesADescartar
  print(paste0(Sys.time(), ' - Descartando datos de  ', sum(!iAConservar), ' estaciones especificadas para descarte.'))
  
  estaciones <- estaciones[iAConservar, ]
  valoresObservaciones <- valoresObservaciones[, iAConservar, drop=F]
  rm(iAConservar)
}

# Descarga de datos de satelite
source('descargaDatosSatelites.r', encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'), encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Cargando shapefile con mapa base de ', pathSHPMapaBase, '...'))
shpBase <- cargarSHP(pathSHPMapaBase, encoding = 'CP1252')
print(paste0(Sys.time(), ' - Descargando datos de GSMaP del ', dt_ini, ' al ', dt_fin))
pathsGSMaP <- descargaGSMaP(
  dt_ini=dt_ini, dt_fin=dt_fin, horaUTCInicioAcumulacion=horaUTCInicioAcumulacion, 
  shpBase=shpBase, forzarReDescarga=forzarReDescarga, borrarDatosOriginales=borrarDatosOriginales
)
loadGSMaPV8 <- FALSE
if (loadGSMaPV8) {
  print(paste0(Sys.time(), ' - Descargando datos de GSMaP_v8 del ', dt_ini, ' al ', dt_fin))
  pathsGSMaPv8 <- descargaGSMaP(
    dt_ini=dt_ini, dt_fin=dt_fin, horaUTCInicioAcumulacion=horaUTCInicioAcumulacion,
    shpBase=shpBase, forzarReDescarga=forzarReDescarga, borrarDatosOriginales=borrarDatosOriginales, 
    productVersion='v8'
  )
}
print(paste0(Sys.time(), ' - Descargando datos de IMERG del ', dt_ini, ' al ', dt_fin))
pathsIMERG <- descargaIMERG(
  dt_ini=dt_ini, dt_fin=dt_fin, horaUTCInicioAcumulacion=horaUTCInicioAcumulacion, 
  shpBase=shpBase, forzarReDescarga=forzarReDescarga, borrarDatosOriginales=borrarDatosOriginales
)

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
if (loadGSMaPV8) {
  pathsRegresores <- as.matrix(cbind(pathsIMERG, pathsGSMaP, pathsGSMaPv8))
} else {
  pathsRegresores <- as.matrix(cbind(pathsIMERG, pathsGSMaP))
}
pathsRegresores <- pathsRegresores[
  , apply(X=pathsRegresores, MARGIN=2, FUN=function(x) {!all(is.na(x))}), drop=F]

# Definici�n de la grilla de interpolaci�n.
# Igual a la del primer regresor con factorEscaladoGrillaInterpolacion celdas en la nueva grilla por
# cleda del satelite
grillaRegresor <- geometry(readGDAL(pathsRegresores[1, 1]))
newNCeldasX <- as.integer(round(grillaRegresor@grid@cells.dim[1] * factorEscaladoGrillaInterpolacion))
coordsAInterpolar <- grillaPixelesSobreBoundingBox(
  objSP=grillaRegresor, outputCRS=CRS(proj4stringAInterpolar), nCeldasX=newNCeldasX
)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar, data.frame(rep(1, length(coordsAInterpolar)))), spTransform(shpBase, CRSobj = CRS(proj4string(coordsAInterpolar))))


# Convierto los dataframes del paso 2 a objetos espaciales del paquete SP
# Convertimos el data.frame de estaciones en un objeto espacial de tipo SpatialPointsDataFrame, es 
# un objeto espacial con geometr�as tipo puntos y con una tabla de valores asociados
coordsObservaciones <- estaciones
sp::coordinates(coordsObservaciones) <- c('longitud', 'latitud')

class(coordsObservaciones)
# Las coordenadas de las estaciones est�n sin proyectar, es decir directamente en latitud/longitud. 
# Le asignamos al objeto una proyecci�n que represente esto
proj4string(coordsObservaciones) <- CRS(projargs = proj4stringLatLong, SRS_string = wktLatLong)

# Reproyectamos las estaciones a la misma proyecci�n que la grilla a interpolar
coordsObservaciones <- spTransform(x = coordsObservaciones, CRS(projargs=proj4stringAInterpolar))
coordsObservaciones$value <- rep(NA_real_, nrow(coordsObservaciones))
iValue <- which(colnames(coordsObservaciones@data) == 'value')
coordsObservaciones@data = coordsObservaciones@data[, c(iValue, (1:ncol(coordsObservaciones@data))[-iValue])]

# TODO: esto funciona pero el encoding deber�a ser uno solo
if (.Platform$OS.type == "windows") {
  shpEncoding <- 'UTF-8NoBOM'
} else {
  shpEncoding <- 'WINDOWS-1252'
}

# Shapefile con el contorno del pa�s y m�scara para los p�xeles de la grilla que son internos al contorno
shpMask <- cargarSHPYObtenerMascaraParaGrilla(
  pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, encoding = shpEncoding)
shpBase = shpMask$shp
# Objeto auxiliar con los ejes de la grilla y el �rea de mapeo, para que sea igual para todos los 
# mapas independientemente de los datos que tenga
xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpBase), ejesXYLatLong = T)

# Grilla para QC de los satelites
grillaRegresor <- as(object = grillaRegresor, Class = 'SpatialPixels')
shpRioNegro <- shpBase[shpBase$NOMBRE == 'CuencaRioNegro', ]
if (length(shpRioNegro) != 1) { stop('cargaDatos.r: error obteniendo pol�gono del R�o Negro') }
shpBufferRioNegro <- spTransform(gBuffer(shpRioNegro, width = 60), grillaRegresor@proj4string)
i <- !is.na(over(grillaRegresor, shpBufferRioNegro))
coordsQC <- grillaRegresor[i, ]

i <- !is.na(over(coordsAInterpolar, geometry(shpRioNegro)))
coordsAInterpolar <- coordsAInterpolar[i, ]

shpMask <- cargarSHPYObtenerMascaraParaGrilla(
  pathSHP=pathSHPMapaBase, grilla=coordsAInterpolar, encoding = shpEncoding)

print(paste0(Sys.time(), ' - Cargando shapefile con subcuencas...'))
shpSubCuencas <- cargarSHP(pathSHP = pathSHPSubCuencas)
if (!identicalCRS(coordsAInterpolar, shpSubCuencas)) {
  shpSubCuencas <- spTransform(shpSubCuencas, coordsAInterpolar@proj4string)
}

estacionesDeReferencia <- readLines(con=paste0(pathDatos, 'pluviometros/estacionesDeReferencia.txt'))
iEstacionesDeReferencia <- estaciones$NombreEstacionR %in% estacionesDeReferencia
iEstacionesNoReferencia <- which(!iEstacionesDeReferencia)
iEstacionesDeReferencia <- which(iEstacionesDeReferencia)

getCorrs <- function(coordsObservaciones, valoresObservaciones, pathsRegresores, logTransforms=TRUE) {
  valoresRegresores <- extraerValoresRegresoresSobreSP(
    objSP=coordsObservaciones, pathsRegresores=pathsRegresores)
  
  if (logTransforms) {
    valoresObservaciones <- log1p(valoresObservaciones)
    valoresRegresores <- lapply(valoresRegresores, log1p)
  }
  
  # j <- 1
  # i <- 7
  # i <- which(row.names(valoresObservaciones) == '2017-04-13')
  corrs <- sapply(1:ncol(pathsRegresores), function(j) {
    return(
      sapply(1:nrow(valoresObservaciones), FUN = function(i) {
        idx <- !is.na(valoresObservaciones[i, ]) & !is.na(valoresRegresores[[j]][i, ])
        if (any(idx)) {
          if (max(abs(valoresObservaciones[i, idx] - valoresRegresores[[j]][i, idx])) <= 1e-3) {
            return(1)
          } else {
            return(cor(valoresObservaciones[i, idx], valoresRegresores[[j]][i, idx], 
                       use = "pairwise.complete.obs", method = "pearson"))
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

getRegresorCombinado <- function(
    coordsObservaciones, valoresObservaciones, pathsRegresores, logTransforms=TRUE
) {
  corrs <- getCorrs(
    coordsObservaciones=coordsObservaciones,
    valoresObservaciones=valoresObservaciones, 
    pathsRegresores=pathsRegresores,
    logTransforms=FALSE
  )
  
  res <- matrix(
    data=rep(NA_character_, nrow(valoresObservaciones)), 
    ncol=1, 
    dimnames=list(rownames(valoresObservaciones), 'Combinado')
  )
  for (iRow in 1:nrow(pathsRegresores)) {
    idx <- which.max(corrs[iRow, ])
    if (length(idx) > 0) {
      res[iRow, 1] <- pathsRegresores[iRow, idx]
    } else if (!is.na(pathsRegresores[iRow, 1])) {
      res[iRow, 1] <- pathsRegresores[iRow, 1]
    } else {
      res[iRow, 1] <- pathsRegresores[iRow, 2] 
    }
  }
  return(res)
}
