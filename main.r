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

source(paste(pathSTInterp, 'descargador/descargadorEx.r', sep=''))

dt_ini <- '2018-01-01'
dt_fin <- '2019-10-12'
url <- paste('***REMOVED***?dtIni=', dt_ini, '&dtFin=', dt_fin, 
             sep = '')
localFile <- paste(gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall.xlsx', sep = '')
descargarArchivos(urls = url, nombresArchivosDestino = localFile, forzarReDescarga = T)

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
instant_pkgs(c('sp', 'gstat', 'Cairo', 'rgdal', 'devEMF'))


# 2 - Lectura de datos de series temporales de observaciones puntuales de las estaciones
source(paste(pathSTInterp, 'SeriesTemporales/leerSeriesTemporales.r', sep=''))
datos <- leerSeriesXLSX(localFile)
estaciones <- datos$estaciones
fechasObservaciones <- datos$fechas
valoresObservaciones <- datos$datos


# 3 - Lectura de rasters del satélite en formato geoTiff y definición de la grilla a interpolar
# Leemos la grilla en el archivo fname
source(paste(pathSTInterp, 'grillas/uIOGrillas.r', sep=''))
proj4StringAInterpolar <- "+proj=utm +zone=21 +south +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"
coordsAInterpolar <- leerGrillaGDAL(nombreArchivo = paste(pathDatos, 'grilla_uy.tiff', sep=''))

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

# Shapefile con el contorno del país y máscara para los píxeles de la grilla que son internos al contorno
shpMask <- cargarSHPYObtenerMascaraParaGrilla(pathSHP=params$pathSHPMapaBase, grilla=coordsAInterpolar)
# Objeto auxiliar con los ejes de la grilla y el área de mapeo, para que sea igual para todos los 
# mapas independientemente de los datos que tenga
xyLims <- getXYLims(spObjs = c(coordsAInterpolar, shpMask$shp), ejesXYLatLong = T)

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
