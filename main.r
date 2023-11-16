if (dir.exists('G:/workspace/precip_rionegro')) { setwd('G:/workspace/precip_rionegro')
} else if (dir.exists('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')) { setwd('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')
} else if (dir.exists('D:/ADME/precip_rionegro')) { setwd('D:/ADME/precip_rionegro') }

# Línea de comandos de ejemplo: 
# - Rscript main.r dt_fin=2021-01-16
# Esto producirá el mapa con los acumulados del período (2020-01-15 10:00 UTC, 2020-01-16 10:00 UTC]
# El archivo de sálida se guarda en Resultados/Operativo/2021_01_15.tif.
# El archivo se guarda con el nombre de la fecha de inicio para usar el mismo criterio de registro
# utilizado por INUMET, donde se guarda en la fecha de inicio pues el período tiene más horas (14)
# el día 15 que las que tiene el día 16.


# Imprimo los parámetros con los que se llamó el script para que quede en el log
paramsStr <- commandArgs(trailingOnly=T)
if (interactive()) {
  #paramsStr <- 'dt_fin=2022-07-02;dt_ini=2017-02-01'
  #paramsStr <- 'dt_fin=2022-09-06;dt_ini=2017-02-01'
  #paramsStr <- 'dt_fin=2018-02-07'
  #paramsStr <- 'dt_fin=2022-01-05;dt_ini=2022-01-01'
  #paramsStr <- 'dt_fin=2023-08-16'
  paramsStr <- 'dt_fin=2023-11-15'
  #paramsStr <- 'dt_fin=2022-05-09'
} else {
  # Deshabilito warnings en corridas de produccion, pero las mantengo en sesiones
  # interactivas de desarrollo
  options(warn=-1)
}

if (length(paramsStr) == 0) { paramsStr <- '' }
print(paste0('ParamsStr="', paramsStr, '"'))

source('st_interp/parsearParams/parsearParamsUtils.r')
createParamsPrecipRioNegro <- function(dt_ini=NA_character_, dt_fin=as.character(Sys.Date())) {
  res <- list(dt_ini=dt_ini, dt_fin=dt_fin)
  return(res)
}

parsearParamsPrecipRioNegro <- function(params) {
  return(getParamValuesFromConstructorParams(params, funcCrearParams=createParamsPrecipRioNegro))
}

params <- parsearParamsPrecipRioNegro(paramsStr)
dt_ini=params$dt_ini
dt_fin=params$dt_fin
if (params$dt_fin==Sys.Date() && as.POSIXlt(Sys.time())$hour < 7) {
  dt_fin <- as.character(as.Date(params$dt_fin) - 1)
}

# Descomentar estas fechas para setearlas manualmente
if (interactive() && length(paramsStr) == 0) {
  dt_fin=as.character(Sys.Date())
  dt_ini=NA
  #dt_ini="2017-02-01"
}
if (is.na(dt_ini)) {
  #dt_ini <- as.Date(dt_fin)-1
  dt_ini <- dt_fin
}

estacionesADescartar <- NULL
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- !interactive()
# forzarReDescarga <- TRUE
borrarDatosOriginales <- forzarReDescarga
#borrarDatosOriginales <- FALSE
pathResultadosOperativos = 'Resultados/Operativo/'

if (dt_ini == dt_fin) {
  print(paste0(Sys.time(), ' - Ejecutando merging para la fecha ', dt_fin, '...'))  
} else {
  print(paste0(Sys.time(), ' - Ejecutando merging para dt_ini=', dt_ini, ', dt_fin=', dt_fin, '...')) 
}

source('cargaDatos.r')
source('aplicaQC.r')
print(paste0(Sys.time(), ' - Aplicando Tests de QC...'))
valoresObservaciones <- applyQCTests(
  coordsObservaciones, fechasObservaciones, valoresObservaciones, 
  paramsInterpolacion=paramsInterpolacionQCTests, pathsRegresores=pathsRegresores, 
  plotMaps=TRUE
)

# Guardamos el mapa de pluviómetros y satélites en datos/mapas/
source('graficosParticulares.r')
source(paste0(pathSTInterp, 'interpolar/leerEscalas.r'))
especificacionEscala <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(
  nDigitos = 1, continuo = T)
print(paste0(Sys.time(), ' - Mapeando observaciones de pluviometros y satelites...'))
plotObservacionesYRegresores(
  coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones,
  pathsRegresoresAEvaluar=pathsRegresores, valoresObservaciones=valoresObservaciones, 
  shpBase=shpBase, replot = forzarReDescarga, grillaAlternativaRegresores=coordsAInterpolar, 
  carpetaSalida='datos/mapas/', especificacionEscala=especificacionEscala)


# Estos fuentes tienen varias funciones necesarias para la interpolación. 
# Tiene las funciones para hacer kriging, universal kriging, la CV, SRT, etc...
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'))
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'))
source(paste0(pathSTInterp, 'interpolar/funcionesAuxiliares.r'))

# Este tiene una función de ayuda que crea el objeto de parámetros para universalGridding
source(paste0(pathSTInterp, 'interpolar/parsearParamsInterpolarYMapear.r'))
# 5 - Preparación de Parámetros
# La función que tenemos implementada para hacer la interpolación se llama interpolarYMapear en 
# interpolarYMapearEx.r
# Tiene muchos parámetros que están pensados para ser llenados por software pero la podemos 
# llamar "a mano" créandole los objetos de entrada que precisa
# universalGridding implementa la "ecuación maestra" 
# Z(s, t) = F(U1(s, t), U2(s, t), ... Un(s, t)) + Z*({Zi(s,t) - F(U1(s, t), U2(s, t), ... Un(s, t))}, s, t) + eps"(s, t)

# El método a usar en las funciones F y Z* se pasa en un objeto de parámetros en los campos
# "metodoIgualacionDistribuciones" para F e "interpolationMethod" para Z*
# Los valores de metodoIgualacionDistribuciones que venimos manejando en la tesis son:
# 1- 'ninguna'  sin ajuste de distribuciones, F(s, t) = U1(s, t), los demás regresores se ignoran
# 2- 'regresionLineal' regresión con MCO  F(s, t) = a0 + a1 * U1(s, t) + a2 * U2(s, t) + ... + an * Un(s, t) 
# 3- 'regresionLinealRobusta' regresión robusta, igual que arriba pero con RLM

# Los valores de interpolationMethod que venimos manejando en la tesis son:
# 1-'automap' Kriging. Según lo que se haya elegido en metodoIgualacionDistribuciones será KO, RK, RRK, etc.
print(paste0(Sys.time(), ' - Preparando parametros de interpolacion...'))
params <- createParamsInterpolarYMapear(
  baseNomArchResultados=pathResultadosOperativos,
  proj4StringObservaciones=wkt(coordsObservaciones),
  proj4StringAInterpolar=wkt(coordsAInterpolar),
  coordsAInterpolarSonGrilla=TRUE, 
  interpolationMethod='automap',
  mLimitarValoresInterpolados='LimitarMinimoyMaximo',
  minimoLVI=0, maximoLVI=450,
  factorDesvEstLVI=3.5,
  metodoIgualacionDistribuciones='GLS',
  formulaRegresion='',
  ventanaIgualacionDistribuciones=0,
  incorporarCoordenadas=FALSE, 
  formulaCoordenadas='x + y',
  incorporarTiempo=FALSE, 
  formulaTiempo='t',
  incorporarDistanciaAlAgua=FALSE, 
  formulaDistanciaAlAgua='I(dist^0.125)',
  incorporarAltitud=FALSE, 
  formulaAltitud='alt',
  descartarCoordenadasNoSignificativas=TRUE,
  rellenarRegresores=FALSE,
  invertirAjusteRegresores=FALSE,
  usarNugget=FALSE,
  block=NA,
  nmin=0,
  nmax=Inf,
  maxdist=Inf,
  inverseDistancePower=NA,
  umbralMascaraCeros=0.3,
  metodoRemocionDeSesgo='IDW_ResiduosPositivos',
  modelosVariograma=c('Exp', 'Sph', 'Pow', 'Cir', 'Pen'),
  #cutoff=quantile(spDists(coordsObservaciones, longlat=!is.projected(coordsObservaciones)), probs = 0.4),
  cutoff=Inf,
  tlags=0:5, 
  nTsST=0,#max(tlags), 
  tlagsAR=NULL,
  tryFixNugget=FALSE, 
  nPuntosIniciales=2,
  usarFitVariogramGLS='auto',
  modelosVariogramaST=c('Separable', 'ProductSum', 'Metric', 'SimpleSumMetric'),
  fit.methodST=6,
  verbose=FALSE,
  pathSHPMapaBase=paste(pathDatos, 'MapaUruguay/uruguay_departamentos.shp', sep=''),
  nCoresAUsar=0, 
  radioReduccionSeriesKm=0,
  funcionReduccionSeries='mean',
  difMaxFiltradoDeOutliersRLM = 0,
  difMaxFiltradoDeOutliersCV = 0,
  modoDiagnostico=TRUE,
  simpleKrigingEnRK=FALSE,
  preECDFMatching=FALSE)
params$modoDiagnostico <- TRUE
params$especEscalaDiagnostico <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(
  nDigitos = 2, continuo = T)

print(paste0(Sys.time(), ' - Obteniendo regresor de maxima correlacion...'))
pathsRegresores <- getRegresorCombinado(
  coordsObservaciones, valoresObservaciones, pathsRegresores, logTransforms = FALSE
)
print(pathsRegresores)
params$signosValidosRegresores <- rep(1, ncol(pathsRegresores))
names(params$signosValidosRegresores) <- colnames(pathsRegresores)
# Descomentar esto para usar Kriging Ordinario
# pathsRegresores <- NULL

# Otros parámetros adicionales. listaMapas dice como se deben llamar los archivos de salida y que
# guardar. geoTiff, png con escala fija/ajustada, etc. Los valores por defecto están bien
listaMapas <- createDefaultListaMapas(paramsIyM = params, fechasObservaciones = fechasObservaciones,
                                      dibujarEscalaFija=F, salvarGeoTiff=T, recalcularSiYaExiste=F)


# Una vez que está todo armado, la función nombreModelo sirve para ver que el modelo que
# especificamos sea efectivamente el que queremos
print(paste0(Sys.time(), ' - Interpolando modelo ', 
             nombreModelo(params=params, pathsRegresores=pathsRegresores), '...'))


# Interpolación de los datos
# Cambiando tsAinterpolar se puede elegir la fecha que se quiera
# tsAInterpolar <- which(fechasObservaciones == as.POSIXct('2018-02-03', tz=tz(fechasObservaciones[1])))
tsAInterpolar=1:nrow(valoresObservaciones)
interpolarYMapear(
  coordsObservaciones=coordsObservaciones, 
  fechasObservaciones=fechasObservaciones, 
  valoresObservaciones=valoresObservaciones, 
  pathsRegresores=pathsRegresores,
  #pathsRegresores=NULL, 
  coordsAInterpolar=coordsAInterpolar, 
  paramsIyM=params,
  shpMask=shpMask,
  xyLims=xyLims,
  listaMapas=listaMapas,
  returnInterpolacion=0L,  # Para ahorrar memoria
  paramsParaRellenoRegresores=NULL,
  pathsRegresoresParaRellenoRegresores=NULL,
  espEscalaFija=NULL,
  espEscalaAdaptada=NULL,
  tsAInterpolar=tsAInterpolar
)

print(paste0(Sys.time(), ' - Finalizado. Resultados guardados en ', getwd(), '/', 
             gsub(listaMapas$nombreArchivo, pattern = 'png', replacement = 'tif')))

# Calcular y guardar acumulados por sub cuencas
acumuladosPorSubCuencas <- t(agregacionEspacialAPoligonosDesdeArchivos(
  pathsSpObjs=listaMapas$nombreArchivo, shpPoligonos=shpSubCuencas, funcionAgregacion=base::mean,
  zcol=1, na.rm=T, nCoresAUsar=0, guardarCSV=FALSE, retornarResultados=TRUE, useRaster=TRUE))


#valoresObservaciones[which(apply(is.na(acumuladosPorSubCuencas), 1, all)), ]
#unlink(changeFileExt(listaMapas$nombreArchivo[which(apply(is.na(acumuladosPorSubCuencas), 1, all))], ".tif"))

rownames(acumuladosPorSubCuencas) <- gsub(
  pattern='_', replacement='-', x=nombreArchSinPathNiExtension(listaMapas$nombreArchivo), fixed=T)

archAcumuladosHoy <- changeFileExt(
  agregarCarpetaAlFinal(listaMapas$nombreArchivo[length(listaMapas$nombreArchivo)], 'acumuladosSubcuencas'), '.tsv')
nomArchHoy <- gsub(pattern = '-', replacement = '_', x = nombreArchSinPathNiExtension(archAcumuladosHoy))

nomArchAyer <- gsub(pattern = '-', replacement = '_', as.character((as.Date(dt_ini)-1)))
archAcumuladosAyer <- gsub(pattern = nomArchHoy, replacement = nomArchAyer, x = archAcumuladosHoy)

if (file.exists(archAcumuladosAyer)) {
  copyFile(archAcumuladosAyer, archAcumuladosHoy, overwrite = TRUE)
} else {
  dir.create(dirname(archAcumuladosHoy), showWarnings = F, recursive = T)
}

write.table(x = acumuladosPorSubCuencas, file = archAcumuladosHoy, append=TRUE, quote = FALSE,
            sep = '\t', na = '-1111', dec = '.', row.names = TRUE, col.names = FALSE)

