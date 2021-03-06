if (dir.exists('F:/ADME/precip_rionegro')) { setwd('F:/ADME/precip_rionegro')
} else if (dir.exists('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')) { setwd('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')
} else if (dir.exists('D:/ADME/precip_rionegro')) { setwd('D:/ADME/precip_rionegro') }


# L�nea de comandos de ejemplo: 
# - Rscript main.r dt_fin=2021-01-16
# Esto producir� el mapa con los acumulados del per�odo (2020-01-15 10:00 UTC, 2020-01-16 10:00 UTC]
# El archivo de s�lida se guarda en Resultados/Operativo/2021_01_15.tif.
# El archivo se guarda con el nombre de la fecha de inicio para usar el mismo criterio de registro
# utilizado por INUMET, donde se guarda en la fecha de inicio pues el per�odo tiene m�s horas (14)
# el d�a 15 que las que tiene el d�a 16.


# Imprimo los par�metros con los que se llam� el script para que quede en el log
paramsStr <- commandArgs(trailingOnly=T)
# paramsStr <- 'dt_fin=2021-01-15'
if (length(paramsStr) == 0) { paramsStr <- '' }
print(paste('ParamsStr="', paramsStr, '"', sep = ''))

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
if (params$dt_fin==Sys.Date() && as.POSIXlt(Sys.time())$hour < 22) {
  dt_fin <- as.character(as.Date(params$dt_fin) - 1)
}

# Descomentar estas fechas para setearlas manualmente
if (interactive() && length(paramsStr) == 0) {
  dt_fin=as.character(Sys.Date())
  dt_ini=NA
  dt_ini="2017-02-01"
}
if (is.na(dt_ini)) {
  dt_ini <- as.Date(dt_fin)-1
}

estacionesADescartar <- c(
  'ANSINA.Paso.BORRACHO.RHT', 'PASO.MAZANGANO.RHT', 'PASO.LAGUNA.I.RHT', 'PASO.AGUIAR.RHT',
  'PASO.PEREIRA.RHT', 'PASO.NOVILLOS.RHT', 'VILLA.SORIANO.RHT')
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- !interactive()
borrarDatosOriginales <- forzarReDescarga
#borrarDatosOriginales <- FALSE
pathResultadosOperativos = 'Resultados/Operativo/'

source('cargaDatos.r', encoding = 'WINDOWS-1252')
source('aplicaQC.r', encoding = 'WINDOWS-1252')
print(paste0(Sys.time(), ' - Aplicando Tests de QC...'))
valoresObservaciones <- applyQCTests(
  coordsObservaciones, fechasObservaciones, valoresObservaciones, 
  paramsInterpolacion = paramsInterpolacionQCTests, pathsRegresores = pathsRegresores, 
  plotMaps = TRUE)

# Guardamos el mapa de pluvi�metros y sat�lites en datos/mapas/
source('graficosParticulares.r', encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/leerEscalas.r'), encoding = 'WINDOWS-1252')
especificacionEscala <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(
  nDigitos = 1, continuo = T)
print(paste0(Sys.time(), ' - Mapeando observaciones de pluviometros y satelites...'))
plotObservacionesYRegresores(
  coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones,
  pathsRegresoresAEvaluar=pathsRegresores, valoresObservaciones=valoresObservaciones, 
  shpBase=shpBase, replot = forzarReDescarga, grillaAlternativaRegresores=coordsAInterpolar, 
  carpetaSalida='datos/mapas/', especificacionEscala=especificacionEscala)


# Estos fuentes tienen varias funciones necesarias para la interpolaci�n. 
# Tiene las funciones para hacer kriging, universal kriging, la CV, SRT, etc...
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')

# Este tiene una funci�n de ayuda que crea el objeto de par�metros para universalGridding
source(paste0(pathSTInterp, 'interpolar/parsearParamsInterpolarYMapear.r'), encoding = 'WINDOWS-1252')
# 5 - Preparaci�n de Par�metros
# La funci�n que tenemos implementada para hacer la interpolaci�n se llama interpolarYMapear en 
# interpolarYMapearEx.r
# Tiene muchos par�metros que est�n pensados para ser llenados por software pero la podemos 
# llamar "a mano" cr�andole los objetos de entrada que precisa
# universalGridding implementa la "ecuaci�n maestra" 
# Z(s, t) = F(U1(s, t), U2(s, t), ... Un(s, t)) + Z*({Zi(s,t) - F(U1(s, t), U2(s, t), ... Un(s, t))}, s, t) + eps"(s, t)

# El m�todo a usar en las funciones F y Z* se pasa en un objeto de par�metros en los campos
# "metodoIgualacionDistribuciones" para F e "interpolationMethod" para Z*
# Los valores de metodoIgualacionDistribuciones que venimos manejando en la tesis son:
# 1- 'ninguna'  sin ajuste de distribuciones, F(s, t) = U1(s, t), los dem�s regresores se ignoran
# 2- 'regresionLineal' regresi�n con MCO  F(s, t) = a0 + a1 * U1(s, t) + a2 * U2(s, t) + ... + an * Un(s, t) 
# 3- 'regresionLinealRobusta' regresi�n robusta, igual que arriba pero con RLM

# Los valores de interpolationMethod que venimos manejando en la tesis son:
# 1-'automap' Kriging. Seg�n lo que se haya elegido en metodoIgualacionDistribuciones ser� KO, RK, RRK, etc.
print(paste0(Sys.time(), ' - Preparando parametros de interpolacion...'))
params <- createParamsInterpolarYMapear(
  baseNomArchResultados=pathResultadosOperativos,
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
params$especEscalaDiagnostico <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(nDigitos = 2, continuo = T)

print(paste0(Sys.time(), ' - Obteniendo regresor de maxima correlacion...'))
corrs <- getCorrs(valoresObservaciones, pathsRegresores, logTransforms = FALSE)

pathsRegresores <- cbind(pathsRegresores, rep(NA_character_, nrow(pathsRegresores)))
colnames(pathsRegresores)[ncol(pathsRegresores)] <- 'Combinado'
for (iRow in 1:nrow(pathsRegresores)) {
  idx <- which.max(corrs[iRow, ])
  if (length(idx) > 0) {
    pathsRegresores[iRow, 'Combinado'] <- pathsRegresores[iRow, idx]
  } else {
    pathsRegresores[iRow, 'Combinado'] <- pathsRegresores[iRow, 1]
  }
}
pathsRegresores <- pathsRegresores[, 'Combinado', drop=F]
params$signosValidosRegresores <- rep(1, ncol(pathsRegresores))
names(params$signosValidosRegresores) <- colnames(pathsRegresores)
# Descomentar esto para usar Kriging Ordinario
# pathsRegresores <- NULL

# Otros par�metros adicionales. listaMapas dice como se deben llamar los archivos de salida y que
# guardar. geoTiff, png con escala fija/ajustada, etc. Los valores por defecto est�n bien
listaMapas <- createDefaultListaMapas(paramsIyM = params, fechasObservaciones = fechasObservaciones,
                                      dibujarEscalaFija=F, salvarGeoTiff=T, recalcularSiYaExiste=F)


# Una vez que est� todo armado, la funci�n nombreModelo sirve para ver que el modelo que
# especificamos sea efectivamente el que queremos
print(paste0(Sys.time(), ' - Interpolando modelo ', 
             nombreModelo(params = params, pathsRegresores = pathsRegresores), '...'))


# Interpolaci�n de los datos
# Cambiando tsAinterpolar se puede elegir la fecha que se quiera
# tsAInterpolar <- which(fechasObservaciones == as.POSIXct('2018-02-03', tz=tz(fechasObservaciones[1])))
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

print(paste0(Sys.time(), ' - Finalizado. Resultados guardados en ', getwd(), '/', 
             gsub(listaMapas$nombreArchivo, pattern = 'png', replacement = 'tif')))

# Calcular y guardar acumulados por sub cuencas
acumuladosPorSubCuencas <- t(agregacionEspacialAPoligonosDesdeArchivos(
  pathsSpObjs=listaMapas$nombreArchivo, shpPoligonos=shpSubCuencas, funcionAgregacion=base::mean,
  zcol=1, na.rm=T, nCoresAUsar=0, guardarCSV=FALSE, retornarResultados=TRUE, useRaster=TRUE))

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

