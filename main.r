setwd('F:/ADME/precip_rionegro')

# �ndice
# 5 - Preparaci�n de Par�metros
# 6 - Interpolaci�n de los datos
# 7 - Cross Validation
pathSTInterp <- 'st_interp/'
pathDatos <- 'datos/'

dt_ini='2018-10-21'
dt_fin = '2019-12-07'
horaUTCInicioAcumulacion = 10
horaLocalInicioAcumulacion = horaUTCInicioAcumulacion - 3

source('cargaDatos.r', encoding = 'WINDOWS-1252')

# 5 - Preparaci�n de Par�metros
# La funci�n que tenemos implementada para hacer la interpolaci�n se llama interpolarYMapear en 
# interpolarYMapearEx.r
# Tiene muchos par�metros que est�n pensados para ser llenados por software pero la podemos 
# llamar "a mano" cr�andole los objetos de entrada que precisa
# universalGridding implementa la "ecuaci�n maestra" 
# Z(s, t) = F(U1((s, t), U2(s, t), ... Un(s, t))) + Z*({Zi(s,t) - F(U1((s, t), U2(s, t), ... Un(s, t)))} s, t) + eps"(s, t)

# El m�todo a usar en las funciones F y Z* se pasa en un objeto de par�metros en los campos
# "metodoIgualacionDistribuciones" para F e "interpolationMethod" para Z*
# Los valores de metodoIgualacionDistribuciones que venimos manejando en la tesis son:
# 1- 'ninguna'  sin ajuste de distribuciones, F(s, t) = U1(s, t), los dem�s regresores se ignoran
# 2- 'regresionLineal' regresi�n con MCO  F(s, t) = a0 + a1 * U1(s, t) + a2 * U2(s, t) + ... + an * Un(s, t) 
# 3- 'regresionLinealRobusta' regresi�n robusta, igual que arriba pero con RLM

# Los valores de interpolationMethod que venimos manejando en la tesis son:
# 1-'automap' Kriging. Seg�n lo que se haya elegido en metodoIgualacionDistribuciones ser� KO, RK, RRK, etc.

# Estos fuentes tienen varias funciones necesarias para la interpolaci�n. 
# Tiene las funciones para hacer kriging, universal kriging, la CV, SRT, etc...
source(paste0(pathSTInterp, 'interpolar/interpolarEx.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')

# Este tiene una funci�n de ayuda que crea el objeto de par�metros para universalGridding
source(paste0(pathSTInterp, 'interpolar/parsearParamsInterpolarYMapear.r'), encoding = 'WINDOWS-1252')
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
                                        pathSHPMapaBase=pathSHPMapaBase,
                                        nCoresAUsar=0, 
                                        modoDiagnostico = TRUE)


# mapearPuntosGGPlot(SpatialPointsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), shpBase = shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar, data = data.frame(rep(1, length(coordsAInterpolar)))), shpBase = shpBase)
# mapearGrillaGGPlot(SpatialPixelsDataFrame(coordsAInterpolar[shpMask$mask], data = data.frame(rep(1, length(coordsAInterpolar[shpMask$mask])))), shpBase = shpBase)

# guardarSHP(as(SpatialPixelsDataFrame(coordsQC, data = data.frame(rep(1, length(coordsQC)))), 'SpatialPointsDataFrame'), 'lala.shp')
GPM = extraerValoresRegresorSobreSP(objSP = coordsQC, pathsRegresor = pathsRegresores[, 1])
GSMaP = extraerValoresRegresorSobreSP(objSP = coordsQC, pathsRegresor = pathsRegresores[, 2])

source(paste0(pathSTInterp, 'qc/qcTests.r'), encoding = 'WINDOWS-1252')
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

source('graficosParticulares.r', encoding = 'WINDOWS-1252')
plotObservacionesYRegresores(coordsObservaciones, fechasObservaciones, valoresObservaciones, 
                             pathsRegresoresAEvaluar = pathsRegresores, shpBase = shpBase, 
                             nColsPlots = 3, carpetaSalida = 'Resultados/1-Exploracion/mapas', 
                             grillaAlternativaRegresores=coordsAInterpolar, replot = TRUE)

# Las climatolog�as son un poco diferentes y por ahora las cargo "a mano". 
# Al pasarle por arriba a pathsRegresores, estoy dejando como regresor solo las climatolog�as, 
# dejo los dos ejemplos por si quieren probar otras cosas
#diasDelAnio <- yday(tempAireMin$fechas)
#diasDelAnio[diasDelAnio == 366] <- 365
#pathsRegresores <- matrix(paste(pathDatos, sprintf('LST_Night_Combinada_Clim_mean/%03d.tif', diasDelAnio), sep=''), ncol = 1)
#colnames(pathsRegresores) <- 'LST_Night_Combinada_Clim_mean'

# Otros valores que determinan los regresores a usar son estos 3. Para precipitaci�n en principio
# ninguno de ellos es relevante asI que los desactivamos
params$incorporarCoordenadas <- FALSE
params$incorporarAltitud <- FALSE
params$incorporarDistanciaAlAgua <- FALSE
# Y estos determinan como se usan los de arriba, el valor por defecto es un gradiente pero con 
# otras formulas se pueden usar polinomios, etc
params$formulaCoordenadas <- 'x + y'


# Otros par�metros adicionales. listaMapas dice como se deben llamar los archivos de salida y que
# guardar. geoTiff, png con escala fija/ajustada, etc. Los valores por defecto est�n bien
listaMapas <- createDefaultListaMapas(paramsIyM = params, fechasObservaciones = fechasObservaciones,
                                      dibujarEscalaFija=F, salvarGeoTiff=T, recalcularSiYaExiste=F)


# Una vez que est� todo armado, la funci�n nombreModelo sirve para ver que el modelo que
# especificamos sea efectivamente el que queremos
nombreModelo(params = params, pathsRegresores = pathsRegresores)

# 6 - Interpolaci�n de los datos
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
# La funci�n universalGriddingCV retorna una matriz de las mismas dimensiones que valoresObservaciones con 
# cv[i, j] el valor de la LOOCV de la estacion j en la fecha i. Es decir el valor de cv[i, j] es la estimaci�n LOOCV de valoresObservaciones[i, j]
cv <- universalGriddingCV(coordsObservaciones=coordsObservaciones, 
                          fechasObservaciones=fechasObservaciones, 
                          valoresObservaciones=valoresObservaciones, 
                          params=params, 
                          pathsRegresores=pathsRegresores, 
                          longitudesEnColumnas=T,
                          iesAEstimar=1:ncol(valoresObservaciones),  # Esto permite hacer la cv solo para las columnas en iesAEstimar
                          eliminarSerieTemporalCompleta=TRUE,        # Esto permite elegir si se saca toda la columna de una estaci�n para hacer la CV o solo el valor de la fecha
                          estimarNAs=FALSE)                          # Esto es una optimizaci�n donde si valoresObservaciones[i, j] es NA no se estima cv[i, j] porque igual no hay nada para comparar
