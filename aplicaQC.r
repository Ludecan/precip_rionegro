source(paste(pathSTInterp, 'qc/qcTests.r', sep=''))
source(paste(pathSTInterp, 'interpolar/interpolarYMapearEx.r', sep=''))

paramsInterpolacionQCTests <- createParamsInterpolarYMapear(
  proj4StringObservaciones=proj4string(coordsObservaciones),
  proj4StringAInterpolar=proj4string(coordsObservaciones),
  coordsAInterpolarSonGrilla=FALSE,
  interpolationMethod='none',
  mLimitarValoresInterpolados='NoLimitar',
  minimoLVI=NA, maximoLVI=NA,
  factorDesvEstLVI=NA,
  metodoIgualacionDistribuciones='regresionLinealRobusta',
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
  umbralMascaraCeros=0,
  metodoRemocionDeSesgo='ninguno',
  modelosVariograma=c('Exp', 'Sph', 'Pow', 'Cir', 'Pen'),
  cutoff=Inf,
  tlags=0:5, 
  nTsST=0,
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
  modoDiagnostico=TRUE)

applyQCTests <- function(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, paramsInterpolacion, 
    pathsRegresores, plotMaps=FALSE) {
  
  replot <- FALSE
  
  # Two rounds of QC tests
  test <- testEspacialPrecipitacion(
    coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
    valoresObservaciones = valoresObservaciones)
  
  # test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/', shpBase = shpBase, replot=replot)
  }
  
  test$reemplazar[test$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)
  
  test <- testEspacialPrecipitacion(
    coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
    valoresObservaciones = valoresObservaciones, minNCuadrantes=3, fInf = 1.4, amplitudMin = 1, 
    amplitudMinRatio = 0.15)
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/2/', shpBase = shpBase, replot=replot)
  }
  
  test$reemplazar[test$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)
  
  # Plus another round of testing with the satellites. If for a given observation both satellites
  # agree it's an extreme value, we discard it
  listaMapas <- createDefaultListaMapas(
    paramsInterpolacion, fechasObservaciones = fechasObservaciones, dibujarEscalaFija = FALSE)
  
  test <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion, 
    pathsRegresores = pathsRegresores[, 'GPM', drop=F], listaMapas = listaMapas, desvMedAbsMin = 1, 
    factorMADHaciaAbajo = 8, returnTestDF = TRUE)
  
  test2 <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion, 
    pathsRegresores = pathsRegresores[, 'GSMaP', drop=F], listaMapas = listaMapas, 
    desvMedAbsMin = 1, factorMADHaciaAbajo = 8, returnTestDF = TRUE)
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos & 
                  test$tipoOutlier == test2$tipoOutlier & 
                  (test$estimado + test2$estimado)*0.5 > 10, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/3/', shpBase = shpBase, replot=replot)
  }
  
  test[test$tipoOutlier == test2$tipoOutlier & test$tipoOutlier %in% tiposOutliersValoresSospechosos, ]$reemplazar <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)
  return(valoresObservaciones)
}