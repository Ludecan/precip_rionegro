source(paste0(pathSTInterp, 'qc/qcTests.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding = 'WINDOWS-1252')

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
    valoresObservaciones = valoresObservaciones, maxValAbs = 450)
  
  # test[test$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  # test[test$fecha == '2019-01-08',]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test[test$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/1/', shpBase = shpBase, replot=replot)
  }
  
  test$reemplazar[test$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)
  
  test2 <- testEspacialPrecipitacion(
    coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
    valoresObservaciones = valoresObservaciones, minNCuadrantes=3, fInf = 1.4, amplitudMin = 1, 
    amplitudMinRatio = 0.15)
  
  # test2[test2$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  # test2[test2$fecha == '2019-01-08',]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test2[test2$tipoOutlier %in% tiposOutliersValoresSospechosos, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/2/', shpBase = shpBase, replot=replot)
  }
  
  test2$reemplazar[test2$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test2, valoresObservaciones)
  
  # Plus another round of testing with the satellites. If for a given observation both satellites
  # agree it's an extreme value, we discard it
  listaMapas <- createDefaultListaMapas(
    paramsInterpolacion, fechasObservaciones = fechasObservaciones, dibujarEscalaFija = FALSE)
  
  test3 <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion, 
    pathsRegresores = pathsRegresores[, 'GPM', drop=F], listaMapas = listaMapas, 
    factorMADHaciaAbajo = NA, factorSDHaciaAbajo = 2.2, sdMin = 1, returnTestDF = TRUE)
  
  # test3[test3$fecha == '2019-01-08',]
  
  test4 <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion, 
    pathsRegresores = pathsRegresores[, 'GSMaP', drop=F], listaMapas = listaMapas, 
    factorMADHaciaAbajo = NA, factorSDHaciaAbajo = 2.2, sdMin = 1, returnTestDF = TRUE)
  
  iTest <- test3$tipoOutlier %in% tiposOutliersValoresSospechosos & 
    test3$tipoOutlier == test4$tipoOutlier & 
    (test3$estimado + test4$estimado)*0.5 > 10
  # test3[iTest, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test3[iTest, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/3/', shpBase = shpBase, replot=replot)
  }
  
  test3[iTest, ]$reemplazar <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test3, valoresObservaciones)
  
  test5 <- deteccionOutliersMediaSD(x = valoresObservaciones, factorSDHaciaAbajo = 3, sdMin = 1)
  # test5[test3$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  
  iTest <- test5$tipoOutlier %in% tiposOutliersValoresSospechosos & 
           test5$estimado >= 3 & 
           test5$estimado <= 15
  # test5[iTest, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test5[iTest, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/4/', shpBase = shpBase, replot=replot)
  }
  
  test5[iTest, ]$reemplazar <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test5, valoresObservaciones)
  
  test6 <- testMaxToMeanRatios(valoresObservaciones)
  test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = 'Resultados/2-QC/mapas/Pluviómetros/5/', shpBase = shpBase, replot=replot)
  }
  
  test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ]$reemplazar <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test6, valoresObservaciones)
  
  return(valoresObservaciones)
}