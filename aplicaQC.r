source(paste0(pathSTInterp, 'qc/qcTests.r'), encoding = 'WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding = 'WINDOWS-1252')

paramsInterpolacionQCTests <- createParamsInterpolarYMapear(
  proj4StringObservaciones=wkt(coordsObservaciones),
  proj4StringAInterpolar=wkt(coordsObservaciones),
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
  pathSHPMapaBase=paste0(pathDatos, 'MapaUruguay/uruguay_departamentos.shp'),
  nCoresAUsar=0, 
  radioReduccionSeriesKm=0,
  funcionReduccionSeries='mean',
  difMaxFiltradoDeOutliersRLM = 0,
  difMaxFiltradoDeOutliersCV = 0,
  modoDiagnostico=TRUE)

applyQCTests <- function(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, paramsInterpolacion, 
    pathsRegresores, plotMaps=FALSE, pathResultadosQC='Resultados/2-QC/') {
  replot <- FALSE
  
  print(paste0(Sys.time(), ' - Ejecutando Test Espacial de Precipitaci�n. Pasada 1...'))
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
      carpetaSalida = paste0(pathResultadosQC, 'mapas/Pluvi�metros/1/'), shpBase = shpBase, 
      replot=replot)
  }
  
  test$reemplazar[test$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)
  
  print(paste0(Sys.time(), ' - Ejecutando Test Espacial de Precipitaci�n. Pasada 2...'))
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
      carpetaSalida = paste0(pathResultadosQC, 'mapas/Pluvi�metros/2/'), shpBase = shpBase, 
      replot=replot)
  }
  
  test2$reemplazar[test2$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test2, valoresObservaciones)
  
  # Plus another round of testing with the satellites. If for a given observation both satellites
  # agree it's an extreme value, we discard it
  listaMapas <- createDefaultListaMapas(
    paramsInterpolacion, fechasObservaciones = fechasObservaciones, dibujarEscalaFija = FALSE)
  
  print(paste0(Sys.time(), ' - Ejecutando Detecci�n Outliers RLM contra IMERG...'))
  
  test3 <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params=paramsInterpolacion, 
    pathsRegresores=pathsRegresores[, 'IMERG_V06B', drop=F], listaMapas=listaMapas, 
    factorMADHaciaAbajo=NA, factorSDHaciaAbajo=2.2, sdMin=1, returnTestDF=TRUE
  )
  
  # test3[test3$fecha == '2019-01-08',]
  
  print(paste0(Sys.time(), ' - Ejecutando Detecci�n Outliers RLM contra GSMaP..'))
  test4 <- deteccionOutliersRLM(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion, 
    pathsRegresores = pathsRegresores[, 'GSMaP_v7', drop=F], listaMapas = listaMapas, 
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
      carpetaSalida = paste0(pathResultadosQC, 'mapas/Pluvi�metros/3/'), shpBase = shpBase, 
      replot=replot)
  }
  
  test3$reemplazar[iTest] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test3, valoresObservaciones)
  
  print(paste0(Sys.time(), ' - Ejecutando Detecci�n Outliers Media/Desv. Est..'))
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
      carpetaSalida = paste0(pathResultadosQC, 'mapas/Pluvi�metros/4/'), shpBase = shpBase, 
      replot=replot)
  }
  
  test5$reemplazar[iTest] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test5, valoresObservaciones)
  
  print(paste0(Sys.time(), ' - Ejecutando Detecci�n Outliers Max to Mean Ratios.'))
  test6 <- testMaxToMeanRatios(valoresObservaciones)
  test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test = test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ], 
      coordsObservaciones = coordsObservaciones, valoresObservaciones = valoresObservaciones,
      tiposOutliersDeInteres = tiposOutliersValoresSospechosos,
      carpetaSalida = paste0(pathResultadosQC, 'mapas/Pluvi�metros/5/'), shpBase = shpBase, 
      replot=replot)
  }
  
  test6$reemplazar[test6$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test6, valoresObservaciones)
  
  #lala <- deteccionOutliersUniversalGriddingCV(
  #  coordsObservaciones, fechasObservaciones, valoresObservaciones, params = paramsInterpolacion,
  #  pathsRegresores = pathsRegresores[, 'GSMaP_v7', drop=F], maxOutlyingness = 3.5, maxNIters = 5)
                                       
  print(paste0(Sys.time(), ' - QC Finalizado.'))
  return(valoresObservaciones)
}
