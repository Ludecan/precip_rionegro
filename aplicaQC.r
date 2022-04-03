source(paste0(pathSTInterp, 'qc/qcTests.r'), encoding='WINDOWS-1252')
source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding='WINDOWS-1252')

paramsInterpolacionSetDeReferencia <- createParamsInterpolarYMapear(
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
  modelosVariograma=c('Exp', 'Sph', 'Pow'),
  #cutoff=quantile(spDists(coordsObservaciones, longlat=!is.projected(coordsObservaciones)), probs=0.4),
  cutoff=Inf,
  tlags=0:5, 
  nTsST=0,#max(tlags), 
  tlagsAR=NULL,
  tryFixNugget=FALSE, 
  nPuntosIniciales=2,
  usarFitVariogramGLS=TRUE,
  modelosVariogramaST=c('Separable', 'ProductSum', 'Metric', 'SimpleSumMetric'),
  fit.methodST=6,
  verbose=FALSE,
  pathSHPMapaBase='',
  nCoresAUsar=0, 
  radioReduccionSeriesKm=0,
  funcionReduccionSeries='mean',
  difMaxFiltradoDeOutliersRLM=0,
  difMaxFiltradoDeOutliersCV=0,
  modoDiagnostico=FALSE,
  simpleKrigingEnRK=FALSE,
  preECDFMatching=FALSE
)

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
  difMaxFiltradoDeOutliersRLM=0,
  difMaxFiltradoDeOutliersCV=0,
  modoDiagnostico=FALSE
)

applyQCTests <- function(
  coordsObservaciones, fechasObservaciones, valoresObservaciones, paramsInterpolacion, 
  pathsRegresores, plotMaps=FALSE, pathResultadosQC='Resultados/2-QC/'
) {
  replot <- FALSE
  # replot <- TRUE
  # plotMaps <- TRUE
  # paramsInterpolacion <- paramsInterpolacionQCTests
  regresorCombinado <- getRegresorCombinado(
    coordsObservaciones=coordsObservaciones[iEstacionesDeReferencia, , drop=F],
    valoresObservaciones=valoresObservaciones[, iEstacionesDeReferencia, drop=F], 
    pathsRegresores=pathsRegresores,
    logTransforms=FALSE
  )
  
  print(paste0(Sys.time(), ' - Estimando valores del set de no referencia...'))
  listaMapas <- createDefaultListaMapas(
    paramsIyM=paramsInterpolacionSetDeReferencia, fechasObservaciones=fechasObservaciones, 
    dibujarEscalaFija=F, salvarGeoTiff=F, recalcularSiYaExiste=T
  )
  
  estimacionSetNoReferencia <- interpolarYMapear(
    coordsObservaciones=coordsObservaciones[iEstacionesDeReferencia, , drop=F], 
    fechasObservaciones=fechasObservaciones, 
    valoresObservaciones=valoresObservaciones[, iEstacionesDeReferencia, drop=F],
    pathsRegresores=regresorCombinado, 
    coordsAInterpolar=coordsObservaciones[iEstacionesNoReferencia, , drop=F], 
    paramsIyM=paramsInterpolacionSetDeReferencia,
    shpMask=NULL,
    xyLims=xyLims,
    listaMapas=listaMapas,
    returnInterpolacion=1L
  )
  colnames(estimacionSetNoReferencia) <- colnames(valoresObservaciones)[iEstacionesNoReferencia]
  
  print(paste0(Sys.time(), ' - Calculando diferencias estandarizadas...'))
  difs <- valoresObservaciones[, iEstacionesNoReferencia, drop=F] - estimacionSetNoReferencia
  
  #which(rownames(valoresObservaciones) == '2017-10-11')
  #which(colnames(valoresObservaciones[, iEstacionesNoReferencia]) == 'VILLA.SORIANO.RHT')
  
  means <- rowMeans(difs, na.rm=T)
  sds <- rowSds(difs, na.rm=T)
  sds[rowSds(valoresObservaciones[, iEstacionesDeReferencia, drop=F], na.rm=T) < 0.2 | sds < 2] <- Inf
  stdDifs <- (difs - means) / sds
  
  test0 <- createDFTestsConEstimadosYStdDifsUmbralValor(
    x=valoresObservaciones[, iEstacionesNoReferencia, drop=F],
    estimados=estimacionSetNoReferencia,
    stdDifs=stdDifs,
    factorHaciaAbajo=1.5,
    factorHaciaArriba=5.4,
    umbralValor=10,
    factorHaciaAbajoSiMayorAUmbralValor=4,
    factorHaciaArribaSiMayorAUmbralValor=7
  )
  test0 <- test0[!is.na(test0$valor), ]
  
  # test0[test0$fecha == '2017-02-03',]
  # test0[test0$estacion == 'VILLA.SORIANO.RHT' & test0$fecha == '2017-02-12', ]
  # test0$tipoOutlier[test0$valor < 0.2 & test0$estimado > 1] <- TTO_CeroPocoProbable
  
  # mads[as.character(fechasObservaciones) == '2017-02-17']
  
  if (plotMaps) {
    tamaniosPuntos <- rep(5, nrow(coordsObservaciones))
    tamaniosPuntos[iEstacionesNoReferencia] <- 2
    
    mapearResultadosDeteccionOutliersV2(
      test=test0[test0$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
      coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
      tiposOutliersDeInteres=tiposOutliersValoresSospechosos, tamaniosPuntos=tamaniosPuntos,
      tamanioResalto=0.5, carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/0/'), 
      shpBase=shpBase, replot=replot)
  }
  test0$reemplazar[test0$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test0, valoresObservaciones)

  if (FALSE) {
    print(paste0(Sys.time(), ' - Estimando valores mediante universalGriddingCV...'))
    estimacionValores <- universalGriddingCV(
      coordsObservaciones=coordsObservaciones,
      fechasObservaciones=fechasObservaciones[1:10],
      valoresObservaciones=valoresObservaciones[1:10, ], 
      params=paramsInterpolacionSetDeReferencia,
      pathsRegresores=regresorCombinado,
      estimarNAs=FALSE
    )
    print(paste0(Sys.time(), ' - Finalizado estimando valores mediante universalGriddingCV...'))
  }
  
  print(paste0(Sys.time(), ' - Ejecutando Test Espacial de Precipitación. Pasada 1...'))
  # Two rounds of QC tests
  test1 <- testEspacialPrecipitacion(
    coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones,
    valoresObservaciones=valoresObservaciones, ispMax=0.3, ispObs=8, 
    isdMin=1, isdObs=0.3, isdEstMin=3, fInf=1, fSup=3.8, amplitudMin=1, 
    amplitudMinRatio=NA, minValAbs=0, maxValAbs=450, maxDistKm=50, 
    minNCuadrantes=4L, minNVecinosPorCuadrante=1L
  )
  
  # test1[test1$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  # test1[test1$fecha == '2019-01-08',]
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test=test1[test1$tipoOutlier %in% tiposOutliersValoresSospechosos,], 
      coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
      tiposOutliersDeInteres=tiposOutliersValoresSospechosos,
      carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/1/'), shpBase=shpBase, 
      replot=replot)
  }
  
  test1$reemplazar[test1$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test1, valoresObservaciones)
  
  print(paste0(Sys.time(), ' - Ejecutando Test Espacial de Precipitación. Pasada 2...'))
  test2 <- testEspacialPrecipitacion(
    coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones,
    valoresObservaciones=valoresObservaciones,ispMax=0.3, ispObs=8, 
    isdMin=1, isdObs=0.3, isdEstMin=3, fInf=1.4, fSup=5, amplitudMin=1, 
    amplitudMinRatio=0.15, minValAbs=0, maxValAbs=450, maxDistKm=50, 
    minNCuadrantes=3L, minNVecinosPorCuadrante=1L)
  
  # test2[test2$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  # test2[test2$fecha == '2019-01-08',]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test=test2[test2$tipoOutlier %in% tiposOutliersValoresSospechosos, ], 
      coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
      tiposOutliersDeInteres=tiposOutliersValoresSospechosos,
      carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/2/'), shpBase=shpBase, 
      replot=replot)
  }
  
  test2$reemplazar[test2$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test2, valoresObservaciones)
  
  # Plus another round of testing with the satellites. If for a given observation both satellites
  # agree it's an extreme value, we discard it
  listaMapas <- createDefaultListaMapas(
    paramsInterpolacion, fechasObservaciones=fechasObservaciones, dibujarEscalaFija=FALSE)
  
  requiredCols <- c('IMERG_V06B', 'GSMaP_v7')
  if (all(requiredCols %in% colnames(pathsRegresores))) {
    print(paste0(Sys.time(), ' - Ejecutando Detección Outliers RLM contra IMERG...'))
    
    test3 <- deteccionOutliersRLM(
      coordsObservaciones, fechasObservaciones, valoresObservaciones, params=paramsInterpolacion, 
      pathsRegresores=pathsRegresores[, 'IMERG_V06B', drop=F], listaMapas=listaMapas, 
      factorMADHaciaAbajo=NA, factorSDHaciaAbajo=5.6, factorSDHaciaArriba=5.6, sdMin=1, 
      returnTestDF=TRUE
    )
    
    # test3[test3$fecha == '2017-04-10',]
    # plot(test3[test3$fecha == '2017-04-10','valor'], test3[test3$fecha == '2017-04-10','estimado'])
    
    print(paste0(Sys.time(), ' - Ejecutando Detección Outliers RLM contra GSMaP..'))
    test4 <- deteccionOutliersRLM(
      coordsObservaciones, fechasObservaciones, valoresObservaciones, params=paramsInterpolacion, 
      pathsRegresores=pathsRegresores[, 'GSMaP_v7', drop=F], listaMapas=listaMapas, 
      factorMADHaciaAbajo=NA, factorSDHaciaAbajo=5.6, factorSDHaciaArriba=5.6, sdMin=1, 
      returnTestDF=TRUE
    )
    
    #corrs <- getCorrs(valoresObservaciones, pathsRegresores)
    
    iTest <- test3$tipoOutlier %in% tiposOutliersValoresSospechosos & 
      test3$tipoOutlier == test4$tipoOutlier & 
      (test3$estimado + test4$estimado) * 0.5 > 10
    # test3[iTest, ]
    
    if (plotMaps) {
      mapearResultadosDeteccionOutliersV2(
        test=test3[iTest, ], 
        coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
        tiposOutliersDeInteres=tiposOutliersValoresSospechosos,
        carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/3/'), shpBase=shpBase, 
        replot=replot)
    }
    
    test3$reemplazar[iTest] <- 1
    valoresObservaciones <- ejecutarReemplazosSRT(test3, valoresObservaciones)    
  } else {
    writeLines(paste0(
      Sys.time(), 
      ' - No se encontró el valor de los siguientes regresores satelitales:\n ',
      requiredCols[!requiredCols %in% colnames(pathsRegresores)],
      '\nSalteando control de calidad en base a satélites.'
    ))
  }

  
  print(paste0(Sys.time(), ' - Ejecutando Detección Outliers Media/Desv. Est..'))
  test5 <- deteccionOutliersMediaSD(x=valoresObservaciones, factorSDHaciaAbajo=7, sdMin=1)
  # test5[test3$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  
  iTest <- test5$tipoOutlier %in% tiposOutliersValoresSospechosos & 
           test5$estimado >= 3 & 
           test5$estimado <= 15
  # test5[iTest, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test=test5[iTest, ], 
      coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
      tiposOutliersDeInteres=tiposOutliersValoresSospechosos,
      carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/4/'), shpBase=shpBase, 
      replot=replot)
  }
  
  test5$reemplazar[iTest] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test5, valoresObservaciones)
  
  print(paste0(Sys.time(), ' - Ejecutando Detección Outliers Max to Mean Ratios.'))
  test6 <- testMaxToMeanRatios(valoresObservaciones, maxRatio=200)
  # test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ]
  
  if (plotMaps) {
    mapearResultadosDeteccionOutliersV2(
      test=test6[test6$tipoOutlier %in% tiposOutliersValoresSospechosos, ], 
      coordsObservaciones=coordsObservaciones, valoresObservaciones=valoresObservaciones,
      tiposOutliersDeInteres=tiposOutliersValoresSospechosos,
      carpetaSalida=paste0(pathResultadosQC, 'mapas/Pluviómetros/5/'), shpBase=shpBase, 
      replot=replot)
  }
  
  test6$reemplazar[test6$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
  valoresObservaciones <- ejecutarReemplazosSRT(test6, valoresObservaciones)
  
  #lala <- deteccionOutliersUniversalGriddingCV(
  #  coordsObservaciones, fechasObservaciones, valoresObservaciones, params=paramsInterpolacion,
  #  pathsRegresores=pathsRegresores[, 'GSMaP_v7', drop=F], maxOutlyingness=3.5, maxNIters=5)
                                       
  print(paste0(Sys.time(), ' - QC Finalizado.'))
  return(valoresObservaciones)
}
