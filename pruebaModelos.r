if (dir.exists('F:/ADME/precip_rionegro')) { setwd('F:/ADME/precip_rionegro')
} else if (dir.exists('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')) { setwd('/media/palfaro/Seagate Backup Plus Drive/ADME/precip_rionegro')
} else if (dir.exists('D:/ADME/precip_rionegro')) { setwd('D:/ADME/precip_rionegro') }
# Linux installations need to run these to have rgdal available
# sudo apt-get update
# sudo apt-get install libgdal-dev libproj-dev
# and this for Cairo
# sudo apt-get install libcairo2-dev libgtk2.0-dev xvfb xauth xfonts-base libxt-dev
# and this for devtools
# sudo apt-get install libcurl4-gnutls-dev libssl-dev libxml2-dev libxslt-dev libcurl4-openssl-dev

# Índice
# 5 - Preparación de Parámetros
# 6 - Interpolación de los datos
# 7 - Cross Validation

#dt_ini <- '2009-09-16'
dt_ini <- '2017-02-01'
dt_fin <- '2020-03-01'
#dt_ini <- '2018-10-21'
#dt_fin <- '2019-12-07'
#dt_ini <- '2020-03-01'
#dt_fin <- '2020-03-05'
estacionesADescartar <- c(
  'ANSINA.Paso.BORRACHO.RHT', 'PASO.MAZANGANO.RHT', 'PASO.LAGUNA.I.RHT', 'PASO.AGUIAR.RHT',
  'PASO.PEREIRA.RHT', 'PASO.NOVILLOS.RHT', 'VILLA.SORIANO.RHT')
#estacionesADescartar <- NA
horaUTCInicioAcumulacion <- 10
horaLocalInicioAcumulacion <- horaUTCInicioAcumulacion - 3
forzarReDescarga <- FALSE
borrarDatosOriginales <- FALSE
plotDatos <- FALSE

runTestsRegresores <- TRUE
runGridding <- TRUE
runCV <- TRUE
runValidation <- TRUE
runPlots <- TRUE
runVerif <- TRUE

postFijoPluvios <- '_seleccionPluvios'
#postFijoPluvios <- ''
nombreExperimento <- paste0('_mascara03_CorreccionExtrapolacion_sateliteEnMascara_nuevaCuenca_v2', 
                            postFijoPluvios)

source('cargaDatos.r', encoding = 'WINDOWS-1252')

localFileNonQCed <- changeFileExt(
  appendToFileName(localFile, paste0('_non_qced', postFijoPluvios)), '.tsv')
grabarSeriesArchivoUnico(
  pathArchivoDatos = localFileNonQCed, estaciones=estaciones, fechas = fechasObservaciones, 
  datos = valoresObservaciones)

localFileQCed <- changeFileExt(
  appendToFileName(localFile, paste0('_qced', postFijoPluvios)), '.tsv')
if (!file.exists(localFileQCed) || file.info(localFileQCed)$size <= 0) {
  source('aplicaQC.r', encoding = 'WINDOWS-1252')
  valoresObservaciones <- applyQCTests(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, 
    paramsInterpolacion = paramsInterpolacionQCTests, pathsRegresores = pathsRegresores, 
    plotMaps = TRUE, pathResultadosQC=paste0('Resultados/2-QC', nombreExperimento, '/'))
  
  grabarSeriesArchivoUnico(
    pathArchivoDatos = localFileQCed, estaciones=estaciones, fechas = fechasObservaciones, 
    datos = valoresObservaciones)
} else {
  datos <- leerSeriesArchivoUnico(
    pathArchivoDatos = localFileQCed, nFilasEstaciones = 4, filaId = 2, fileEncoding = 'WINDOWS-1252')
  valoresObservaciones <- datos$datos
  rm(datos)
}

if (plotDatos) {
  source('graficosParticulares.r', encoding = 'WINDOWS-1252')
  source(paste0(pathSTInterp, 'interpolar/leerEscalas.r'), encoding = 'WINDOWS-1252')
  especificacionEscala <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(
    nDigitos = 1, continuo = T)
  plotObservacionesYRegresores(
    coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones, 
    valoresObservaciones=valoresObservaciones, shpBase=shpBase, replot = TRUE,
    grillaAlternativaRegresores=coordsAInterpolar, especificacionEscala=especificacionEscala)
}

source(paste0(pathSTInterp, 'interpolar/interpolarYMapearEx.r'), encoding = 'WINDOWS-1252')
# pathsRegresores <- pathsRegresores[, -3]


if (FALSE) {
  cuts <- cut(x = valoresObservaciones, breaks=c(0, 0.1, 2, 5, 10, 15, 25, 50, 100, 500), include.lowest=T, ordered_result=T)
  uniqueCuts <- levels(cuts)
  valoresRegresores <- extraerValoresRegresoresSobreSP(
    objSP = coordsObservaciones, pathsRegresores = pathsRegresores)
  sapply(uniqueCuts, FUN = function(x) {
    idx = cuts == x
    return(cor(valoresObservaciones[idx], valoresRegresores[['GPM']][idx], use = "pairwise.complete.obs"))
  })
}

corrs <- getCorrs(valoresObservaciones, pathsRegresores, logTransforms = FALSE)

dfCorrs <- data.frame(satelite=c(rep('GPM', nrow(pathsRegresores)), rep('GSMaP', nrow(pathsRegresores))), 
                      fecha=as.Date(rep(fechasObservaciones, 2)),
                      corr=c(corrs[, 1], corrs[, 2]))

p <- ggplot(data=dfCorrs, aes(x=fecha, y=corr, colour=satelite, group=satelite)) + 
  geom_line() + geom_point() +
  scale_x_date(date_breaks = "1 month") +
  theme(panel.background=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title='Correlación Diaria', x='Fecha', y='Correlación')

ggsave(filename = 'Resultados/1-Exploracion/CorrelacionDiaria.png', plot = p)


corr_thresholds <- c(0.5, 0.6, 0.7, 0.8)
for (i in seq.int(0, length(corr_thresholds))) {
  pathsRegresores <- cbind(pathsRegresores, rep(NA_character_, nrow(pathsRegresores)))
}
idx <- (ncol(pathsRegresores) - (length(corr_thresholds))):ncol(pathsRegresores)
colnames(pathsRegresores)[idx] <- c('Combinado', paste0('Combinado', corr_thresholds))
# pathsRegresores[, 'Combinado0.6'] <- NA_character_

iRow <- 2
for (iRow in 1:nrow(pathsRegresores)) {
  idx <- which.max(corrs[iRow, ])
  if (length(idx) > 0) {
    corr <- corrs[iRow, idx]
    if (!is.na(corr)) {
      if (corr >= 0.8) {
        pathsRegresores[iRow, 'Combinado0.5'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.6'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.7'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.8'] <- pathsRegresores[iRow, idx]
      } else if (corr >= 0.7) {
        pathsRegresores[iRow, 'Combinado0.5'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.6'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.7'] <- pathsRegresores[iRow, idx]
      } else if (corr >= 0.6) {
        pathsRegresores[iRow, 'Combinado0.5'] <- pathsRegresores[iRow, idx]
        pathsRegresores[iRow, 'Combinado0.6'] <- pathsRegresores[iRow, idx]
      } else if (corr >= 0.5) {
        pathsRegresores[iRow, 'Combinado0.5'] <- pathsRegresores[iRow, idx]
      }
      pathsRegresores[iRow, 'Combinado'] <- pathsRegresores[iRow, idx]
    }
  } else {
    pathsRegresores[iRow, 'Combinado'] <- pathsRegresores[iRow, 1]
  }
}

sum(!is.na(pathsRegresores[, 'Combinado']))
sum(!is.na(pathsRegresores[, 'Combinado0.5']))
sum(!is.na(pathsRegresores[, 'Combinado0.6']))
sum(!is.na(pathsRegresores[, 'Combinado0.7']))
sum(!is.na(pathsRegresores[, 'Combinado0.8']))

paramsBase <- createParamsInterpolarYMapear(
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
paramsBase$especEscalaDiagnostico <- crearEspecificacionEscalaRelativaAlMinimoYMaximoDistinguir0(nDigitos = 2, continuo = T)

# escala <- darEscala(especificacion = paramsBase$especEscalaDiagnostico, valores = c(0, 1, 10))

if (FALSE) {
  for (i in seq.int(0, ncol(pathsRegresores))) {
    regresor <- colnames(pathsRegresores)[i]
    pathsRegresor <- pathsRegresores[, i]
    
    fechas <- names(pathsRegresor)
    claseFechaI <- yday(fechas)
    claseFechaI[claseFechaI == 366] <- 365
    clases=sort(unique(claseFechaI))
    pathsClimMean <- paste(pathDatos, sprintf('satelites/%s_Clim_mean/%03d.tif', regresor, clases), sep='')
    minNfechasParaAgregar <- 5
    funcionAgregacion=base::mean
    interpolarFaltantes='idw'
    overlap <- 5
    pathShpMask=NULL
    proj4stringShpMask=NULL
    spSinMascara <- shpMask$shp
    recalcularSiYaExiste = F
    
    agregacionTemporalGrillada3(
      fechas = fechas, pathsRegresor = pathsRegresor, claseFechaI = claseFechaI, clases=clases, 
      nomArchivosSalidaClaseI = pathsClimMean, minNfechasParaAgregar = minNfechasParaAgregar, 
      funcionAgregacion = funcionAgregacion, interpolarFaltantes = interpolarFaltantes, 
      overlap = overlap, pathShpMask = pathShpMask, proj4stringShpMask = proj4stringShpMask, 
      spSinMascara = spSinMascara, recalcularSiYaExiste = recalcularSiYaExiste)
    
    funcionAgregacion=stats::median
    pathsClimMedian <- paste(pathDatos, sprintf('satelites/%s_Clim_median/%03d.tif', regresor, clases), sep='')
    
    agregacionTemporalGrillada3(
      fechas = fechasObservaciones, pathsRegresor = pathsRegresor, claseFechaI = claseFechaI, 
      clases=clases, nomArchivosSalidaClaseI = pathsClimMedian, 
      minNfechasParaAgregar = minNfechasParaAgregar, funcionAgregacion = funcionAgregacion, 
      interpolarFaltantes = interpolarFaltantes, overlap = overlap, pathShpMask = pathShpMask, 
      proj4stringShpMask = proj4stringShpMask, spSinMascara = spSinMascara, 
      recalcularSiYaExiste = recalcularSiYaExiste)
    
    funcionAgregacion=stats::sd
    pathsClimSD <- paste(pathDatos, sprintf('satelites/%s_Clim_sd/%03d.tif', regresor, clases), sep='')
    
    agregacionTemporalGrillada3(
      fechas = fechasObservaciones, pathsRegresor = pathsRegresor, claseFechaI = claseFechaI, 
      clases=clases, nomArchivosSalidaClaseI = pathsClimSD, 
      minNfechasParaAgregar = minNfechasParaAgregar, funcionAgregacion = funcionAgregacion, 
      interpolarFaltantes = interpolarFaltantes, overlap = overlap, pathShpMask = pathShpMask, 
      proj4stringShpMask = proj4stringShpMask, spSinMascara = spSinMascara, 
      recalcularSiYaExiste = recalcularSiYaExiste)
    
    iDias <- 1:365
    r1 <- range(extraerRangoSPDataFrames(pathsClimMean))
    r2 <- range(extraerRangoSPDataFrames(pathsClimMedian))
    r3 <- range(extraerRangoSPDataFrames(pathsClimSD))
    
    escalaCentral <- crearEscalaEquiespaciadaDistinguirMenoresOIgualesAUmbral(
      datos = c(r1, r2), umbral = 0, brewerPal = 'Blues', nIntervalos = 9, continuo = T)
    escalaSD <- crearEscalaEquiespaciada(r3, brewerPal = 'Reds', nIntervalos = 8, continuo = T)
    
    pathsRasters <- cbind(pathsClimMean, pathsClimMedian, pathsClimSD)
    colnames(pathsRasters) <- c('Media', 'Mediana', 'Desviación Estandar')
    
    escalas <- list()
    escalas[[1]] <- escalaCentral
    escalas[[2]] <- escalaCentral
    escalas[[3]] <- escalaSD
    
    plotMultiRastersEnPaneles(
      pathsRasters = pathsRasters, fechasRasters = sprintf('%03d', iDias), 
      carpetaSalida = sprintf('Resultados/1-Exploracion/Climatologias/%s/', regresor), 
      postFijoNomArchSalida = '', shpBase = shpMask$shp, escalas = escalas, nCols = 3, byRow = T, 
      replot = T, heightPx = 620, alturaEscalaContinua = unit(x=1, units = 'in'))
  }
}


{
  listaParams <- list()
  listaRegresores <- list()
  
  # 1 - Kriging Ordinario Espacial
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'ninguna'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[1]] <- paramsI
  listaRegresores[[1]] <- NA
  
  # 2 - GPM sin calibrar
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'NoLimitar'
  paramsI$interpolationMethod <- 'none'
  paramsI$metodoIgualacionDistribuciones <- 'ninguna'
  paramsI$umbralMascaraCeros <- 0
  paramsI$metodoRemocionDeSesgo <- 'ninguno'
  listaParams[[2]] <- paramsI
  listaRegresores[[2]] <- pathsRegresores[,c('GPM'), drop=FALSE]
  
  # 3 - GSMaP sin calibrar
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'NoLimitar'
  paramsI$interpolationMethod <- 'none'
  paramsI$metodoIgualacionDistribuciones <- 'ninguna'
  paramsI$umbralMascaraCeros <- 0
  paramsI$metodoRemocionDeSesgo <- 'ninguno'
  listaParams[[3]] <- paramsI
  listaRegresores[[3]] <- pathsRegresores[,c('GSMaP'), drop=FALSE]
  
  # 4 - Kriging Universal Espacial + Regresion Generalizada en GPM
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[4]] <- paramsI
  listaRegresores[[4]] <- pathsRegresores[, c('GPM'), drop=FALSE]
  paramsI$signosValidosRegresores <- 1
  names(paramsI$signosValidosRegresores) <- colnames(listaRegresores[[12]])  
  
  # 5 - Kriging Universal Espacial + Regresion Generalizada en GSMaP
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[5]] <- paramsI
  listaRegresores[[5]] <- pathsRegresores[,c('GSMaP'), drop=FALSE]
  paramsI$signosValidosRegresores <- 1
  names(paramsI$signosValidosRegresores) <- colnames(listaRegresores[[12]])  
  
  # 6 - Kriging Universal Espacial + Regresion Generalizada en GPM y GSMaP
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[6]] <- paramsI
  listaRegresores[[6]] <- pathsRegresores[,c('GPM', 'GSMaP'), drop=FALSE]
  
  # 7 - Kriging Universal Espacial + Regresion Generalizada en Combinado
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[7]] <- paramsI
  listaRegresores[[7]] <- pathsRegresores[, 'Combinado', drop=FALSE]
  paramsI$signosValidosRegresores <- 1
  names(paramsI$signosValidosRegresores) <- colnames(listaRegresores[[7]])
  
  # 8 - Kriging Universal Espacial + Regresion Generalizada en Combinado0.5
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[8]] <- paramsI
  listaRegresores[[8]] <- pathsRegresores[, 'Combinado0.5', drop=FALSE]
  
  # 9 - Kriging Universal Espacial + Regresion Generalizada en Combinado0.6
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[9]] <- paramsI
  listaRegresores[[9]] <- pathsRegresores[, 'Combinado0.6', drop=FALSE]
  
  # 10 - Kriging Universal Espacial + Regresion Generalizada en Combinado0.7
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[10]] <- paramsI
  listaRegresores[[10]] <- pathsRegresores[, 'Combinado0.7', drop=FALSE]
  
  # 11 - Kriging Universal Espacial + Regresion Generalizada en Combinado0.8
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  listaParams[[11]] <- paramsI
  listaRegresores[[11]] <- pathsRegresores[, 'Combinado0.8', drop=FALSE]
  
  # 12 - Regresion Generalizada en Combinado
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'none'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'ninguno'
  listaParams[[12]] <- paramsI
  listaRegresores[[12]] <- pathsRegresores[, 'Combinado', drop=FALSE]
  paramsI$signosValidosRegresores <- 1
  names(paramsI$signosValidosRegresores) <- colnames(listaRegresores[[12]])
  
  
  # 13 - Kriging Ordinario Espacial sin máscara
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'ninguna'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  paramsI$umbralMascaraCeros <- 0
  listaParams[[13]] <- paramsI
  listaRegresores[[13]] <- NA
  
  # 14 - Kriging Universal Espacial + Regresion Generalizada en Combinado
  paramsI <- paramsBase
  paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
  paramsI$interpolationMethod <- 'automap'
  paramsI$metodoIgualacionDistribuciones <- 'GLS'
  paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
  paramsI$umbralMascaraCeros <- 0
  listaParams[[14]] <- paramsI
  listaRegresores[[14]] <- pathsRegresores[, 'Combinado', drop=FALSE]
  paramsI$signosValidosRegresores <- 1
  names(paramsI$signosValidosRegresores) <- colnames(listaRegresores[[7]])
}

modelosACorrer <- 1:length(listaParams)
modelosACorrer <- c('K', 'GPM', 'GSMaP', 'GR-Combinado', 'GRK-Combinado')
modelosACorrer <- c(1, 2, 3, 12, 7)

source(paste0(pathSTInterp, 'interpolar/testInterpolationModels.r'), encoding = 'WINDOWS-1252')

############# Tests Regresores #############
if (runTestsRegresores) {
  testRegressors(valoresObservaciones = valoresObservaciones, pathsRegresores = pathsRegresores, 
                 pathSHPNotNUll=pathSHPMapaBase, pathResultados='Resultados/1-Exploracion/', 
                 seriesName='Rainfall', outputTableFilename='testRegresores.csv')
}

############# Gridding #############
pathResultadosGrillado <- paste0('Resultados/3-Grillado', nombreExperimento, '/')
if (runGridding) {
  i <- 7
  i <- 14
  for (i in modelosACorrer) {
    try({
      paramsI <- listaParams[[i]]
      if (is.na(listaRegresores[i])) { pr <- NULL 
      } else { pr <- listaRegresores[[i]] }
      
      nomModelo <- nombreModelo(params = paramsI, pathsRegresores=pr)
      print(paste(Sys.time(), ': Gridding ', nomModelo, sep=''))
      
      # paramsI$nCoresAUsar <- 1
      listaMapas <- createDefaultListaMapas(
        paramsI, fechasObservaciones, dibujarEscalaFija = FALSE, salvarGeoTiff = TRUE, 
        recalcularSiYaExiste = FALSE, incluirSubtitulo = FALSE)
      
      nomModeloFormateadoParaArchivos <- gsub(pattern = '*', replacement = '', x = nomModelo, fixed = T)
      pathModelo <- paste0(pathResultadosGrillado, nomModeloFormateadoParaArchivos, '/')
      dir.create(pathModelo, showWarnings = F, recursive = T)      
      listaMapas$nombreArchivo <- 
        paste0(pathModelo, 
               appendToFileName(filename=listaMapas$nombreArchivo, 
                                postFijo=paste('_', nomModeloFormateadoParaArchivos, sep='')))
      
      #pathsRegresores = pr
      #paramsIyM = paramsI
      #paramsParaRellenoRegresores = NULL
      #pathsRegresoresParaRellenoRegresores = NULL
      #returnInterpolacion <- FALSE
      #tsAInterpolar <- 1:nrow(valoresObservaciones)
      #tsAInterpolar <- 48
      #tsAInterpolar <- which(fechasObservaciones == as.POSIXct('2014-01-31', tz=tz(fechasObservaciones[1])))
      tsAInterpolar <- 1:nrow(valoresObservaciones)
      tsAInterpolar <- which(fechasObservaciones == as.POSIXct('2019-12-27', tz=tz(fechasObservaciones[1])))
      interpolarYMapear(
        coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones, 
        valoresObservaciones = valoresObservaciones, pathsRegresores = pr, 
        coordsAInterpolar = coordsAInterpolar, paramsIyM = paramsI, shpMask = shpMask, 
        xyLims = xyLims, listaMapas=listaMapas, returnInterpolacion = F, 
        paramsParaRellenoRegresores = NULL, pathsRegresoresParaRellenoRegresores = NULL, 
        tsAInterpolar=tsAInterpolar)
    })
  }
}

############# Cross Validation #############
if (runCV) {
  # La función universalGriddingCV retorna una matriz de las mismas dimensiones que 
  # valoresObservaciones, con cv[i, j] el valor de la LOOCV de la estacion j en la fecha i. 
  # Es decir el valor de cv[i, j] es la estimación LOOCV de valoresObservaciones[i, j]
  pathResultadosValidacion <- paste0('Resultados/4-Validacion', nombreExperimento, '/')
  cvs <- st_interpCrossValidations(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, listaParams, listaRegresores,
    pathResultados=pathResultadosValidacion, modelosACorrer=modelosACorrer, recalcCV=FALSE)
  
  ############# Validation Stats #############
  if (runValidation) {
    if (FALSE) {
      idx <- !is.na(pathsRegresores[, 'Combinado0.6'])
      fObs <- fechasObservaciones[idx]
      vObs <- valoresObservaciones[idx, ]
      cvs_idx <- sapply(cvs, FUN = function(x) { return(x[idx,])})
    } else {
      fObs <- fechasObservaciones
      vObs <- valoresObservaciones
      cvs_idx <- cvs
    }
    
    validationStats <- calcValidationStatisticsMultipleModels(
      vObs, cvs_idx, climatologias=NULL, pathResultados='Resultados/4-Validacion/')
    
    validationStats$validationStatsOverall
    
    # validationStats$validationStatsTemporales
    rmses <- sapply(validationStats$validationStatsTemporales, FUN = function(x) {return(x[, 'RMSE'])} )
    
    mejores <- unlist(apply(rmses, MARGIN = 1, which.min))
    uMejores <- unique(mejores)
    i <- uMejores[1]
    for (i in uMejores) {
      print(sum(!is.na(mejores) & mejores==i))
    }
    
    linePlot(x=fechasObservaciones, y=rmses)
    t(sapply(validationStats$validationStatsEspaciales, function(x) { round(apply(x, 2, mean, na.rm=T), 2)}))
    
    ordenModelosPorColumnas <- names(cvs)
    ordenModelosPorColumnas <- c('K', 'GRK-Combinado', 'GRK-Combinado0.6')
    calcAndPlotAllValidationStatisticsV2(
      fechas = fObs, pronosticos = cvs_idx, observaciones = vObs,
      climatologias = NULL, coordsObservaciones = coordsObservaciones, 
      shpBase = shpBase, xyLims = xyLims, nColsPlots = min(length(ordenModelosPorColumnas), 3),
      ordenModelosPorColumnas = ordenModelosPorColumnas,
      tamaniosPuntos=8, tamanioFuentePuntos=7, tamanioFuenteEjes=20, tamanioFuenteTitulo=22,
      carpetaSalida=pathResultadosValidacion)    
  }
}


############# Plots #############
if (runPlots) {
  dirPlots <- paste0('Resultados/5-ComparacionModelos', nombreExperimento, '/')
  source('graficosParticulares.r', encoding = 'WINDOWS-1252')
  # ti <- 1
  
  plotComparacionModelos(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, 
    pathsModelos=cargarRegresores(carpetaRegresores = pathResultadosGrillado, fechasRegresando = fechasObservaciones), 
    modelosAPlotear=c('GPM', 'GSMaP', 'K', 'GR-Combinado', 'GRK-Combinado'), 
    especificacionEscala=especificacionEscala, shpBase=shpBase, nColsPlots=3, 
    carpetaSalida=dirPlots, replot=FALSE)
}

i