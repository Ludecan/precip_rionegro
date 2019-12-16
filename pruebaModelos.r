setwd('F:/ADME/precip_rionegro')

# Índice
# 5 - Preparación de Parámetros
# 6 - Interpolación de los datos
# 7 - Cross Validation

dt_ini='2017-09-01'
dt_fin = '2019-12-13'
dt_ini='2018-10-21'
dt_fin = '2019-12-07'

horaUTCInicioAcumulacion = 10
horaLocalInicioAcumulacion = horaUTCInicioAcumulacion - 3

source('cargaDatos.r')
source(paste(pathSTInterp, 'qc/qcTests.r', sep=''))
test <- testEspacialPrecipitacion(
  coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones,
  valoresObservaciones = valoresObservaciones)

test$reemplazar[test$tipoOutlier %in% tiposOutliersValoresSospechosos] <- 1
valoresObservaciones <- ejecutarReemplazosSRT(test, valoresObservaciones)

runTestsRegresores <- TRUE
runCV <- TRUE
runValidation <- TRUE
runGridding <- TRUE
runPlots <- TRUE

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
  umbralMascaraCeros=0.2,
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
  modoDiagnostico=TRUE)

{
listaParams <- list()
listaRegresores <- list()

# 1 - Kriging Ordinario Espacial
paramsI <- paramsBase
paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
paramsI$interpolationMethod <- 'automap'
paramsI$metodoIgualacionDistribuciones <- 'ninguna'
paramsI$umbralMascaraCeros <- 0.2
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
paramsI$umbralMascaraCeros <- 0.2
paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
listaParams[[4]] <- paramsI
listaRegresores[[4]] <- pathsRegresores[,c('GPM'), drop=FALSE]

# 5 - Kriging Universal Espacial + Regresion Generalizada en GSMaP
paramsI <- paramsBase
paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
paramsI$interpolationMethod <- 'automap'
paramsI$metodoIgualacionDistribuciones <- 'GLS'
paramsI$umbralMascaraCeros <- 0.2
paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
listaParams[[5]] <- paramsI
listaRegresores[[5]] <- pathsRegresores[,c('GSMaP'), drop=FALSE]

# 6 - Kriging Universal Espacial + Regresion Generalizada en GPM y GSMaP
paramsI <- paramsBase
paramsI$mLimitarValoresInterpolados <- 'LimitarMinimoyMaximo'
paramsI$interpolationMethod <- 'automap'
paramsI$metodoIgualacionDistribuciones <- 'GLS'
paramsI$umbralMascaraCeros <- 0.2
paramsI$metodoRemocionDeSesgo <- 'IDW_ResiduosPositivos'
listaParams[[6]] <- paramsI
listaRegresores[[6]] <- pathsRegresores[,c('GPM', 'GSMaP'), drop=FALSE]
}

source(paste(pathSTInterp, 'interpolar/testInterpolationModels.r', sep=''))

############# Tests Regresores #############
if (runTestsRegresores) {
  testRegressors(valoresObservaciones, pathsRegresores, pathSHPNotNUll=pathSHPMapaBase, 
                 pathResultados='Resultados/', seriesName='Rainfall', 
                 outputTableFilename='testRegresores.csv')
}

############# Cross Validation #############

if (runCV) {
  cvs <- st_interpCrossValidations(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, listaParams, listaRegresores, 
    pathResultados='Resultados/3-GrilladoYCV/', recalcCV=FALSE, modelosACorrer=1:length(listaParams))
    
    ############# Validation Stats #############
    if (runValidation) {
      validationStats <- calcValidationStatistics(valoresObservaciones, cvs, climatologias=NULL, 
                                                  pathResultados='Resultados/4-Validacion/')
      
      ordenModelosPorColumnas <- nombresModelosACorrer
      calcAndPlotAllValidationStatisticsV2(
        fechas = fechasObservaciones, pronosticos = cvs, observaciones = valoresObservaciones, 
        climatologias = climatologias, coordsObservaciones = coordsObservaciones, 
        shpBase = shpBase, xyLims = xyLims, nColsPlots = min(length(ordenModelosPorColumnas), 3),
        ordenModelosPorColumnas = ordenModelosPorColumnas, carpetaSalida = pathResultados, 
        tamaniosPuntos = 8, tamanioFuentePuntos = 7, tamanioFuenteEjes = 20)    
    }
}

############# Gridding #############
iAnio <- 1
# nombresModelosACorrer
i <- modelosACorrer[1]
if (runGridding) {
  #for (iAnio in 1:nrow(tSeqs)) {
  for (iAnio in 1:1) {
    for (i in modelosACorrer) {
      try({
        paramsI <- listaParams[[i]]
        if (is.na(listaRegresores[i])) { pr <- NULL 
        } else { pr <- listaRegresores[[i]] }
        
        nomModelo <- nombreModelo(params = paramsI, pathsRegresores=pr)
        print(paste(Sys.time(), ': Gridding ', nomModelo, ' ', tSeqs$anio[iAnio], ' (', (iAnio-1) * length(listaParams) + i, '/', nrow(tSeqs) * length(listaParams), ')', sep=''))
        
        if (!is.null(paramsI$tlagsAR)) { modTIni <- 2 * paramsI$ventanaIgualacionDistribuciones + max(paramsI$tlagsAR)
        } else { modTIni <- 0 }
        tIni <- max(1, tSeqs$tIni[iAnio] - modTIni)
        #tIni <- tSeqs$tFin[iAnio] - (2 * paramsI$ventanaIgualacionDistribuciones + 1) # Para debuggear varios modelos rápido
        tFin <- tSeqs$tFin[iAnio]
        tSeq <- seq.int(from = tIni, to = tFin, by=1)      
        
        pathsModelos[i] <- paste('Resultados/2-GrilladoYCV/', nomModelo ,'/', sep='')
        dir.create(pathsModelos[i], showWarnings = F, recursive = T)
        
        # Hay problemas con el uso de RAM. Hay que disminuir el tamaño de la ventana para los stUniversalKriging porque están usando como 4 gigas por thread
        if (paramsI$interpolationMethod == 'stUniversalKriging') paramsI$nCoresAUsar <- 6
        # paramsI$nCoresAUsar <- 1
        
        nomModeloFormateadoParaArchivos <- gsub(pattern = '*', replacement = '', x = nomModelo, fixed = T)
        listaMapas$nombreArchivo <- paste('Resultados/2-GrilladoYCV/', nomModeloFormateadoParaArchivos,'/', 
                                          appendToFileName(filename=nombresArchivos, postFijo=paste('_', nomModeloFormateadoParaArchivos, sep='')), sep='')
        
        #fechasObservaciones = fechasObservaciones[tSeq]
        #valoresObservaciones = valoresObservaciones[tSeq,]
        #pathsRegresores = pr[tSeq, , drop = F]
        #pathsRegresores = pr                          # Solo usar este para deteccionOutliersRLM
        #paramsIyM = paramsI
        #listaMapas=listaMapas[tSeq,]
        #paramsParaRellenoRegresores = NULL
        #pathsRegresoresParaRellenoRegresores = NULL
        #tsAInterpolar <- which(fechasObservaciones == as.POSIXct('2014-01-31', tz=tz(fechasObservaciones[1])))
        #tsAInterpolar <- which(fechasObservaciones[tSeq] == as.POSIXct('2014-01-02', tz=tz(fechasObservaciones[1])))
        #returnInterpolacion <- FALSE
        #tsAInterpolar <- 48
        tsAInterpolar <- seq_along(tSeq)
        
        interpolarYMapear(coordsObservaciones = coordsObservaciones, fechasObservaciones = fechasObservaciones[tSeq], 
                          valoresObservaciones = valoresObservaciones[tSeq,, drop=F], pathsRegresores = pr[tSeq, , drop = F], 
                          coordsAInterpolar = coordsAInterpolar, paramsIyM = paramsI, shpMask = shpMask, xyLims = xyLims,
                          listaMapas=listaMapas[tSeq,,drop=F], returnInterpolacion = F, paramsParaRellenoRegresores = NULL,
                          pathsRegresoresParaRellenoRegresores = NULL, tsAInterpolar = tsAInterpolar)
      })
    }
  }
}

############# Plots #############
if (runPlots) {
  dir.create('Resultados/ComparacionModelos/', showWarnings = F)
  # ti <- tSeq[1]
  # ti <- tSeq[length(tSeq)]
  # ti <- which(fechasObservaciones == as.POSIXct('2011-01-04', tz=tz(fechasObservaciones[1])))
  
  zcol=1
  for (ti in tSeq) {
    nomArch <- paste('Resultados/ComparacionModelos/', gsub(pattern = '-', replacement = '_', x = fechasObservaciones[ti]), '.png', sep='')
    
    if (rePlot || !file.exists(nomArch)) {
      grids <- list()
      data <- numeric()
      
      existenTodosLosArchivosDeLaFecha <- TRUE
      j <- 1
      while (j <= length(modelosACorrer) && existenTodosLosArchivosDeLaFecha) {
        jModelo <- modelosACorrer[j]
        
        paramsI <- listaParams[[jModelo]]
        if (is.na(listaRegresores[jModelo])) { pr <- NULL 
        } else { pr <- listaRegresores[[jModelo]] }
        
        nomModelo <- nombreModelo(params = paramsI, pathsRegresores=pr)
        pathArchTiModeloI <- paste('Resultados/', nomModelo ,'/', gsub(pattern = '-', replacement = '_', x = fechasObservaciones[ti]), '_', nomModelo, '.tif', sep='')
        
        if (file.exists(pathArchTiModeloI)) {
          grids[[length(grids) + 1]] <- readGDAL(fname = pathArchTiModeloI, silent = T)
          names(grids)[length(grids)] <- nomModelo
          data <- c(data, as.vector(grids[[length(grids)]]@data[,zcol]))
          j <- j + 1
        } else { existenTodosLosArchivosDeLaFecha <- FALSE }
      }
      
      if (existenTodosLosArchivosDeLaFecha) {
        print(paste('Plot:', fechasObservaciones[ti]))
        # Esto solo funciona con un regresor y es medio cerdo para corregir las unidades de la proyección de km a m
        u1 <- readGDAL(fname = pathsRegresoresST[ti,1], silent = T)
        if (!identicalCRS(u1, grids[[1]])) u1 <- SpatialPixelsDataFrame(points=SpatialPoints(coordinates(u1) / 1000, proj4string=CRS(proj4StringAInterpolar)), data = u1@data)
        grids[[length(grids) + 1]] <- u1
        names(grids)[length(grids)] <- colnames(pathsRegresoresST)[1]
        data <- c(data, as.vector(grids[[length(grids)]]@data[,zcol]), valoresObservaciones[ti,])
        
        nIntervals <- 10
        #colores <- c('gray90', c(brewer.pal(n = nIntervals - 1, name = 'Blues'), ""))
        colores <- c(rev(brewer.pal(n = nIntervals + 1, name = 'Spectral')))
        escala <- crearEscalaEquiespaciada(datos = data, nIntervalos = nIntervals, nDigitos=1, continuo=T, colores = colores)
        rm(u1, data)
        
        nrows <- length(ordenModelosPorColumnas) / 3
        gs <- list()
        length(gs) <- length(grids) + nrows
        
        gs[[1]] <- mapearPuntosGGPlot(puntos = SpatialPointsDataFrame(coords = geometry(coordsObservaciones), data = data.frame(valoresObservaciones[ti,])), 
                                      shpBase = shpMask$shp, xyLims=xyLims, escala = escala, titulo = paste('Observaciones: ', fechasObservaciones[ti], sep=''), 
                                      dibujar=F, tamaniosPuntos = 3, contornearPuntos = TRUE)
        for (j in 2:nrows) {
          residuos <- valoresObservaciones[ti,] - over(geometry(coordsObservaciones), grids[[ordenModelosPorColumnas[j]]])[, zcol]
          gs[[j]] <- mapearPuntosGGPlot(puntos = SpatialPointsDataFrame(coords = geometry(coordsObservaciones), data = data.frame(residuos)), 
                                        shpBase = shpMask$shp, xyLims=xyLims, dibujar=F, tamaniosPuntos = 3, contornearPuntos = TRUE, 
                                        titulo = paste('Residuos ', ordenModelosPorColumnas[j], ': ', fechasObservaciones[ti], ': ', sep=''))
        }
        
        #dibujarEscala=T
        #dibujarEjes=T
        #isolineas=F
        #DPI=90
        #widthPx=630
        #heightPx=630
        #subtitulo = ''
        #continuo <- escala$continuo
        #j <- 1
        for (j in 1:length(grids)) {
          gr <- grids[[ordenModelosPorColumnas[j]]]
          gs[[j + nrows]] <- mapearGrillaGGPlot(grilla = gr, shpBase = shpMask$shp, escala=escala, xyLims = xyLims, 
                                                titulo = paste(ordenModelosPorColumnas[j], ': ', fechasObservaciones[ti], sep=''), 
                                                dibujar=F, alturaEscalaContinua = unit(x=0.7, units = 'in'))
        }
        
        png(nomArch, width = 1920, height = 1080)
        tryCatch(expr = print(multiplot(plotlist=gs, cols=4)), finally = dev.off())
      }
    }
  }
}