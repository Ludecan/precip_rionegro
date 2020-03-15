mapearClimatologias <- function() {
  # Para hacer un grafico de 4 paneles con las climatologias, una para cada estaci?n del a?o
  
  # Verano -> DEF
  # Otoño -> MAM
  # Invierno -> JJA
  # Primavera -> SON
  
  alturaEscalaContinua = unit(x=0.7, units = 'in')
  widthPx <- 1200
  heightPx <- 1080
  
  fechas <- c(as.POSIXct('2017-01-15'), as.POSIXct('2017-04-15'),  as.POSIXct('2017-07-15'), as.POSIXct('2017-10-15'))
  iDiasDelAnio <- yday(fechas)

  for (iClimatologia in 1:3) {
    if (iClimatologia == 1) {
      paths <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_mean/%03d.tif', iDiasDelAnio), sep='')    
      baseNomArch <- 'ClimatologiasMedias'
      baseTitulo <- 'Climatología Media LST'
    } else if (iClimatologia == 2) {
      paths <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_median/%03d.tif', iDiasDelAnio), sep='')    
      baseNomArch <- 'ClimatologiasMedianas'
      baseTitulo <- 'Climatología Mediana LST'
    } else {
      paths <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_SD/%03d.tif', iDiasDelAnio), sep='')    
      baseNomArch <- 'ClimatologiasSDs'
      baseTitulo <- 'Climatología Desv. Estándar LST'
    }
    
    gs <- list()
    length(gs) <- 4
    
    verano <- readGDAL(paths[1], silent = T)
    otonio <- readGDAL(paths[2], silent = T)
    invierno <- readGDAL(paths[3], silent = T)
    primavera <- readGDAL(paths[4], silent = T)
    
    shpBase <- cargarSHP(paste(pathDatos, 'MapaUruguay/uruguay_departamentos.shp', sep=''))
    shpBase <- spTransform(shpBase, CRSobj = CRS(proj4string(verano)))
    
    for (iEscala in 1:2) {
      escalas <- list()
      length(escalas) <- 4
      if (iEscala == 1) {
        escalas[[1]] <- crearEscalaEquiespaciada(datos = c(verano@data$band1, otonio@data$band1,
                                                           invierno@data$band1, primavera@data$band1), continuo = T)
        escalas[[2]] <- escalas[[1]]
        escalas[[3]] <- escalas[[1]]
        escalas[[4]] <- escalas[[1]]
        postFijoNomArchEscala <- '_EscalaFija'
      } else {
        escalas[[1]] <- crearEscalaEquiespaciada(datos = verano@data$band1, continuo = T)
        escalas[[2]] <- crearEscalaEquiespaciada(datos = otonio@data$band1, continuo = T)
        escalas[[3]] <- crearEscalaEquiespaciada(datos = invierno@data$band1, continuo = T)
        escalas[[4]] <- crearEscalaEquiespaciada(datos = primavera@data$band1, continuo = T)
        postFijoNomArchEscala <- '_EscalaAjustada'
      }
      
      gs[[1]] <- mapearGrillaGGPlot(grilla = verano, shpBase = shpBase, escala = escalas[[1]], dibujar=F, 
                                    titulo = paste(baseTitulo, ' Verano (', format(fechas[1], format='%d de %B'), ')', sep=''), 
                                    alturaEscalaContinua = alturaEscalaContinua)
      gs[[2]] <- mapearGrillaGGPlot(grilla = invierno, shpBase = shpBase, escala = escalas[[3]], dibujar=F, 
                                    titulo = paste(baseTitulo, ' Invierno (', format(fechas[3], format='%d de %B'), ')', sep=''), 
                                    alturaEscalaContinua = alturaEscalaContinua)
      gs[[3]] <- mapearGrillaGGPlot(grilla = otonio, shpBase = shpBase, escala = escalas[[2]], dibujar=F, 
                                    titulo = paste(baseTitulo, ' Otoño (', format(fechas[2], format='%d de %B'), ')', sep=''), 
                                    alturaEscalaContinua = alturaEscalaContinua)
      gs[[4]] <- mapearGrillaGGPlot(grilla = primavera, shpBase = shpBase, escala = escalas[[4]], dibujar=F, 
                                    titulo = paste(baseTitulo, ' Primavera (', format(fechas[4], format='%d de %B'), ')', sep=''), 
                                    alturaEscalaContinua = alturaEscalaContinua)  
      
      oldSciPen <- getOption("scipen")
      options(scipen=15)
      archivoSalida <- paste('Resultados/Exploracion/', baseNomArch, postFijoNomArchEscala, '.png', sep='')
      
      png(archivoSalida, width = widthPx, height = heightPx)
      tryCatch(expr = print(multiplot(plotlist=gs, cols=2)), finally = dev.off())
      options(scipen = oldSciPen)
    }
  }
}

graficarClimatologiasPuntuales <- function() {
  # Para hacer un grafico de 4 paneles, 2 arriba con la media y SD del p?xel de una estaci?n y 2 m?s iguales abajo.
  # Se puede usar por ejemplo para mostrar una estaci?n al norte y una al sur del pa?s
  
  alturaEscalaContinua = unit(x=0.7, units = 'in')
  widthPx <- 1200
  heightPx <- 1080

  diasDelAnio <- 1:365  
  paths <- cbind(paste(pathDatos, sprintf('LST_Night_Combinada_Clim_mean/%03d.tif', diasDelAnio), sep=''), 
                 paths <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_SD/%03d.tif', diasDelAnio), sep=''))
  colnames(paths) <- c('Media', 'Desviación Estándar')

  estacionesAConsiderar <- estaciones[estaciones$Nombre %in% c('Aeropuerto.Carrasco', 'Prado'),]
  
  coords <- coordinates(estacionesAConsiderar)
  aux <- estacionesAConsiderar[order(coords[, 2], coords[, 1], decreasing = c(T, F)),]
  clims <- extraerValoresRegresoresSobreSP(objSP = aux, pathsRegresores = paths, silent = T)
  
  #aux <- cbind(clims[[1]][,1], clims[[2]][,1])
  #colnames(aux) <- names(clims)
  #linePlot(x=diasDelAnio, y = aux, tituloEjeX = 'Día del Año', tituloEjeY = 'ºC', 
  #         titulo = paste('Climatología de LST de ', estacionesAConsiderar$Nombre[1], sep=''))
  
  xyLims <- lapply(clims, FUN = function(x) {
    rango <- range(x, na.rm = T)
    return(crearXYLims(xMin = 1, xMax = 365, yMin = rango[1], yMax = rango[2]))
  })
  
  iClimatologia <- 1
  iEstacion <- 1
  gs <- list()
  length(gs) <- length(estacionesAConsiderar) * length(clims)
  i <- 1
  for (iEstacion in seq_along(estacionesAConsiderar)) {
    for (iClimatologia in seq_along(clims)) {
      gs[[i]] <- linePlot(x=diasDelAnio, y = clims[[iClimatologia]][, iEstacion, drop=F], tituloEjeX = 'Día del Año', tituloEjeY = 'ºC', 
                          titulo = paste('Climatología ', names(clims)[iClimatologia], ' de LST de ', estaciones$Nombre[iEstacion], sep=''),
                          xyLims = xyLims[[iClimatologia]], dibujar = F)
      i <- i + 1
    }
  }
  multiplot(plotlist=gs, cols=length(estacionesAConsiderar))
  
  iClimatologia <- 1
  linePlot(x=diasDelAnio, y = clims[[iClimatologia]], tituloEjeX = 'Día del Año', tituloEjeY = 'ºC', 
           titulo = paste('Climatolog?a ', names(clims)[iClimatologia], ' de LST', sep=''), dibujar = F)
    
      
  oldSciPen <- getOption("scipen")
  options(scipen=15)
  archivoSalida <- paste('Resultados/Exploracion/ClimatologiasTemporales.png', sep='')
  
  png(archivoSalida, width = widthPx, height = heightPx)
  tryCatch(expr = print(multiplot(plotlist=gs, cols=length(estacionesAConsiderar))), finally = dev.off())
  options(scipen = oldSciPen)
}

plotEjemplosRellenoRasters <- function() {
  # Dibuja las 4 opciones de relleno que se manejaron
  # Arriba Izquierda: Dato crudo original
  # Arriba Derecha: Relleno Temporal _9
  # Abajo Izquierda: Relleno Espacial
  # Abajo Derecha: Filtrado + Relleno Espacial
  colnames(pathsRegresores)
  #paths <- pathsRegresores[,c('MOD11A1_LST_Night', 
  #                            'MOD11A1_LST_Night_filtrado', 
  #                            'MOD11A1_LST_Night_Rellenado_RK-LST_Night_Combinada_Clim_mean', 
  #                            'MOD11A1_LST_Night_filtrado_Rellenado_RK-LST_Night_Combinada_Clim_mean'), drop=F]
  #colnames(paths) <- c('MOD11A1_LST_Night', 'Filtrado', 'Rellenado', 'Filtrado + Rellenado')
  
  paths <- pathsRegresores[,c('MOD11A1_LST_Night',
                              'MOD11A1_LST_Night_9',
                              'MOD11A1_LST_Night_R',
                              'MOD11A1_LST_Night_FRv2'), drop=F]
  colnames(paths) <- c('MOD11A1_LST_Night', 'Reconstrucción Temporal', 'Reconstrucción Espacial', 'Filtrado + Reconstrucción Espacial')
  
  
  i <- 47
  i <- which(fechasObservaciones == as.POSIXct('2002-07-17', tz=tz(fechasObservaciones[1])))
  j <- 4
  colnames(paths)
  escala <- crearEscalaEquiespaciadaMultiRasters(paths[i, , drop=F])
  for (j in 1:ncol(paths)) {
    plotRasterI(i = i, pathsRaster = paths[, j], shpBase=shpMask$shp, escala=escala, carpetaSalida='Resultados/EjemplosFiltrado/', 
                titulos=paste(fechasObservaciones, ': ', colnames(paths)[j], sep = ''), replot=F, widthPx = 1080)
  }
  
  dir.create(carpetaSalida, recursive = T)
  plotMultiRastersEnPanelesI(i = i, pathsRasters = paths[, c(1, 3, 2, 4)], fechasRasters = fechasObservaciones, shpBase = shpMask$shp,
                             escalas = NULL, carpetaSalida='Resultados/EjemplosReconstruccion/Terra/', nCols=2, replot = T, 
                             widthPx = 1080, heightPx = 1080, alturaEscalaContinua = unit(x=heightPx / 1080, units = 'in'))
  
  nCoresAUsar <- detectCores(T, T)
  cl <- makeCluster(getOption("cl.cores", nCoresAUsar))
  clusterExport(cl, varlist = c('script.dir.funcionesAuxiliares'))
  clusterEvalQ(cl = cl, expr = {
    require(rgdal)
    require(sp)
    require(Rmisc)
    source(paste0(script.dir.funcionesAuxiliares, 'mapearEx.r'), encoding = 'WINDOWS-1252')
    source(paste0(script.dir.funcionesAuxiliares, 'funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')
  })
  parSapplyLB(cl=cl, X=1:length(fechasObservaciones), FUN=plotMultiRastersEnPanelesI, pathsRasters = paths[, c(1, 3, 2, 4)], 
              fechasRasters = fechasObservaciones, shpBase = shpMask$shp, escalas = NULL, 
              carpetaSalida='Resultados/EjemplosReconstruccion/Terra/', nCols = 2, replot=T,
              widthPx = 1080, heightPx = 1080, alturaEscalaContinua = unit(x=heightPx / 1080, units = 'in'))
  stopCluster(cl)
  
  colnames(pathsRegresores)
  paths <- pathsRegresores[,c('MOD11A1_LST_Night', 'MYD11A1_LST_Night', 'MYD11A1_LST_Night_filtrado_Rellenado_RK-LST_Night_Combinada_Clim_mean', 'MOD11A1_LST_Night_filtrado_Rellenado_RK-LST_Night_Combinada_Clim_mean'), drop=F]
  colnames(paths)
  
  nCoresAUsar <- detectCores(T, T)
  cl <- makeCluster(getOption("cl.cores", nCoresAUsar))
  clusterExport(cl, varlist = c('script.dir.funcionesAuxiliares'))
  clusterEvalQ(cl = cl, expr = {
    require(rgdal)
    require(sp)
    require(Rmisc)
    source(paste0(script.dir.funcionesAuxiliares, 'mapearEx.r'), encoding = 'WINDOWS-1252')
    source(paste0(script.dir.funcionesAuxiliares, 'funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')
  })
  parSapplyLB(cl=cl, X=1:length(fechasObservaciones), FUN=plotMultiRastersEnPanelesI, pathsRasters = paths[, c(1, 4, 2, 3)], 
              fechasRasters = fechasObservaciones, shpBase = shpMask$shp, escalas = NULL, 
              carpetaSalida='Resultados/EjemplosFiltrado/TerraYAqua/', nCols = 2, replot=T)
  stopCluster(cl)
}

plotComparacionModelos <- function(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, 
    pathsModelos=cargarRegresores(carpetaRegresores = 'Resultados/3-Grillado', fechasRegresando = fechasObservaciones), 
    modelosAPlotear=c('GPM', 'GSMaP', 'K', 'GRK-Combinado', 'GRK-Combinado0.6'), 
    especificacionEscala, shpBase, nColsPlots=3, carpetaSalida='Resultados/5-ComparacionModelos', 
    replot=FALSE) {
  pathsModelos <- pathsModelos[, modelosAPlotear]
  especificacionEscala <- paramsBase$especEscalaDiagnostico
  
  iFecha <- 4
  plotComparacionModelosI <- function(
      iFecha, coordsObservaciones, fechasObservaciones, valoresObservaciones, pathsModelos, 
      especificacionEscala, shpBase, nColsPlots, carpetaSalida, replot) {
    print(iFecha)
    nomArchMapa <- paste0(carpetaSalida, '/', format(fechasObservaciones[iFecha], format='%Y%m%d'), '.png')
    
    if (replot || !file.exists(nomArchMapa) || file.info(nomArchMapa)$size == 0) {
      if (!any(is.na(pathsModelos[iFecha,]))) {
        nFilasPlots <- ceiling((ncol(pathsModelos) + 1) / nColsPlots)
        alturaEscalaContinua = unit(x=0.7 * nFilasPlots, units = 'in')
        
        coordsObservaciones$value <- valoresObservaciones[iFecha, ]
        rastersI <- lapply(pathsModelos[iFecha,], FUN = readGDAL, silent=T)
        
        xyLims <- getXYLims(c(shpBase, coordsObservaciones, rastersI), ejesXYLatLong = F)
        escala <- darEscala(especificacionEscala, 
                            c(coordsObservaciones$value, 
                              unlist(sapply(rastersI, FUN = function(x) x@data[,1]))))
        
        gs <- vector(mode = "list", length = ncol(pathsModelos) + 1)    
        gs[[1]] <- mapearPuntosGGPlot(
          puntos=coordsObservaciones, shpBase=shpBase, escala=escala, xyLims=xyLims, zcol='value', 
          dibujarTexto = T, titulo = paste0('Observaciones - ', fechasObservaciones[iFecha]), 
          dibujar=F, alturaEscalaContinua=alturaEscalaContinua)
        for (iModelo in 1:ncol(pathsModelos)) {
          gs[[iModelo+1]] <- mapearGrillaGGPlot(
            grilla=rastersI[[iModelo]], shpBase=shpBase, escala=escala, xyLims=xyLims, 
            titulo = paste0(colnames(pathsModelos)[iModelo], ' - ', fechasObservaciones[iFecha]), 
            dibujarPuntosObservaciones=T, coordsObservaciones=coordsObservaciones, dibujar=F,
            alturaEscalaContinua = alturaEscalaContinua)
        }
        
        gs <- gs[permutacionColumnasParaGraficarPorFilas(length(gs), nColsPlot = nColsPlots)]
        
        png(nomArchMapa, width = 630 * nColsPlots, height = 630 * nFilasPlots, type='cairo')
        tryCatch(expr = multiplot(plotlist=gs, cols=nColsPlots), finally = dev.off())
      }
    }
  }
  
  dir.create(carpetaSalida, showWarnings = F)
  
  nCoresAUsar <- detectCores(T, T)
  if (nCoresAUsar > 1) {
    cl <- makeCluster(getOption("cl.cores", nCoresAUsar))
    paste(pathSTInterp, 'interpolar/mapearEx.r', sep='')
    clusterExport(cl, varlist = c('pathSTInterp'))
    clusterEvalQ(cl = cl, expr = {
      require(rgdal)
      require(sp)
      require(Rmisc)
      source(paste0(pathSTInterp, 'interpolar/leerEscalas.r'), encoding = 'WINDOWS-1252')
      source(paste0(pathSTInterp, 'interpolar/mapearEx.r'), encoding = 'WINDOWS-1252')
      source(paste0(pathSTInterp, 'interpolar/funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')
    })
    parSapplyLB(
      cl=cl, X=seq_along(fechasObservaciones), FUN=plotComparacionModelosI, 
      coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones, 
      valoresObservaciones=valoresObservaciones, pathsModelos=pathsModelos, 
      especificacionEscala=especificacionEscala, shpBase=shpBase, nColsPlots=nColsPlots, 
      carpetaSalida=carpetaSalida, replot=replot)
    stopCluster(cl)
  } else {
    sapply(
      X=seq_along(fechasObservaciones), FUN=plotComparacionModelosI, 
      coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones, 
      valoresObservaciones=valoresObservaciones, pathsModelos=pathsModelos, 
      especificacionEscala=especificacionEscala, shpBase=shpBase, nColsPlots=nColsPlots, 
      carpetaSalida=carpetaSalida, replot=replot)
  }
}

plotObservacionesYRegresores <- function(
    coordsObservaciones, fechasObservaciones, valoresObservaciones, 
    pathsRegresoresAEvaluar=pathsRegresores, shpBase, nColsPlots=ncol(pathsRegresoresAEvaluar)+1, 
    carpetaSalida='datos/mapas/', replot=FALSE, grillaAlternativaRegresores=coordsAInterpolar,
    especificacionEscala=NULL) {
  # replot=F
  plotObservacionesYRegresoresI <- function(
      iFecha, coordsObservaciones, fechasObservaciones, valoresObservaciones, 
      pathsRegresoresAEvaluar, shpBase, nColsPlots, carpetaSalida, replot, 
      grillaAlternativaRegresores, especificacionEscala) {
    # iFecha <- 253
    print(iFecha)
    
    nomArchMapa <- paste(carpetaSalida, format(fechasObservaciones[iFecha], format='%Y%m%d'), '.png', sep='')
    
    if (replot || !file.exists(nomArchMapa) || file.info(nomArchMapa)$size == 0) {
      if (!all(is.na(pathsRegresoresAEvaluar[iFecha,]))) {
        nFilasPlots <- ceiling((ncol(pathsRegresoresAEvaluar) + 1) / nColsPlots)
        alturaEscalaContinua = unit(x=0.7 * nFilasPlots, units = 'in')
        
        coordsObservaciones$value <- valoresObservaciones[iFecha, ]
        # x <- pathsRegresoresAEvaluar[iFecha, 1]
        rastersI <- lapply(pathsRegresoresAEvaluar[iFecha,], FUN = function(x) {
            if (!is.na(x)) { 
              return(readGDAL(x, silent = T))
            } else { 
              grillaAux <- grillaSobreBoundingBox(objSP = shpBase, nCeldasX = 2, nCeldasY = 2)
              grillaAux <- SpatialGridDataFrame(grillaAux, data = data.frame(value=rep(NA, length(grillaAux))))
              return(grillaAux)
            }
          })
        
        if (grepl(pattern = '+units=m', x = proj4string(rastersI[[1]]), fixed = T)) {
          rastersI[[1]] <- SpatialGridDataFrameEnMtoSpatialGridDataFrameEnKm(rastersI[[1]])
        }
        
        if (!is.null(grillaAlternativaRegresores)) {
          rastersI <- sapply(rastersI, FUN = function(x, grillaAlternativaRegresores) {
            if (!identicalCRS(x, grillaAlternativaRegresores)) {
              grillaAux <- spTransform(geometry(grillaAlternativaRegresores), proj4string(x))
              grillaAux <- SpatialPixelsDataFrame(
                grillaAlternativaRegresores, data = data.frame(value=over(grillaAux, x)))
              
              return(grillaAux)
            } else {
              return(x)
            }
          }, grillaAlternativaRegresores=grillaAlternativaRegresores)
        }
        
        xyLims <- getXYLims(c(shpBase, coordsObservaciones, rastersI), ejesXYLatLong = F)
        if (is.null(especificacionEscala)) {
          escala <- crearEscalaEquiespaciada(
            c(coordsObservaciones$value, sapply(rastersI, FUN = function(x) x@data[,1])), 
            nDigitos = 1, nIntervalos = 10, continuo = T)  
        } else {
          escala <- darEscala(
            especificacion = especificacionEscala,
            valores = c(coordsObservaciones$value, 
                        unlist(sapply(rastersI, FUN = function(x) x@data[,1])), use.names = FALSE))
        }
        
        gs <- vector(mode = "list", length = ncol(pathsRegresoresAEvaluar) + 1)
        gs[[1]] <- mapearPuntosGGPlot(
          puntos = coordsObservaciones, shpBase = shpBase, escala = escala, xyLims = xyLims, 
          zcol='value', dibujarTexto = T, 
          titulo = paste('Observaciones - ', fechasObservaciones[iFecha], sep=''), dibujar = F,
          alturaEscalaContinua = alturaEscalaContinua)
        
        for (iModelo in seq_along(rastersI)) {
          gs[[iModelo+1]] <- mapearGrillaGGPlot(
            grilla = rastersI[[iModelo]], shpBase = shpBase, escala = escala, xyLims = xyLims, 
            titulo = paste(colnames(pathsRegresoresAEvaluar)[iModelo], ' - ', fechasObservaciones[iFecha], sep=''), 
            dibujarPuntosObservaciones = T, coordsObservaciones = coordsObservaciones, dibujar = F,
            alturaEscalaContinua = alturaEscalaContinua)
        }
        
        gs <- gs[permutacionColumnasParaGraficarPorFilas(length(gs), nColsPlot = nColsPlots)]
        png(nomArchMapa, width = 630 * nColsPlots, height = 630 * nFilasPlots, type='cairo')
        tryCatch(expr = multiplot(plotlist=gs, cols=nColsPlots), finally = dev.off())
      }
    }
  }
  
  if (!identicalCRS(shpBase, coordsObservaciones)) {
    shpBase <- spTransform(shpBase, CRS(proj4string(coordsObservaciones)))
  }
  
  dir.create(carpetaSalida, showWarnings = FALSE, recursive = TRUE)
  nCoresAUsar <- min(getAvailableCores(maxCoresPerGB = 1), length(fechasObservaciones))
  if (nCoresAUsar > 1) {
    cl <- makeCluster(getOption("cl.cores", nCoresAUsar))
    clusterExport(cl, varlist = c('script.dir.funcionesAuxiliares'))
    clusterEvalQ(cl = cl, expr = {
      source(paste0(script.dir.funcionesAuxiliares, 'interpolarEx.r'), encoding = 'WINDOWS-1252')
      source(paste0(script.dir.funcionesAuxiliares, 'leerEscalas.r'), encoding = 'WINDOWS-1252')
      source(paste0(script.dir.funcionesAuxiliares, 'mapearEx.r'), encoding = 'WINDOWS-1252')
      source(paste0(script.dir.funcionesAuxiliares, 'funcionesAuxiliares.r'), encoding = 'WINDOWS-1252')
    })
    parSapplyLB(
      cl=cl, X=seq_along(fechasObservaciones), FUN=plotObservacionesYRegresoresI, 
      coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones, 
      valoresObservaciones=valoresObservaciones, pathsRegresoresAEvaluar=pathsRegresoresAEvaluar, 
      shpBase=shpBase, nColsPlots=nColsPlots, carpetaSalida=carpetaSalida, replot=replot,
      grillaAlternativaRegresores=grillaAlternativaRegresores, especificacionEscala=especificacionEscala)
    stopCluster(cl)
  } else {
    sapply(
      X=seq_along(fechasObservaciones), FUN=plotObservacionesYRegresoresI, 
      coordsObservaciones=coordsObservaciones, fechasObservaciones=fechasObservaciones, 
      valoresObservaciones=valoresObservaciones, pathsRegresoresAEvaluar=pathsRegresoresAEvaluar, 
      shpBase=shpBase, nColsPlots=nColsPlots, carpetaSalida=carpetaSalida, replot=replot,
      grillaAlternativaRegresores=grillaAlternativaRegresores, especificacionEscala=especificacionEscala)    
  }
  replot = F
  nColsPlots <- 3
}

plotClimatologiasYSD_v006 <- function() {
  iDias <- 1:365
  pathDatos <- 'G:/Tesis/Datos/MODIS_V006/'
  shpBase <- cargarSHP('G:/Tesis/Datos/MapaUruguay/uruguay_departamentos.shp')
  
  pathsRasters <- cbind(paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_mean/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_median/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_mean/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_median/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_sd/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_mad/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_sd/%03d.tif', iDias), sep=''),
                        paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_mad/%03d.tif', iDias), sep=''))
  colnames(pathsRasters) <- c('Media MOD11A1', 'Mediana MOD11A1', 'Desviación Estandar MOD11A1', 'Desviación Mediana Absoluta MOD11A1',
                              'Media MYD11A1', 'Mediana MYD11A1', 'Desviación Estandar MYD11A1', 'Desviación Mediana Absoluta MYD11A1')
  
  r1 <- extraerRangoSPDataFrames(as.vector(pathsRasters[,c(1,2,5,6)]))
  r2 <- extraerRangoSPDataFrames(as.vector(pathsRasters[,c(3,4,7,8)]))
  
  escalaCentral <- crearEscalaEquiespaciada(r1, nIntervalos = 10, continuo = T)
  escalaSD <- crearEscalaEquiespaciada(r2, brewerPal = 'Reds', nIntervalos = 8, continuo = T)
  
  escalas <- list()
  escalas[[1]] <- escalaCentral
  escalas[[2]] <- escalaCentral
  escalas[[3]] <- escalaSD
  escalas[[4]] <- escalaSD
  escalas[[5]] <- escalaCentral
  escalas[[6]] <- escalaCentral
  escalas[[7]] <- escalaSD
  escalas[[8]] <- escalaSD  
  
  plotMultiRastersEnPaneles(pathsRasters = pathsRasters, fechasRasters = sprintf('%03d', iDias), 
                            carpetaSalida = 'E:/Tesis/Resultados/MODISv006/ClimatologiasLST/', 
                            postFijoNomArchSalida = '', shpBase = shpBase, escalas = escalas, 
                            nCols = 4, byRow = T, replot = F, widthPx = 1920 * 2, 
                            alturaEscalaContinua=unit(x=2, units = 'in'))
  
  
  pathsRastersCentral <- cbind(paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_mean/%03d.tif', iDias), sep=''),
                               paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_median/%03d.tif', iDias), sep=''),
                               paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_mean/%03d.tif', iDias), sep=''),
                               paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_median/%03d.tif', iDias), sep=''))
  colnames(pathsRastersCentral) <- c('Media MOD11A1', 'Mediana MOD11A1', 'Media MYD11A1', 'Mediana MYD11A1')
  
  pathsRastersDispersion <- cbind(paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_sd/%03d.tif', iDias), sep=''),
                                  paste(pathDatos, sprintf('MOD11A1_LST_Night_Clim_mad/%03d.tif', iDias), sep=''),
                                  paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_sd/%03d.tif', iDias), sep=''),
                                  paste(pathDatos, sprintf('MYD11A1_LST_Night_Clim_mad/%03d.tif', iDias), sep=''))
  colnames(pathsRastersDispersion) <- c('Desviación Estandar MOD11A1', 'Desviación Mediana Absoluta MOD11A1', 'Desviación Estandar MYD11A1', 'Desviación Mediana Absoluta MYD11A1')
  
  r1 <- extraerRangoSPDataFrames(as.vector(pathsRastersCentral))
  r2 <- extraerRangoSPDataFrames(as.vector(pathsRastersDispersion))
  
  escalaCentral <- crearEscalaEquiespaciada(r1, nIntervalos = 10, continuo = T)
  escalaSD <- crearEscalaEquiespaciada(r2, brewerPal = 'Reds', nIntervalos = 8, continuo = T)
  
  escalas <- list()
  escalas[[1]] <- escalaCentral
  escalas[[2]] <- escalaCentral
  escalas[[3]] <- escalaCentral
  escalas[[4]] <- escalaCentral
  
  plotMultiRastersEnPaneles(pathsRasters = pathsRastersCentral, fechasRasters = sprintf('%03d', iDias), 
                            carpetaSalida = 'G:/Tesis/Resultados/MODISv006/ClimatologiasLST/Central/', 
                            postFijoNomArchSalida = '', shpBase = shpBase, escalas = escalas, 
                            nCols = 2, byRow = T, replot = F, widthPx = 1080 * 2, heightPx = 1080 * 2,
                            alturaEscalaContinua=unit(x=2, units = 'in'))
  
  escalas[[1]] <- escalaSD
  escalas[[2]] <- escalaSD
  escalas[[3]] <- escalaSD
  escalas[[4]] <- escalaSD  
  
  plotMultiRastersEnPaneles(pathsRasters = pathsRastersDispersion, fechasRasters = sprintf('%03d', iDias), 
                            carpetaSalida = 'G:/Tesis/Resultados/MODISv006/ClimatologiasLST/Dispersion/', 
                            postFijoNomArchSalida = '', shpBase = shpBase, escalas = escalas, 
                            nCols = 2, byRow = T, replot = F, widthPx = 1080 * 2, heightPx = 1080 * 2, 
                            alturaEscalaContinua=unit(x=2, units = 'in'))
}

plotClimatologiasYSD <- function() {
  iDias <- 1:365
  pathsClimMean <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_mean/%03d.tif', iDias), sep='')
  pathsClimMedian <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_median/%03d.tif', iDias), sep='')
  pathsClimSD <- paste(pathDatos, sprintf('LST_Night_Combinada_Clim_sd/%03d.tif', iDias), sep='')
  
  r1 <- extraerRangoSPDataFrames(pathsClimMean)
  r2 <- extraerRangoSPDataFrames(pathsClimMedian)
  r3 <- extraerRangoSPDataFrames(pathsClimSD)
  
  escalaCentral <- crearEscalaEquiespaciada(c(r1, r2), nIntervalos = 10, continuo = T)
  escalaSD <- crearEscalaEquiespaciada(r3, brewerPal = 'Reds', nIntervalos = 8, continuo = T)
  
  pathsRasters <- cbind(pathsClimMean, pathsClimMedian, pathsClimSD)
  colnames(pathsRasters) <- c('Media', 'Mediana', 'Desviación Estandar')
  
  escalas <- list()
  escalas[[1]] <- escalaCentral
  escalas[[2]] <- escalaCentral
  escalas[[3]] <- escalaSD
  
  plotMultiRastersEnPaneles(pathsRasters = pathsRasters, fechasRasters = sprintf('%03d', iDias), carpetaSalida = 'Resultados/ClimatologiasLST/', 
                            postFijoNomArchSalida = '', shpBase = shpMask$shp, escalas = escalas, nCols = 3, byRow = T, replot = T, heightPx = 620,
                            alturaEscalaContinua = unit(x=1, units = 'in'))
}

plotVariograma <- function(psill=1, range=1, nugget = 0.05, model='Sph', simularVariogramaEmpirico=F, archivoSalida=NULL, 
                           maxDist = range * 1.2, annotateNugget=T, annotatePsill=T, annotateRange=T, titulo='') {
  if (!is.null(archivoSalida)) {
    require('Cairo')
    Cairo(file = archivoSalida, bg='white')
  }
  
  require('gstat')
  par(xpd=TRUE)
  vg <- vgm(psill = psill, range = range, nugget = nugget, model = model)
  maxY <- (psill+nugget) * 1.2
  vgL <- variogramLine(vg, maxdist = maxDist)
  
  plot(x=vgL$dist, y=vgL$gamma, xlim=c(0, maxDist), ylim=c(0, maxY), xaxs="i",yaxs="i", type='l', axes=F, frame.plot=TRUE, 
       xlab='Distancia', ylab='Semivarianza', col='red', main=titulo)
  
  if (simularVariogramaEmpirico) {
    set.seed(31)
    nSimEmpiricalVG <- 14
    simEmpiricalVGDist <- vgL$dist[seq.int(from = 1, to=length(vgL$dist), length.out = nSimEmpiricalVG)]
    simEmpiricalVGGamma <- vgL$gamma[seq.int(from = 1, to=length(vgL$dist), length.out = nSimEmpiricalVG)] + rnorm(nSimEmpiricalVG, mean = 0, sd = .1)
    points(x=simEmpiricalVGDist, y = simEmpiricalVGGamma, pch=19)
  }
  
  if (annotateNugget) {
    points(x=0, y=0, type = "p", pch=20, col='red')
    text(x = - maxDist * 0.06, y = nugget * 1.1, label='Nugget')
  }
  text(x = 0, y = -0.05, label='0')
  
  if (annotatePsill) text(x = maxDist * 1.03, y = psill * 1.1, label='Psill')
  if (annotateRange) text(x = range, y = maxY * 1.05, label='Rango')
  
  par(xpd=FALSE)
  if (annotatePsill) abline(h=psill+nugget, col = "black", lwd=1, lty=2)
  if (annotateRange) abline(v=range, col = "black", lwd=1, lty=3)
  
  if (!is.null(archivoSalida)) dev.off()
}

ejemplosVariogramas <- function() {
  if (F) {
    plotVariograma(simularVariogramaEmpirico = T, archivoSalida = 'Resultados/Ejemplos/Variogramas/variogramaAcotado.png')
    
    Cairo(file = 'Resultados/Ejemplos/Variogramas/VariogramasComportamientoEnElOrigen.png', bg='white')
    par(mfrow=c(2,2))
    plotVariograma(model = 'Gau', nugget = 0, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'a) Parabólico')
    plotVariograma(model = 'Exp', nugget = 0, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'b) Lineal')
    plotVariograma(model = 'Exp', nugget = 0.4, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'c) Efecto Nugget')
    plotVariograma(model = 'Exp', range=1E-4, psill = 0, nugget = 1, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'd) Efecto Nugget Puro')
    par(mfrow=c(1,1))  
    dev.off()
    
    Cairo(file = 'Resultados/Ejemplos/Variogramas/VariogramasComportamientoAGrandesDistancias.png', bg='white', width = 1024)
    grid.arrange(
      plotVariogramaGGPlot(simularVariogramaEmpirico = T),
      plotVariogramaGGPlot(simularVariogramaEmpirico = T, model = 'Pow', range=1.2),
      ncol=2)
    dev.off()
  }
  
  # Nueva versión con GGPlot
  plotVariogramaGGPlot(simularVariogramaEmpirico = T, archivoSalida = 'Resultados/Ejemplos/Variogramas/variogramaAcotado.png', titulo = 'Variograma Empírico y Ajuste Teórico')
  plotVariogramaGGPlot(simularVariogramaEmpirico = T, model = 'Pow', range=1.2, archivoSalida = 'Resultados/Ejemplos/Variogramas/variogramaNoAcotado.png')
  
  Cairo(file = 'Resultados/Ejemplos/Variogramas/VariogramasComportamientoEnElOrigen.png', bg='white')
  grid.arrange(
    plotVariogramaGGPlot(model = 'Gau', nugget = 0, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'a) Parabólico'),
    plotVariogramaGGPlot(model = 'Exp', nugget = 0, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'b) Lineal'),
    plotVariogramaGGPlot(model = 'Exp', nugget = 0.4, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'c) Lineal + Efecto Nugget'),
    plotVariogramaGGPlot(model = 'Exp', range=1E-4, psill = 0, nugget = 1, maxDist = 3, annotateNugget = F, annotatePsill = F, annotateRange = F, titulo = 'd) Efecto Nugget Puro'),
    ncol=2)
  dev.off()
}

plotVariogramaGGPlot <- function(psill=1, range=1, nugget = 0.1, model='Sph', simularVariogramaEmpirico=F, archivoSalida=NULL, 
                                 maxDist = range * 1.2, annotateNugget=T, annotatePsill=T, annotateRange=T, titulo='') {
  # model = 'Pow'
  # range = 1.5
  vg <- vgm(psill = psill, range = range, nugget = nugget, model = model)
  vgL <- variogramLine(vg, maxdist = maxDist)
  maxY <- max(psill + nugget, vgL$gamma[length(vgL$gamma)]) * 1.2
  
  xyLims <- crearXYLims(xMin = 0, xMax = maxDist, yMin = 0, yMax = maxY)
  
  p <- linePlot(x = vgL$dist, y = vgL$gamma, titulo = titulo, tituloEjeX = 'Distancia', tituloEjeY = 'Semivarianza', 
                xyLims = xyLims, grosorLineas = 1, colores = 'red',  sinFondo = TRUE, dibujar = F) +
    theme(panel.background = element_rect(colour = "black", fill=NA, size=1), axis.line=element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) + 
    annotation_custom(grob = textGrob(label = '0', hjust = 0), ymin = -0.03, ymax = -0.03, xmin = -0.01, xmax = -0.01)
  
  if (simularVariogramaEmpirico) {
    set.seed(31)
    nSimEmpiricalVG <- 14
    simEmpiricalVGDist <- vgL$dist[seq.int(from = 1, to=length(vgL$dist), length.out = nSimEmpiricalVG)]
    simEmpiricalVGGamma <- vgL$gamma[seq.int(from = 1, to=length(vgL$dist), length.out = nSimEmpiricalVG)] + rnorm(nSimEmpiricalVG, mean = 0, sd = .1)
    p <- p + geom_point(data=data.frame(x=simEmpiricalVGDist, y=simEmpiricalVGGamma))
  }
  
  if (annotateNugget) {
    leftMargin <- 3
    p <- p + annotation_custom(grob = textGrob(label = 'Nugget', hjust = 0),
                               ymin = nugget + 0.01, ymax = nugget + 0.01, 
                               xmin = -0.15, xmax = -0.15) + 
      geom_point(data = data.frame(x=0, y=nugget), color='red', size=3)
  } else { leftMargin <- 1 }
  
  if (annotatePsill & model != 'Pow') {
    rightMargin <- 3
    p <- p + geom_hline(yintercept=nugget + psill, linetype="dashed", color = "black") + 
             annotation_custom(grob = textGrob(label = 'Sill', hjust = 0),
                               ymin = nugget + psill + 0.01, ymax = nugget + psill + 0.01, 
                               xmin = maxDist + 0.02, xmax = maxDist + 0.02)
  } else { rightMargin <- 1 }
  
  if (annotateRange & model != 'Pow') {
    p <- p + geom_vline(xintercept=range, linetype="dashed", color = "black") + 
             annotation_custom(grob = textGrob(label = 'Rango', hjust = 0),
                               ymin = -0.03, ymax = -0.03, xmin = range, xmax = range - 0.08)
  }
  p <- p + theme(plot.margin = unit(c(1,rightMargin,1,leftMargin), "lines"))
  
  if (!is.null(archivoSalida)) {
    require('Cairo')
    Cairo(file = archivoSalida, bg='white')
  }
  
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
  
  if (!is.null(archivoSalida)) dev.off()
  return(gt)
}

plotComportamientoEnElOrigen <- function() {
  variograma <- data.frame(np = 10, 
                           dist = c(7, 12, 17, 22, 27, 32),
                           gamma = c(1.8, 2.4, 2.7, 3.05, 2.8, 2.95),
                           dir.hor = 0, dir.ver = 0, id = 'var1', stringsAsFactors = F)
  class(variograma) <- c('gstatVariogram', 'data.frame')
  
  sph <- getModelVariogram(variograma, formula = 'value~1', model = 'Sph', fix.values = c(0, NA, NA), tryFixNugget = F)$var_model
  #exponencial <- getModelVariogram(variograma, formula = 'value~1', model = 'Exp', fix.values = c(0, NA, NA), tryFixNugget = F)$var_model
  sphNug <- getModelVariogram(variograma, formula = 'value~1', model = 'Sph', tryFixNugget = F)$var_model
  gauNug <- getModelVariogram(variograma, formula = 'value~1', model = 'Gau', tryFixNugget = F)$var_model
  
  x <- seq(0.01, max(variograma$dist), length.out = 100)
  ySph <- variogramLine(sph, dist_vector = x)
  #yExp <- variogramLine(exponencial, dist_vector = x)
  ySphNug <- variogramLine(sphNug, dist_vector = x)
  yGauNug <- variogramLine(gauNug, dist_vector = x)
  
  #yes <- cbind(ySph$gamma, yExp$gamma, ySphNug$gamma, yGauNug$gamma)
  #nombresSeries <- c('Esférico', 'Exponencial', 'Esférico + Nugget', 'Gaussiano + Nugget')
  
  yes <- cbind(ySph$gamma, ySphNug$gamma, yGauNug$gamma)
  nombresSeries <- c('a) Esférico', 'b) Esférico + Nugget', 'c) Gaussiano + Nugget')
    
  g <- linePlot(x = x, y = yes, 
                titulo = 'Variogramas - Comportamiento en Origen', tituloEjeX = 'Distancia', tituloEjeY = 'Semivarianza',
                tiposDeLinea = getTiposDeLineaParaSeries(yes, TRUE), nombresSeries = nombresSeries, nombreVariableGrupo = 'Caso', 
                dibujar=F) + 
       geom_point(data=data.frame(x=variograma$dist, value=variograma$gamma))
  g
  guardarGrafico(g, widthPx = 1024, heightPx = 600, nomArchSalida = 'Resultados/Ejemplos/variogramaComportamientosEnElOrigen.png')
}

plotFiltrados <- function() {
  paths <- cbind(dir(paste(pathDatos, 'MODIS/LST_Night_Combinada', sep=''), pattern = '*.tif$', full.names = T),                # datos crudos
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_median/%03d.tif', 1:365), sep='')[diasDelAnio],            # mediana
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_sd/%03d.tif', 1:365), sep='')[diasDelAnio],                # sd
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.005/%03d.tif', 1:365), sep='')[diasDelAnio],            # q0.005
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.9999/%03d.tif', 1:365), sep='')[diasDelAnio],           # q0.9999
                 dir(paste(pathDatos, 'LST_Night_Combinada_0.0050_0.9999', sep=''), pattern = '*.tif$', full.names = T),  # filtrados 1
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.010/%03d.tif', 1:365), sep='')[diasDelAnio],            # q0.010
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.9995/%03d.tif', 1:365), sep='')[diasDelAnio],           # q0.9995
                 dir(paste(pathDatos, 'LST_Night_Combinada_0.0100_0.9995', sep=''), pattern = '*.tif$', full.names = T),  # filtrados 2
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.015/%03d.tif', 1:365), sep='')[diasDelAnio],            # q0.015
                 paste(pathDatos, sprintf('LST_Night_Combinada_Clim_q0.9990/%03d.tif', 1:365), sep='')[diasDelAnio],           # q0.9990
                 dir(paste(pathDatos, 'LST_Night_Combinada_0.0150_0.9990', sep=''), pattern = '*.tif$', full.names = T)   # filtrados 3
  )
  dim(paths)
  colnames(paths) <- c('LST Night Combinada', 
                       'Mediana Climatologica', 
                       'Desviación Estándar Climatológica',
                       'Q0.0050', 
                       'Q0.9999',
                       'Filtrado Q0.0050-Q0.9999',
                       'Q0.0100', 
                       'Q0.9995',
                       'Filtrado Q0.0100-Q0.9995',
                       'Q0.0150', 
                       'Q0.9990',
                       'Filtrado Q0.0150-Q0.9990')
  
  carpetaSalida <- 'Resultados/Filtrado/'
  
  muestras <- muestrearValores(pathsRaster = paths[, 1], nMuestras = 25000)
  muestras <- muestras[!is.na(muestras)]
  sMuestras <- sort(muestras)
  lowTrim <- 1
  highTrim <- 0
  lowTrim <- length(sMuestras) * 0.5 / 100
  highTrim <- length(sMuestras) * 0.05 / 100
  #plot(ecdf(sMuestras[lowTrim:(length(sMuestras)-highTrim)]))
  #f <- ecdf(sMuestras[lowTrim:(length(sMuestras)-highTrim)])
  #f(30)
  #f(-3)
  #quantile(sMuestras[lowTrim:(length(sMuestras)-highTrim)], probs=seq(from=0, to=1, length.out = 11))
  #seq(from=sMuestras[iTrim], to = sMuestras[lowTrim:(length(sMuestras)-highTrim)], length.out=11)
  escala <- crearEscalaEquiespaciada(sMuestras[c(lowTrim,(length(sMuestras)-highTrim))], nIntervalos = 10, continuo = T)
  rm(muestras, sMuestras)
  
  escalas <- list()
  length(escalas) <- ncol(paths)
  for (i in 1:length(escalas)) escalas[[i]] <- escala
  
  plotMultiRastersEnPaneles(pathsRasters = paths, fechasRasters = fechasObservaciones, carpetaSalida = carpetaSalida, shpBase = shpMask$shp, 
                            escalas = escalas, nCols = 3, replot = F, nCoresAUsar = 0, widthPx = 1920*2, heightPx = 1080*2, 
                            alturaEscalaContinua = unit(x=0.7, units = 'in'))
}

plotFiltradaYCombinada <- function() {
  colnames(pathsRegresores)
  paths <- pathsRegresores[, c('MOD11A1_LST_Night', 'MYD11A1_LST_Night', 'LST_Night_Combinada', 
                               'MOD11A1_LST_Night_filtrado', 'MYD11A1_LST_Night_filtrado', 'LST_Night_Filtrada_Combinada')]
  
  carpetaSalida <- 'Resultados/FiltradaYCombinada/'
  
  muestras <- muestrearValores(pathsRaster = paths[, 1], nMuestras = 25000)
  muestras <- muestras[!is.na(muestras)]
  sMuestras <- sort(muestras)
  lowTrim <- 1
  highTrim <- 0
  lowTrim <- length(sMuestras) * 0.5 / 100
  highTrim <- length(sMuestras) * 0.05 / 100
  #plot(ecdf(sMuestras[lowTrim:(length(sMuestras)-highTrim)]))
  #f <- ecdf(sMuestras[lowTrim:(length(sMuestras)-highTrim)])
  #f(30)
  #f(-3)
  #quantile(sMuestras[lowTrim:(length(sMuestras)-highTrim)], probs=seq(from=0, to=1, length.out = 11))
  #seq(from=sMuestras[iTrim], to = sMuestras[lowTrim:(length(sMuestras)-highTrim)], length.out=11)
  escala <- crearEscalaEquiespaciada(sMuestras[c(lowTrim,(length(sMuestras)-highTrim))], nIntervalos = 10, continuo = T)
  rm(muestras, sMuestras)
  
  escalas <- list()
  length(escalas) <- ncol(paths)
  for (i in 1:length(escalas)) escalas[[i]] <- escala
  
  plotMultiRastersEnPaneles(pathsRasters = paths, fechasRasters = fechasObservaciones, carpetaSalida = carpetaSalida, shpBase = shpMask$shp, 
                            escalas = escalas, nCols = 3, replot = T, nCoresAUsar = 0, widthPx = 1920*2, heightPx = 1080*2, 
                            alturaEscalaContinua = unit(x=0.075, units = 'npc'))
}
