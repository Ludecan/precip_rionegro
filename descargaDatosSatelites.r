iFrame <- sys.nframe()
if (iFrame >= 3) { script.dir.descargaDatosSatelites <- sys.frame(iFrame - 3)$ofile
} else { script.dir.descargaDatosSatelites <- NULL }
while ((is.null(script.dir.descargaDatosSatelites) || is.na(regexpr('descargaDatosSatelites.r', script.dir.descargaDatosSatelites, fixed=T)[1])) && iFrame >= 0) {
  script.dir.descargaDatosSatelites <- sys.frame(iFrame)$ofile
  iFrame <- iFrame - 1
}
if (is.null(script.dir.descargaDatosSatelites)) { script.dir.descargaDatosSatelites <- ''
} else { script.dir.descargaDatosSatelites <- paste0(dirname(script.dir.descargaDatosSatelites), '/') }

source(paste0(script.dir.descargaDatosSatelites, '/st_interp/instalarPaquetes/instant_pkgs.r'), encoding = 'WINDOWS-1252')
instant_pkgs(c('jsonlite', 'R.utils', 'lubridate', 'benchmarkme'))

source(paste0(script.dir.descargaDatosSatelites, '/st_interp/descargador/descargadorEx.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatosSatelites, '/st_interp/GrADS/ReadGrADS.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatosSatelites, '/st_interp/agregacion/agregacion.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatosSatelites, '/st_interp/sysutils/sysutils.r'), encoding = 'WINDOWS-1252')

descargaGSMaP <- function(
  dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 7 * 24*60*60, dt_fin=date(now()),
  horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/GSMaP/', shpBase=NULL,
  forzarReDescarga=FALSE, borrarDatosOriginales=FALSE,
  urlBase='ftp://hokusai.eorc.jaxa.jp/realtime_ver/', productVersion='v7',
  producto='hourly_G', verbose=TRUE
) {
  # Los datos de GSMaP siguen este formato de nombres:
  # gsmap_gauge.20211226.1700.dat.gz
  # El período de observación corresponde a (2021-12-26 16:00 UTC, 2021-12-26 17:00 UTC]?
  
  # fijo las hora inicial/final
  dt_ini_gsmap <- sprintf('%s %02d:00', date(dt_ini) - 1, horaUTCInicioAcumulacion + 1)
  dt_fin_gsmap <- sprintf('%s %02d:00', date(dt_fin), horaUTCInicioAcumulacion)
  
  # Descargo y parseo el CTL
  nomArchCTL <- paste0('GSMaP_NRT.', producto, '.rain.ctl')
  pathLocalCTL <- paste0(pathSalida, nomArchCTL)
  descargarArchivos(
    urls=paste0(urlBase, productVersion, '/sample/', nomArchCTL), 
    nombresArchivosDestino=pathLocalCTL, 
    forzarReDescarga=forzarReDescarga,
    maxRetries=1, 
    segundosEntreIntentos=3, 
    curlOpts=list(netrc=1)
  )
  ctl <- parseCTL(ctlFile=pathLocalCTL, convert360to180=TRUE)

  # Armo urls y pathsLocales horarios
  horas <- seq(as.POSIXct(dt_ini_gsmap), as.POSIXct(dt_fin_gsmap), by="hour")
  urls <- strftime(
    x=horas, 
    format=paste0(
      urlBase, productVersion, '/', producto, '/%Y/%m/%d/gsmap_gauge.%Y%m%d.%H%M.dat.gz'
    )
  )
  urlBasenames <- basename(urls)
  pathsLocales <- paste0(pathSalida, productVersion, '/originales/', urlBasenames)
  pathsLocalesDescomprimidos <- substr(pathsLocales, start=1, stop=nchar(pathsLocales) - 3)
  # write(toJSON(authInfo), 'GSMaP_authInfo.json')
  
  # Armo paths locales diarios para la agregación
  dias <- seq(as.POSIXct(dt_ini_gsmap), as.POSIXct(dt_fin_gsmap), by="day") + lubridate::days(1)
  pathsLocalesDiarios <- strftime(x=dias, format=paste0(pathSalida, productVersion, '/%Y%m%d.tif'))

  # Busco los paths locales diarios que no existan, los que sean previos a la mínima 
  # fecha disponible se tratan como existentes para que no los descargue pero retorne 
  # sus paths como NA
  iNoExisten <- which(!file.exists(pathsLocalesDiarios) | file.info(pathsLocalesDiarios)$size <= 0)
  
  if (length(iNoExisten) > 0) {
    # Descargo los archivos horarios de los paths diarios que no existan
    iHorasADescargar <- rep(1:24, length(iNoExisten))
    for (i in seq_along(iNoExisten)) { 
      iHorasADescargar[(24*(i-1) + 1):(24*i)] <- iHorasADescargar[(24*(i-1) + 1):(24*i)] + (iNoExisten[i]-1)*24
    }
    
    strHoraUTCInicioAcumulacion <- sprintf('%02d00', horaUTCInicioAcumulacion)
    fechaMax <- paste0(
      gsub(pattern='-', x=today() + 1, replacement=''), '.', strHoraUTCInicioAcumulacion
    )
    availableDates <- list(
      v7=list(min=paste0('20170401.', strHoraUTCInicioAcumulacion), max=fechaMax),
      v8=list(min=paste0('20211201.', strHoraUTCInicioAcumulacion), max=fechaMax)
    )
    
    urlDates <- regmatches(
      x=urls[iHorasADescargar], 
      m=regexpr(pattern='[[:digit:]]{8}.[[:digit:]]{4}', text=urls[iHorasADescargar])
    )
    iHorasADescargar <- iHorasADescargar[
      (urlDates >= availableDates[[productVersion]][['min']]) &
      (urlDates <= availableDates[[productVersion]][['max']])
    ]
    
    nCoresAUsar <- getAvailableCores(maxCoresPerGB = 1)
    nConexionesSimultaneas <- min(10, nCoresAUsar)
    
    if (verbose) {
      print(paste0(
        Sys.time(), " - Downloading ", length(iHorasADescargar), " files for ", length(iNoExisten), 
        " days. Output Path: ", pathSalida, productVersion, "/"))
      if (length(pathsLocalesDiarios) == 1) {
        print(paste0(
          Sys.time(), " - Daily accumulation will be saved to: ", pathsLocalesDiarios[1]))
      }
    }
    
    res <- descargarArchivos(
      urls=urls[iHorasADescargar], nombresArchivosDestino=pathsLocales[iHorasADescargar],
      maxRetries=2, segundosEntreIntentos=5, curlOpts=list(netrc=1), 
      nConexionesSimultaneas=nConexionesSimultaneas, forzarReDescarga=forzarReDescarga,
      do_unzip=rep(TRUE, length(iHorasADescargar))
    )
    if (any(res == 0)) {
      warning(paste('Error downloading GSMaP files:', 
                    paste(urls[iHorasADescargar[res == 0]], collapse = '\n'), 
                    sep='\n'))
    }
    
    if (verbose) {
      print(paste0(
        sum(res == 1), " new files downloaded, ", sum(res == 2), " already existed, ", 
        sum(res == 0), " could not be downloaded."))
    }
    
    if (length(iHorasADescargar) > 0) {
      agregacionTemporalGrillada(
        fechas=horas[iHorasADescargar],
        pathsRegresor=pathsLocalesDescomprimidos[iHorasADescargar],
        formatoNomArchivoSalida=paste(pathSalida, productVersion, '/%Y%m%d.tif', sep=''),
        minNfechasParaAgregar=24, 
        nFechasAAgregar=24, 
        funcionAgregacion=base::sum, 
        ctl=ctl, 
        shpBase=shpBase,
        overlap=FALSE, 
        nCoresAUsar=nCoresAUsar, 
        archivoSalidaUsaFechaFinal=TRUE,
        borrarArchivosFallidos=FALSE
      )
    }
    if (borrarDatosOriginales) {
      unlink(pathsLocales)
      unlink(pathsLocalesDescomprimidos)
    }
  }
  
  paths <- matrix(
    data = pathsLocalesDiarios,
    ncol = 1, 
    byrow = T,
    dimnames = list(
      as.character(date(dias)),
      paste(basename(pathSalida), productVersion, sep='_')
    )
  )
  paths[!file.exists(paths[, 1]), ] <- NA
  
  return(paths)
}

descargaIMERG <- function(
  dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 1 * 24*60*60, dt_fin=date(now()),
  horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/IMERG/', shpBase=NULL,
  productVersion='V06B', forzarReDescarga=FALSE, borrarDatosOriginales=FALSE,
  urlBase='ftp://jsimpsonftps.pps.eosdis.nasa.gov/data/imerg/', producto='gis',
  verbose=TRUE
) {
  # Los datos de GPM-Imerg siguen este formato de nombres:
  # 3B-HHR-L.MS.MRG.3IMERG.20170201-S100000-E102959.0600.V06B.30min.tif
  # El período de observación corresponde a (2017-02-01 10:00 UTC, 2017-02-01 10:29:59 UTC]?
  
  # fijo las horas inicial/final
  dt_ini_gpm <- sprintf('%s %02d:00', date(dt_ini) - 1, horaUTCInicioAcumulacion)
  dt_fin_gpm <- sprintf('%s %02d:30', date(dt_fin), horaUTCInicioAcumulacion - 1)
  
  formatoPrefijo <- paste0(urlBase, producto, '/%Y/%m/3B-HHR-L.MS.MRG.3IMERG.%Y%m%d-S%H%M%S')
  formatoE <- '-E%H%M%S.'
  formatoPostfijo <- paste0('%04d.', productVersion, '.30min.tif')
  
  # Armo urls y pathsLocales horarios
  mediasHoras <- seq(as.POSIXct(dt_ini_gpm), as.POSIXct(dt_fin_gpm) + 30 * 60, by="30 mins")
  urls <- paste0(
    strftime(x=head(mediasHoras, -1), format=formatoPrefijo),
    strftime(x=tail(mediasHoras, -1) - 1, format=formatoE),
    sprintf(fmt = formatoPostfijo, (head(seq_along(mediasHoras), -1) + 2 * horaUTCInicioAcumulacion -1) %% 48 * 30)
  )
  pathsLocales <- paste0(pathSalida, productVersion, '/originales/', basename(urls))
  do_unzip = rep(FALSE, length(urls))
  
  # Armo paths locales diarios para la agregación
  dias <- seq(as.POSIXct(dt_ini_gpm), as.POSIXct(dt_fin_gpm), by="day") + lubridate::days(1)
  pathsLocalesDiarios <- strftime(x=dias, format=paste0(pathSalida, productVersion, '/%Y%m%d.tif'))
  
  # Busco los paths locales diarios que no existan
  iNoExisten <- which(!file.exists(pathsLocalesDiarios) | file.info(pathsLocalesDiarios)$size <= 0)
  if (length(iNoExisten) > 0) {
    # Descargo los archivos horarios de los paths diarios que no existan
    numPeriodos <- 48
    iPeriodosADescargar <- rep(1:numPeriodos, length(iNoExisten))
    for (i in seq_along(iNoExisten)) { 
      idx <- (numPeriodos*(i-1) + 1):(numPeriodos*i)
      iPeriodosADescargar[idx] <- iPeriodosADescargar[idx] + (iNoExisten[i]-1)*numPeriodos
    }
    
    strHoraUTCInicioAcumulacion <- sprintf('%02d0000', horaUTCInicioAcumulacion)
    fechaMax <- paste0(
      gsub(pattern='-', x=today() + 1, replacement=''), '.', strHoraUTCInicioAcumulacion
    )
    availableDates <- list(
      V06B=list(min=paste0('20000608-', strHoraUTCInicioAcumulacion), max=fechaMax)
    )
    
    urlDates <- regmatches(
      x=urls[iPeriodosADescargar], 
      m=regexpr(pattern='[[:digit:]]{8}-S[[:digit:]]{6}', text=urls[iPeriodosADescargar])
    )
    iPeriodosADescargar <- iPeriodosADescargar[
      (urlDates >= availableDates[[productVersion]][['min']]) &
        (urlDates <= availableDates[[productVersion]][['max']])
    ]    
    
    nCoresAUsar <- getAvailableCores(maxCoresPerGB = 1)
    nConexionesSimultaneas <- min(10, nCoresAUsar)

    if (verbose) {
      print(paste0(
        Sys.time(), " - Downloading ", length(iPeriodosADescargar), " files for ", 
        length(iNoExisten), " days. Output Path: ", pathSalida, productVersion, "/"))
      if (length(pathsLocalesDiarios) == 1) {
        print(paste0(
          Sys.time(), " - Daily accumulation will be saved to: ", pathsLocalesDiarios[1]))
      }
    }

    curlOpts <- list(use_ssl=3, netrc=1, timeout=600L, connecttimeout=600L)
    res <- descargarArchivos(
      urls=urls[iPeriodosADescargar], nombresArchivosDestino=pathsLocales[iPeriodosADescargar],
      forzarReDescarga=forzarReDescarga, maxRetries=2, segundosEntreIntentos=5, 
      curlOpts=curlOpts, nConexionesSimultaneas=nConexionesSimultaneas, 
      do_unzip=do_unzip[iPeriodosADescargar]
    )
    if (any(res == 0)) {
      warning(paste('Error downloading IMERG files:', 
                    paste(urls[iPeriodosADescargar[res == 0]], collapse = '\n'), 
                    sep='\n'))
    }
    
    if (verbose) {
      print(paste0(
        sum(res == 1), " new files downloaded, ", sum(res == 2), " already existed, ", 
        sum(res == 0), " could not be downloaded."))
    }
    
    if (length(iPeriodosADescargar) > 0) {
      agregacionTemporalGrillada(
        fechas=mediasHoras[iPeriodosADescargar], 
        pathsRegresor=pathsLocales[iPeriodosADescargar],
        formatoNomArchivoSalida=paste0(pathSalida, productVersion, '/%Y%m%d.tif'),
        minNfechasParaAgregar=numPeriodos, nFechasAAgregar=numPeriodos, 
        funcionAgregacion=base::sum, shpBase=shpBase, overlap=FALSE, 
        funcEscalado=function(x) { x / 20 }, nCoresAUsar=nCoresAUsar, 
        archivoSalidaUsaFechaFinal=TRUE,
        borrarArchivosFallidos=FALSE
      )
    }
    if (borrarDatosOriginales) {
      unlink(pathsLocales)
    }
  }
  
  paths <- matrix(
    data = pathsLocalesDiarios,
    ncol = 1, 
    byrow = T,
    dimnames = list(
      as.character(date(dias)),
      paste(basename(pathSalida), productVersion, sep='_')
    )
  )
  paths[!file.exists(paths[, 1]), ] <- NA
  
  return(paths)
}
