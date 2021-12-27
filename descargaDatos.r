iFrame <- sys.nframe()
if (iFrame >= 3) { script.dir.descargaDatos <- sys.frame(iFrame - 3)$ofile
} else { script.dir.descargaDatos <- NULL }
while ((is.null(script.dir.descargaDatos) || is.na(regexpr('descargaDatos.r', script.dir.descargaDatos, fixed=T)[1])) && iFrame >= 0) {
  script.dir.descargaDatos <- sys.frame(iFrame)$ofile
  iFrame <- iFrame - 1
}
if (is.null(script.dir.descargaDatos)) { script.dir.descargaDatos <- ''
} else { script.dir.descargaDatos <- paste0(dirname(script.dir.descargaDatos), '/') }

source(paste0(script.dir.descargaDatos, '/st_interp/instalarPaquetes/instant_pkgs.r'), encoding = 'WINDOWS-1252')
instant_pkgs(c('jsonlite', 'R.utils', 'lubridate', 'benchmarkme'))

source(paste0(script.dir.descargaDatos, '/st_interp/descargador/descargadorEx.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatos, '/st_interp/GrADS/ReadGrADS.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatos, '/st_interp/agregacion/agregacion.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatos, '/st_interp/sysutils/sysutils.r'), encoding = 'WINDOWS-1252')

descargaPluviosADME <- function(
    dt_ini=dt_fin, dt_fin=date(now()), pathSalida='datos/pluviometros/',
    forzarReDescarga=FALSE) {
  url_medidas_pluvios <- Sys.getenv(x='URL_MEDIDAS_PLUVIOS')
  if (url_medidas_pluvios == '') {
    stop(paste0(
      'La variable de entorno URL_MEDIDAS_PLUVIOS no se encuentra definida. ',
      'Defina su valor y vuelva a intentarlo'))
  }
  url <- paste0(url_medidas_pluvios, '?dtIni=', dt_ini, '&dtFin=', dt_fin)
  
  sonEstacionesConvencionales <- endsWith(url_medidas_pluvios, 'Convencionales.php')
  if (sonEstacionesConvencionales) {
    postfijo <- '_convencionales'
  } else {
    postfijo <- ''
  }
  localFile <- paste0(
    pathSalida, gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall', 
    postfijo, '.xlsx')
  
  descargarArchivos(
    urls=url, nombresArchivosDestino=localFile, curlOpts=list(use_ssl = 3), 
    forzarReDescarga=forzarReDescarga)
  return(localFile)
}

descargaGSMaP <- function(
  dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 7 * 24*60*60, dt_fin=date(now()),
  horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/GSMaP/', shpBase=NULL,
  forzarReDescarga=FALSE, borrarDatosOriginales=FALSE,
  urlBase='ftp://hokusai.eorc.jaxa.jp/realtime_ver/', productVersion='v7', 
  producto='hourly_G', verbose=TRUE
) {
  # Los datos de GSMaP siguen este formato de nombres:
  # gsmap_gauge.20211226.1700.dat.gz
  # El período de observación corresponde a (2021-12-26 17:00 UTC, 2021-12-26 18:00 UTC]?
  
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
  pathsLocales <- paste0(pathSalida, 'originales/', basename(urls))
  pathsLocalesDescomprimidos <- substr(pathsLocales, start=1, stop=nchar(pathsLocales) - 3)
  # write(toJSON(authInfo), 'GSMaP_authInfo.json')
  
  # Armo paths locales diarios para la agregación
  dias <- seq(as.POSIXct(dt_ini_gsmap), as.POSIXct(dt_fin_gsmap), by="day") + lubridate::days(1)
  pathsLocalesDiarios <- strftime(x=dias, format=paste0(pathSalida, '%Y%m%d.tif'))
  
  # Busco los paths locales diarios que no existan
  iNoExisten <- which(!file.exists(pathsLocalesDiarios) | file.info(pathsLocalesDiarios)$size <= 0)
  if (length(iNoExisten) > 0) {
    # Descargo los archivos horarios de los paths diarios que no existan
    iHorasADescargar <- rep(1:24, length(iNoExisten))
    for (i in seq_along(iNoExisten)) { 
      iHorasADescargar[(24*(i-1) + 1):(24*i)] <- iHorasADescargar[(24*(i-1) + 1):(24*i)] + (iNoExisten[i]-1)*24
    }
    
    nCoresAUsar <- getAvailableCores(maxCoresPerGB = 1)
    nConexionesSimultaneas <- min(10, nCoresAUsar)
    
    if (verbose) {
      print(paste0(
        Sys.time(), " - Downloading ", length(iHorasADescargar), " files for ", length(iNoExisten), 
        " days. Output Path: ", pathSalida))
      if (length(pathsLocalesDiarios) == 1) {
        print(paste0(
          Sys.time(), " - Daily accumulation will be saved to: ", pathsLocalesDiarios[1]))
      }
    }
    
    res <- descargarArchivos(
      urls=urls[iHorasADescargar], nombresArchivosDestino=pathsLocales[iHorasADescargar],
      maxRetries=1, segundosEntreIntentos=3, curlOpts=list(netrc=1), 
      nConexionesSimultaneas=nConexionesSimultaneas, forzarReDescarga=forzarReDescarga)
    if (any(res == 0)) {
      warning(paste('Error downloading GSMaP files:', 
                    paste(urls[iHorasADescargar[res == 0]], collapse = '\n'), 
                    sep='\n'))
      iHorasADescargar <- iHorasADescargar[res != 0]
    }
    
    if (verbose) {
      print(paste0(
        sum(res == 1), " new files downloaded, ", sum(res == 2), " already existed, ", 
        sum(res == 0), " could not be downloaded."))
    }
    
    if (length(iHorasADescargar) > 0) {
      agregacionTemporalGrillada(
        fechas=horas[iHorasADescargar], pathsRegresor=pathsLocalesDescomprimidos[iHorasADescargar],
        formatoNomArchivoSalida=paste(pathSalida, '%Y%m%d.tif', sep=''), minNfechasParaAgregar=24, 
        nFechasAAgregar=24, funcionAgregacion=base::sum, ctl=ctl, shpBase=shpBase,
        overlap=FALSE, nCoresAUsar=nCoresAUsar, archivoSalidaUsaFechaFinal=TRUE)
    }
    if (borrarDatosOriginales) {
      unlink(pathsLocales)
      unlink(pathsLocalesDescomprimidos)
    }
  }
  return(pathsLocalesDiarios)
}

descargaGPM <- function(
  dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 1 * 24*60*60, dt_fin=date(now()),
  horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/GPM/', shpBase=NULL,
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
  pathsLocales <- paste0(pathSalida, 'originales/', basename(urls))
  do_unzip = rep(FALSE, length(urls))
  
  # Armo paths locales diarios para la agregación
  dias <- seq(as.POSIXct(dt_ini_gpm), as.POSIXct(dt_fin_gpm), by="day") + lubridate::days(1)
  pathsLocalesDiarios <- strftime(x=dias, format=paste0(pathSalida, '%Y%m%d.tif'))
  
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
    
    nCoresAUsar <- getAvailableCores(maxCoresPerGB = 1)
    nConexionesSimultaneas <- min(10, nCoresAUsar)

    if (verbose) {
      print(paste0(
        Sys.time(), " - Downloading ", length(iPeriodosADescargar), " files for ", 
        length(iNoExisten), " days. Output Path: ", pathSalida))
      if (length(pathsLocalesDiarios) == 1) {
        print(paste0(
          Sys.time(), " - Daily accumulation will be saved to: ", pathsLocalesDiarios[1]))
      }
    }

    curlOpts <- list(use_ssl=3, netrc=1, timeout=600L, connecttimeout=600L)
    res <- descargarArchivos(
      urls=urls[iPeriodosADescargar], nombresArchivosDestino=pathsLocales[iPeriodosADescargar],
      forzarReDescarga=forzarReDescarga, maxRetries=1, segundosEntreIntentos=3, 
      curlOpts=curlOpts, nConexionesSimultaneas=nConexionesSimultaneas, 
      do_unzip=do_unzip[iPeriodosADescargar]
    )
    if (any(res == 0)) {
      warning(paste('Error downloading GSMaP files:', 
                    paste(urls[iPeriodosADescargar[res == 0]], collapse = '\n'), 
                    sep='\n'))
      iPeriodosADescargar <- iPeriodosADescargar[res != 0]
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
        formatoNomArchivoSalida=paste0(pathSalida, '%Y%m%d.tif'), 
        minNfechasParaAgregar=numPeriodos, nFechasAAgregar=numPeriodos, 
        funcionAgregacion=base::sum, shpBase=shpBase, overlap=FALSE, 
        funcEscalado=function(x) { x / 20 }, nCoresAUsar=nCoresAUsar, 
        archivoSalidaUsaFechaFinal=TRUE
      )
    }
    if (borrarDatosOriginales) {
      unlink(pathsLocales)
    }
  }
  return(pathsLocalesDiarios)
}
