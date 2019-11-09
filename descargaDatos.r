script.dir.descargaDatos <- dirname((function() { attr(body(sys.function()), "srcfile") })()$filename)

source(paste(script.dir.descargaDatos, '/st_interp/instalarPaquetes/instant_pkgs.r', sep=''))
instant_pkgs(c('jsonlite', 'R.utils', 'lubridate'))

source(paste(script.dir.descargaDatos, '/st_interp/descargador/descargadorEx.r', sep=''))
source(paste(script.dir.descargaDatos, '/st_interp/GrADS/ReadGrADS.r', sep=''))
source(paste(script.dir.descargaDatos, '/st_interp/agregacion/agregacion.r', sep=''))

descargaPluviosADME <- function(dt_ini=dt_fin, dt_fin=date(now()), pathSalida='datos/pluviometros/') {
  url <- paste('***REMOVED***?dtIni=', dt_ini, '&dtFin=', dt_fin, 
               sep = '')
  localFile <- paste(pathSalida, 
                     gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall.xlsx', sep = '')
  if (!file.exists(localFile) || file.info(localFile)$size == 0) {
    descargarArchivos(urls = url, nombresArchivosDestino = localFile, forzarReDescarga = T)
  }
  return(localFile)
}

descargaGSMaP <- function(
    dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 24*60*60, dt_fin=date(now()),
    horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/GSMaP/', shpBase=NULL) {
  urlBase <- 'ftp://hokusai.eorc.jaxa.jp/realtime_ver/v7/'
  producto <- 'hourly_G'
  
  authInfo <- fromJSON(txt = 'GSMaP_authInfo.json')
  
  # fijo la hora inicial
  dt_ini <- sprintf('%s %02d:00', date(dt_ini), horaUTCInicioAcumulacion + 1)
  dt_fin <- sprintf('%s %02d:00', date(dt_fin), horaUTCInicioAcumulacion)
  
  # Descargo y parseo el CTL
  nomArchCTL <- paste('GSMaP_NRT.', producto, '.rain.ctl', sep = '')
  pathLocalCTL <- paste(pathSalida, nomArchCTL, sep = '')
  if (!file.exists(pathLocalCTL) | file.info(pathLocalCTL)$size <= 0) {
    res <- descargarArchivos(urls = paste(urlBase, 'sample/', nomArchCTL, sep = ''), 
                             nombresArchivosDestino = pathLocalCTL, authInfo = authInfo)  
  }
  ctl <- parseCTL_V2(ctlFile = pathLocalCTL)
  # Corrijo la longitud a estar en -180, 180. Esto se podría pasaer directo al parseCTL_v2
  ctl$xdef$from <- ctl$xdef$from - 180
  ctl$xdef$vals <- ctl$xdef$vals - 180
  
  
  # Armo urls y pathsLocales horarios
  horas <- seq(as.POSIXct(dt_ini), as.POSIXct(dt_fin), by="hour")
  urls <- strftime(x = horas, 
                   format = paste(urlBase, producto, '/%Y/%m/%d/gsmap_gauge.%Y%m%d.%H%M.dat.gz', sep=''))
  pathsLocales <- paste(pathSalida, basename(urls), sep='')
  pathsLocalesDescomprimidos <- substr(pathsLocales, start = 1, stop = nchar(pathsLocales) - 3)
  # write(toJSON(authInfo), 'GSMaP_authInfo.json')
  
  # Armo paths locales diarios para la agregación
  dias <- seq(as.POSIXct(dt_ini), as.POSIXct(dt_fin), by="day")
  pathsLocalesDiarios <- strftime(x = dias, format = paste(pathSalida, '%Y%m%d.tif', sep=''))
  
  # Busco los paths locales diarios que no existan
  iNoExisten <- which(!file.exists(pathsLocalesDiarios) | file.info(pathsLocalesDiarios)$size <= 0)
  if (length(iNoExisten) > 0) {
    # Descargo los archivos horarios de los paths diarios que no existan
    iHorasADescargar <- rep(1:24, length(iNoExisten))
    for (i in seq_along(iNoExisten)) { 
      iHorasADescargar[(24*(i-1) + 1):(24*i)] <- iHorasADescargar[(24*(i-1) + 1):(24*i)] + (iNoExisten[i]-1)*24
    }
    # Si ya existe el archivo o el descomprimido, lo omito de las descargas
    idx <- (!file.exists(pathsLocales[iHorasADescargar]) | file.info(pathsLocales[iHorasADescargar])$size <= 0) |
           (!file.exists(pathsLocalesDescomprimidos[iHorasADescargar]) | file.info(pathsLocalesDescomprimidos[iHorasADescargar])$size <= 0)
    
    res <- descargarArchivos(
      urls = urls, nombresArchivosDestino = pathsLocales[iHorasADescargar][idx], authInfo = authInfo)
    if (any(res == 0)) {
      stop(paste('Error downloading GSMaP files:', paste(urls[res == 0], collapse = ', ')))
    }
    
    # Descomprimo en paralelo
    nCoresAUsar <- min(detectCores(T, T), length(pathsLocales))
    cl <- makeCluster(getOption('cl.cores', nCoresAUsar))
    parSapplyLB(cl = cl, X = pathsLocales[iHorasADescargar], FUN = function(x) try(R.utils::gunzip(x)))
    stopCluster(cl)
    pathsLocales
    
    agregacionTemporalGrillada(
      fechas = horas[iHorasADescargar], pathsRegresor = pathsLocalesDescomprimidos[iHorasADescargar], 
      formatoNomArchivoSalida = paste(pathSalida, '%Y%m%d.tif', sep=''), nFechasAAgregar = 24,
      funcionAgregacion = base::sum, padding = FALSE, ctl=ctl, shpBase = shpBase)
    # unlink(pathsLocales)
  }
  return(pathsLocalesDiarios)
}
