script.dir.descargaDatos <- dirname((function() { attr(body(sys.function()), "srcfile") })()$filename)

source(paste(script.dir.descargaDatos, '/st_interp/instalarPaquetes/instant_pkgs.r', sep=''))
instant_pkgs(c('jsonlite', 'R.utils', 'lubridate'))

source(paste(script.dir.descargaDatos, '/st_interp/descargador/descargadorEx.r', sep=''))
source(paste(script.dir.descargaDatos, '/st_interp/GrADS/ReadGrADS.r', sep=''))

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

descargaGSMaP <- function(dt_ini=parse_date_time(dt_fin, orders = 'ymd') - 24*60*60, dt_fin=date(now()), 
                          horaUTCInicioAcumulacion=10, pathSalida='datos/satelites/GSMaP/') {
  urlBase <- 'ftp://hokusai.eorc.jaxa.jp/realtime_ver/v7/'
  producto <- 'hourly_G'
  
  authInfo <- fromJSON(txt = 'GSMaP_authInfo.json')
  
  dt_ini <- sprintf('%s %02d:00', date(dt_ini), horaInicioAcumulacion + 1)
  dt_fin <- sprintf('%s %02d:00', date(dt_fin), horaInicioAcumulacion)
  
  nomArchCTL <- paste('GSMaP_NRT.', producto, '.rain.ctl', sep = '')
  pathLocalCTL <- paste(pathSalida, nomArchCTL, sep = '')
  if (!file.exists(pathLocalCTL) | file.info(pathLocalCTL)$size <= 0) {
    res <- descargarArchivos(urls = paste(urlBase, 'sample/', nomArchCTL, sep = ''), 
                             nombresArchivosDestino = pathLocalCTL, authInfo = authInfo)  
  }
  
  urls <- strftime(x = seq(as.POSIXct(dt_ini), as.POSIXct(dt_fin), by="hour"), 
                   format = paste(urlBase, producto, '/%Y/%m/%d/gsmap_gauge.%Y%m%d.%H%M.dat.gz', sep=''))
  pathsLocales <- paste(pathSalida, basename(urls), sep='')
  # write(toJSON(authInfo), 'GSMaP_authInfo.json')
  
  res <- descargarArchivos(urls = urls, nombresArchivosDestino = pathsLocales, authInfo = authInfo)
  
  seq.int(from=1, to=length(pathsLocales), by = 24)
  
  if (any(res == 0)) {
    stop(paste('Error downloading GSMaP files:', paste(urls[res == 0], collapse = ', ')))
  }
  sapply(pathsLocales, function(x) try(R.utils::gunzip(x)))
  pathsLocales <- substr(pathsLocales, start = 1, stop = nchar(pathsLocales) - 3)
  ctl <- parseCTL_V2(ctlFile = pathLocalCTL)
  
source(paste(pathSTInterp, 'interpolar/mapearEx.r', sep=''))
  ctl$xdef$from <- ctl$xdef$from - 180
  ctl$xdef$vals <- ctl$xdef$vals - 180
  
  grilla <- readXYGridSP(ctl = ctl, dsetOverride = pathsLocales[1])
  grilla <- as(grilla, 'SpatialPixelsDataFrame')
  grilla <- spTransform(grilla, proj4string(shpBase))
  
  spTransform(grilla, proj4string(shpBase))
  
  
  
  aux <- spTransform(shpBase, proj4string(grilla))
  bbaux <- getPoligonoBoundingBox(aux)
  i <- !is.na(over(grilla, bbaux))
  
  coordinates(grilla)[, 'lon'] <- coordinates(grilla)[, 'lon'] -180
  
  i <- !is.na(over(grilla, shpBase)[,1])
  
  grilla$value  
  
  proj4string(grilla)
  mapearGrillaGGPlot(grilla[i,], shpBase = aux)
}
