script.dir.descargaDatos <- dirname((function() { attr(body(sys.function()), "srcfile") })()$filename)

source(paste(script.dir.descargaDatos, '/st_interp/descargador/descargadorEx.r', sep=''))


descargaPluviosADME <- function(dt_ini=dt_fin, dt_fin=date(now()), pathSalida='Datos/') {
  url <- paste('***REMOVED***?dtIni=', dt_ini, '&dtFin=', dt_fin, 
               sep = '')
  localFile <- paste(pathSalida, 
                     gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall.xlsx', sep = '')
  if (!file.exists(localFile) || file.info(localFile)$size == 0) {
    descargarArchivos(urls = url, nombresArchivosDestino = localFile, forzarReDescarga = T)
  }
  return(localFile)
}
