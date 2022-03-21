iFrame <- sys.nframe()
if (iFrame >= 3) { script.dir.descargaDatosPluviometros <- sys.frame(iFrame - 3)$ofile
} else { script.dir.descargaDatosPluviometros <- NULL }
while ((is.null(script.dir.descargaDatosPluviometros) || is.na(regexpr('descargaDatosPluviometros.r', script.dir.descargaDatosPluviometros, fixed=T)[1])) && iFrame >= 0) {
  script.dir.descargaDatosPluviometros <- sys.frame(iFrame)$ofile
  iFrame <- iFrame - 1
}
if (is.null(script.dir.descargaDatosPluviometros)) { script.dir.descargaDatosPluviometros <- ''
} else { script.dir.descargaDatosPluviometros <- paste0(dirname(script.dir.descargaDatosPluviometros), '/') }

#source(paste0(script.dir.descargaDatosPluviometros, '/st_interp/instalarPaquetes/instant_pkgs.r'), encoding = 'WINDOWS-1252')
#instant_pkgs(c('jsonlite', 'R.utils', 'lubridate', 'benchmarkme'))

source(paste0(script.dir.descargaDatosPluviometros, '/st_interp/descargador/descargadorEx.r'), encoding = 'WINDOWS-1252')
source(paste0(script.dir.descargaDatosPluviometros, '/st_interp/SeriesTemporales/leerSeriesTemporales.r'), encoding='WINDOWS-1252')

logDatosObtenidosPluviometros <- function(datos, postFijoMsj='') {
  if (nrow(datos$datos) == 1) {
    print(paste0(Sys.time(), " - Se obtuvieron ", sum(!is.na(datos$datos)), " datos de ", ncol(datos$datos), " pluviometros", postFijoMsj, "."))
  } else {
    print(paste0(Sys.time(), " - Se obtuvieron ", sum(!is.na(datos$datos)), " datos de ", ncol(datos$datos), " pluviometros en ", nrow(datos$datos), " fechas", postFijoMsj, "."))
  }
}

descargaPluviosADMEConvencionales <- function(
  dt_ini=dt_fin, 
  dt_fin=date(now()), 
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_CONVENCIONALES'),
  pathSalida='datos/pluviometros/',
  forzarReDescarga=FALSE
) {
  url <- paste0(url_medidas_pluvios, "?dtIni=", dt_ini, "&dtFin=", dt_fin)
  localFile <- paste0(
    pathSalida, gsub("-", "", dt_ini), "_", gsub("-", "", dt_fin), "_rainfall_convencionales.xlsx"
  )

  descargarArchivos(
    urls = url, nombresArchivosDestino = localFile, curlOpts = list(use_ssl = 3),
    forzarReDescarga = forzarReDescarga
  )

  datosConvencionales <- leerSeriesXLSX(
    pathArchivoDatos = localFile, colsEstaciones = 1:6, colId = 3, hojaDatos = "Medidas",
    fileEncoding = "UTF-8", na.strings = c("-1111", "-1,79769313486E+308")
  )
  # TODO: -1,79769313486E+308 no se está interpretando bien como una string NA
  datosConvencionales$datos[datosConvencionales$datos < 0] <- NA
  
  colRenames <- list(
    NombreEstacionR='cod_pluv',
    nombre='localidad',
    latitud='lat',
    longitud='long'
  )
  
  for (i in seq_along(colRenames)) {
    oldName <- colRenames[[i]]
    newName <- names(colRenames)[i]
    names(datosConvencionales$estaciones)[names(datosConvencionales$estaciones) == oldName] <- newName
  }
  
  datosConvencionales$estaciones <- datosConvencionales$estaciones[, names(colRenames)]
  datosConvencionales$estaciones[, 'tipoMet'] <- FALSE
  datosConvencionales$estaciones[, 'tipoPluvio'] <- TRUE
  datosConvencionales$estaciones[, 'tipoAutomatica'] <- FALSE
  datosConvencionales$estaciones[, 'redOrigen'] <- 'convencionales'
  
  return(datosConvencionales)
}

descargaPluviosADMETelemedida <- function(
  dt_ini=dt_fin, 
  dt_fin=date(now()), 
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_TELEMEDIDA'),
  pathSalida='datos/pluviometros/',
  forzarReDescarga=FALSE
) {
  dt_ini_telemedida <- as.Date(dt_ini) - lubridate::days(1)
  dt_fin_telemedida <- as.Date(dt_fin)
  
  url <- paste0(url_medidas_pluvios, '?dtIni=', dt_ini_telemedida, '&dtFin=', dt_fin_telemedida)
  localFile <- paste0(
    pathSalida, gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall_telemedida.xlsx'
  )
  
  descargarArchivos(
    urls=url, nombresArchivosDestino=localFile, curlOpts=list(use_ssl = 3), 
    forzarReDescarga=forzarReDescarga)
  datosTelemedida <- leerSeriesXLSX(
    pathArchivoDatos=localFile, hojaDatos='MedidasHorarias', fileEncoding='UTF-8'
  )
  
  # TODO: Actualizar esto para que cumpla con el criterio del día i + 1 si alguna vez 
  # se vuelve a utilizar
  # Agregacion diaria
  print(paste0(Sys.time(), ' - Agregando valores diarios...'))
  triHourlyUpTo <- list(
    PASO.MAZANGANO.RHT=ymd_hm("2019-11-06 06:00", tz = tz(datosTelemedida$fechas[1])),
    PASO.LAGUNA.I.RHT=ymd_hm("2019-12-03 09:00", tz = tz(datosTelemedida$fechas[1])),
    PASO.LAGUNA.II.RHT=ymd_hm("2019-12-03 12:00", tz = tz(datosTelemedida$fechas[1])),
    PASO.PEREIRA.RHT=ymd_hm("2019-12-04 15:00", tz = tz(datosTelemedida$fechas[1])),
    BARRA.DE.PORONGOS.RHT=ymd_hm("2019-12-10 09:00", tz = tz(datosTelemedida$fechas[1])),
    VILLA.SORIANO.RHT=ymd_hm("2019-12-11 12:00", tz = tz(datosTelemedida$fechas[1]))
  )
  triHourlyUpTo <- triHourlyUpTo[names(triHourlyUpTo) %in% datosTelemedida$estaciones$Nombre]
  
  colsToSplit <- which(sapply(colnames(datosTelemedida$datos), FUN = function(x) x %in% names(triHourlyUpTo)))
  # x <- triHourlyUpTo[[5]]
  rowsToSplit <- sapply(triHourlyUpTo, function(x, fechasObservaciones) {
    hora <- as.integer(substr(fechasObservaciones, 12, 13))
    if (!is.na(x)) { 
      return(seq_along(fechasObservaciones)[hora %% 3 == 0 & fechasObservaciones <= x])
    } else { 
      return(seq_along(fechasObservaciones)[hora %% 3])
    }
  }, fechasObservaciones=datosTelemedida$fechas)
  
  idx <- sapply(rowsToSplit, function(x) !is.null(x))
  colsToSplit <- colsToSplit[idx]
  rowsToSplit <- rowsToSplit[idx]
  rm(idx)
  
  getMode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  splitAccumulated <- function(valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
    splitAccumulated_i <- function(i, valoresObservaciones, colsToSplit, rowsToSplit, rowWeights=NULL) {
      # i <- colsToSplit[1]
      rowsToSplit_i <- rowsToSplit[[i]]
      rowsPerPeriod <- getMode(diff(rowsToSplit_i))
      idx_i <- colsToSplit[i]
      j <- 2
      for (j in seq_along(rowsToSplit_i)) {
        endRow <- rowsToSplit_i[j]
        if (j > 1) { startRow <- rowsToSplit_i[j - 1] + 1
        } else { startRow <- 1 }
        
        if (!is.null(rowWeights)) { rowWeightsJ <- rowWeights[j]
        } else {
          n <- endRow - startRow + 1
          rowWeightsJ <- rep(1 / rowsPerPeriod, n)
        }
        
        valoresObservaciones[startRow:endRow, idx_i] <- valoresObservaciones[endRow, idx_i] * rowWeightsJ
      }
      
      return(valoresObservaciones[, idx_i])
    }
    
    valoresObservaciones[, colsToSplit] <- sapply(
      seq_along(colsToSplit), splitAccumulated_i, valoresObservaciones=valoresObservaciones, 
      colsToSplit=colsToSplit, rowsToSplit=rowsToSplit, rowWeights=rowWeights)
    return(valoresObservaciones)
  }
  
  datosTelemedida$datos <- splitAccumulated(
    datosTelemedida$datos, colsToSplit, rowsToSplit, rowWeights = NULL)
  
  rm(rowsToSplit, colsToSplit)
  
  iStartHours <- grep(
    pattern=sprintf('%02d:00', horaLocalInicioAcumulacion + 1), 
    x=rownames(datosTelemedida$datos), fixed=T
  )
  iStartHour <- iStartHours[1]
  if (iStartHours[length(iStartHours)] + 23 <= nrow(datosTelemedida$datos)) {
    iEndHour <- iStartHours[length(iStartHours)] + 23
  } else {
    iEndHour <- iStartHours[length(iStartHours) - 1] + 23
  }
  
  idx <- iStartHour:iEndHour
  rm(iStartHours, iStartHour, iEndHour)
  
  datosTelemedida$fechas <- datosTelemedida$fechas[idx]
  datosTelemedida$datos <- datosTelemedida$datos[idx, ]
  clases <- (seq_along(datosTelemedida$fechas) - 1) %/% 24L
  clases <- datosTelemedida$fechas[(clases * 24L) + 1]
  clases <- parse_date_time(
    substr(as.character(clases), 1, 10), orders = 'Ymd', tz=tz(datosTelemedida$fechas), truncated = 0
  )
  # max_gap_lengths <- aggregate(valoresObservaciones, by=list(day=clases), FUN=max_run_length)
  nNoNa <- aggregate(datosTelemedida$datos, by=list(day=clases), FUN=function(x) sum(!is.na(x)))[, -1]
  datosTelemedida$datos <- as.matrix(aggregate(datosTelemedida$datos, by=list(day=clases), FUN=sum, na.rm=T)[, -1])
  datosTelemedida$fechas <- unique(clases) + lubridate::days(1)
  row.names(datosTelemedida$datos) <- as.character(datosTelemedida$fechas)
  
  maxNHorasParaRechazarDia <- 21
  iAceptados <- nNoNa > maxNHorasParaRechazarDia
  datosTelemedida$datos[!iAceptados] <- NA
  datosTelemedida$datos[iAceptados] <- datosTelemedida$datos[iAceptados] * 24 / nNoNa[iAceptados]  
  
  datosTelemedida$estaciones[, 'nombre'] <- datosTelemedida$estaciones[, 'Nombre']
  colRenames <- list(
    NombreEstacionR='Nombre',
    nombre='nombre',
    latitud='Latitud',
    longitud='Longitud'
  )
  
  for (i in seq_along(colRenames)) {
    oldName <- colRenames[[i]]
    newName <- names(colRenames)[i]
    names(datosTelemedida$estaciones)[names(datosTelemedida$estaciones) == oldName] <- newName
  }
  
  datosTelemedida$estaciones <- datosTelemedida$estaciones[, names(colRenames)]
  datosTelemedida$estaciones[, 'tipoMet'] <- FALSE
  datosTelemedida$estaciones[, 'tipoPluvio'] <- TRUE
  datosTelemedida$estaciones[, 'tipoAutomatica'] <- TRUE
  datosTelemedida$estaciones[, 'redOrigen'] <- 'telemedida'
  
  return(datosTelemedida)
}

descargaPluviosRespaldo <- function(
  dt_ini=dt_fin, 
  dt_fin=date(now()), 
  url_medidas_pluvios=Sys.getenv(x='URL_MEDIDAS_PLUVIOS_RESPALDO'),
  pathSalida='datos/pluviometros/',
  forzarReDescarga=FALSE
) {
  dt_ini_respaldo <- as.Date(dt_ini)
  attr(dt_ini_respaldo, "tzone") <- "UTC"
  dt_fin_respaldo <- as.Date(dt_fin)
  attr(dt_fin_respaldo, "tzone") <- "UTC"
  
  localFile <- paste0(
    pathSalida, gsub('-', '', dt_ini), '_', gsub('-', '', dt_fin), '_rainfall_respaldo.mch'
  )
  
  descargarArchivos(
    urls=url_medidas_pluvios, nombresArchivosDestino=localFile, curlOpts=list(use_ssl = 3), 
    forzarReDescarga=forzarReDescarga
  )
  
  datosRespaldo <- leerDatosVarsEstacionesFechasDeJSON(
    pathArchivoDatos=localFile, colIdEstacion='nombre'
  )
  datosRespaldo <- extraerVariableEstacionesDeFechas(
    datosVarsEstacionesFechas=datosRespaldo, idStrVariable='R3'
  )
  
  datosRespaldo$datos[datosRespaldo$datos == 'TRAZA'] <- "5e-2"
  class(datosRespaldo$datos) <- "numeric"

  fechas <- as.POSIXct(seq(dt_ini_respaldo, dt_fin_respaldo, by=1))
  attr(fechas, "tzone") <- "UTC"
  datos <- matrix(data=NA_real_, nrow = length(fechas), ncol = ncol(datosRespaldo$datos))
  rownames(datos) <- as.character(fechas)
  colnames(datos) <- colnames(datosRespaldo$datos)
  
  iMatches <- base::match(as.character(fechas), as.character(datosRespaldo$fechas))
  idx <- !is.na(iMatches)
  datos[idx, ] <- datosRespaldo$datos[iMatches[idx], ]
  
  if (any(!idx)) {
    datosRespaldoHistorico <- leerSeriesArchivoUnico(
      paste0(pathSalida, 'R3_201701_202201.tsv')
    )
    # Criterío dia i + 1
    datosRespaldoHistorico$fechas <- datosRespaldoHistorico$fechas + lubridate::days(1)
    rownames(datosRespaldoHistorico$datos) <- as.character(datosRespaldoHistorico$fechas)
    
    iMatchesFechas <- base::match(as.character(fechas), as.character(datosRespaldoHistorico$fechas))
    idxFechas <- !is.na(iMatchesFechas)
    
    iMatchesEstaciones <- base::match(
      datosRespaldo$estaciones$NombreEstacionR,
      datosRespaldoHistorico$estaciones$Estacion
    )
    idxEstaciones <- !is.na(iMatchesEstaciones)
    
    datos[idxFechas, idxEstaciones] <- datosRespaldoHistorico$datos[
      iMatchesFechas[idxFechas], iMatchesEstaciones[idxEstaciones]]
  }

  datosRespaldo$fechas <- fechas
  datosRespaldo$datos <- datos
  
  datosRespaldo$estaciones <- datosRespaldo$estaciones[
    , c('NombreEstacionR', 'nombre', 'latitud', 'longitud', 'tipoMet', 'tipoPluvio', 'tipoAutomatica')
  ]
  datosRespaldo$estaciones[, 'redOrigen'] <- 'respaldo'
  
  return(datosRespaldo)
}
