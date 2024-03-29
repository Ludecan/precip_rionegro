# precip_rionegro
Sistema de monitoreo de precipitación de la cuenca del Río Negro incorporando información satelital y de pluviómetros

# Instalación con Docker

1. Instalar Docker y Docker Compose:
- Docker: [Install Docker Engine](https://docs.docker.com/engine/install/)
- Docker Compose: [Install Docker Compose](https://docs.docker.com/compose/install/)

2. Clonar repositorio `precip_rionegro`
```
git clone git@github.com:Ludecan/precip_rionegro.git
```

3. Clonar la librería `st_interp`
```
cd precip_rionegro
git clone git@github.com:Ludecan/st_interp.git
cd ..
```

4. Agregar credenciales para descarga de datos satelitales en .netrc
```
nano $HOME/.netrc
```

Agregar entradas para `jsimpsonftps.pps.eosdis.nasa.gov` y `jsimpson.pps.eosdis.nasa.gov` (IMERG) y `hokusai.eorc.jaxa.jp` (GSMaP)
```
machine jsimpsonftps.pps.eosdis.nasa.gov
login
password

machine jsimpson.pps.eosdis.nasa.gov
login
password

machine hokusai.eorc.jaxa.jp
login
password
```

5. Crear archivo con variables de entorno para URLs de descarga de datos de pluviómetros
```
URL_MEDIDAS_PLUVIOS_CONVENCIONALES=
URL_MEDIDAS_PLUVIOS_RESPALDO=
URL_MEDIDAS_PLUVIOS_TELEMEDIDA=
```
Salvar en un archivo .env en la raíz del proyecto

# Ejecución

1. Si hubo cambios de código, (re)construir la imagen de Docker
```
docker build -t precip_rionegro .
```
La reconstrucción se puede hacer con la siguiente regla de makefile:
```
make docker-build
```

2. Ejecución mediante docker run
2.1 Fecha actual
```
docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/datos -v $(PWD)/Resultados:/Resultados -v $(HOME)/.netrc:/.netrc:ro -v $(PWD)/RCache_unix:/RCache_unix --env-file .env --env HOME=/ precip_rionegro
```
2.2 Fecha específica
```
docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/datos -v $(PWD)/Resultados:/Resultados -v $(HOME)/.netrc:/.netrc:ro -v $(PWD)/RCache_unix:/RCache_unix --env-file .env --env HOME=/ precip_rionegro dt_fin=2022-04-08
```
2.3 Rango de Fechas
```
docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/datos -v $(PWD)/Resultados:/Resultados -v $(HOME)/.netrc:/.netrc:ro -v $(PWD)/RCache_unix:/RCache_unix --env-file .env --env HOME=/ precip_rionegro dt_fin=2022-04-08;dt_ini=2022-03-31
```

El comando docker-run se puede ejecutar como una regla de make:
```
make docker-run
make docker-run "run_args=dt_fin=2022-05-17"
make docker-run "run_args=dt_fin=2022-04-08\;dt_ini=2022-04-01"
```

# Resultados
El programa almacena los resultados de su ejecución en 2 ubicaciones:

## 1. Datos de pluviometros
Los datos descargados de las redes pluviométricas se almacenan en `datos\pluviometros` con los siguientes nombres de archivo:
```
YYYYMMDD_YYYYMMDD_rainfall_convencionales.xlsx
YYYYMMDD_YYYYMMDD_rainfall_respaldo.json
YYYYMMDD_YYYYMMDD_rainfall_telemedida.xlsx
```
Donde cada archivo corresponde a las redes convencionales, de respaldo y de telemedida respectivamente y YYYYMMDD son las fechas de comienzo y fin de los datos.
El período de acumulación de los datos almacenados en los archivos es desde el día YYYYMMDD-1 a las 10 UTC, hasta el día YYYYMMDD a las 10 UTC: `(YYYYMMDD-1 a las 10UTC, YYYYMMDD a las 10 UTC]`.

## 2. Datos Satelitales
Los datos descargados de los productos satelitales se almacenan en `datos\satelites` con el siguiente formato de nombres:
```
\<<producto>>\<<version>>\YYYYMMDD.tif
```
Donde <<producto>> puede ser IMERG o GSMaP y <<version>> es la versión de producto almacenada. Actualmente se utiliza IMERG en su versión V06B y GSMaP en su versión v7.
Al igual que para los pluviometros, el período de acumulación de los datos almacenados en los archivos es desde el día YYYYMMDD-1 a las 10 UTC, hasta el día YYYYMMDD a las 10 UTC: `(YYYYMMDD-1 a las 10UTC, YYYYMMDD a las 10 UTC]`.

## 3. Resultados de interpolación y merging
La interpolación y merging producen 3 salidas en la carpeta `Resultados/Operativo` asociados a cada fecha de ejecución:
- Una carpeta `YYYYMMDD` con archivos `png` correspondientes a la salida de cada paso intermedio de la interpolación. Los archivos en esta carpeta son útiles para depuración y para análisis de las corridas.
- Un archivo `YYYYMMDD.tif` con los valores interpolados en cada píxel así como su información geográfica en formato geoTiff. Estos archivos son autocontenidos y puden ser cargados en cualquier software GIS para análisis posteriores o visualización.
- Un archivo `acumuladosSubcuencas/YYYY_MM_DD.tsv` con una fila para cada fecha en la que haya ejecutado el programa y 25 columnas, una para cada subcuenca contenida en la cuenca del Río Negro, con el valor promedio de todos los píxeles que componen la subcuenca. Las subcuencas se definen en el archivo `datos/CartografiaBase/SubcuencasModelo/mini_para_modelo_RioNegro.shp` y las columnas siguen el mismo orden que los polígonos en el shapefile.