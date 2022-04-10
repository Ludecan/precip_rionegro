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

Agregar entradas para `jsimpson.pps.eosdis.nasa.gov` (IMERG) y `hokusai.eorc.jaxa.jp` (GSMaP)
```
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
docker run -v $PWD/datos:/datos -v $PWD/Resultados:/Resultados -v $HOME/.netrc:/root/.netrc:ro --env-file .env precip_rionegro
```
2.2 Fecha específica
```
docker run -v $PWD/datos:/datos -v $PWD/Resultados:/Resultados -v $HOME/.netrc:/root/.netrc:ro --env-file .env precip_rionegro dt_fin=2022-04-08
```
2.3 Rango de Fechas
```
docker run -v $PWD/datos:/datos -v $PWD/Resultados:/Resultados -v $HOME/.netrc:/root/.netrc:ro --env-file .env precip_rionegro dt_fin=2022-04-08;dt_ini=2022-04-01
```

El comando docker-run se puede ejecutar como una regla de make:
```
make docker-run
make docker-run "dt_fin=2022-04-08"
make docker-run "dt_fin=2022-04-08\;dt_ini=2022-04-01"
```
