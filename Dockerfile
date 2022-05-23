# Base R image
FROM rstudio/r-base:4.2.0-focal

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
	cmake \
	libgsl-dev \
	libgdal-dev \
	libxt-dev \
    libcairo2-dev \
	libharfbuzz-dev \
	libfribidi-dev \
	libudunits2-dev \
	jags

# Install R packages
COPY renv.lock ./
RUN R -e 'install.packages("renv", repos="https://cran.rstudio.com/")'
RUN R -e 'renv::restore()'

COPY st_interp ./st_interp
COPY *.r ./

ENTRYPOINT ["Rscript", "main.r"]
