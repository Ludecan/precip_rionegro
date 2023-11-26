# Base R image
FROM rstudio/r-base:4.2.3-focal

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
	
WORKDIR /precip_rionegro
COPY renv.lock renv.lock

RUN R -e 'install.packages("renv", repos="https://cran.rstudio.com/")'

# Install R packages
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN R -e 'renv::restore()'

RUN mkdir -p .cache/R/renv

COPY st_interp ./st_interp
COPY *.r ./

ENTRYPOINT ["Rscript", "main.r"]
