FROM rocker/r-ubuntu:20.04

LABEL maintainer="Peter Solymos <peter@analythium.io>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R
RUN install.r shiny forecast jsonlite ggplot2 htmltools plotly

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .
RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app', port = 3838, host = '0.0.0.0')"]