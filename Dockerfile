FROM bioconductor/bioconductor_docker:RELEASE_3_19

LABEL maintainer="Joseph Longworth"

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /app

COPY packages.R /app/packages.R
RUN Rscript /app/packages.R

COPY app.R /app/app.R
COPY www/ /app/www/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', port=3838, host='0.0.0.0')"]
