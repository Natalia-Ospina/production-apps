FROM rocker/shiny:4.5.3

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY requirements.R .

RUN R -e "source('requirements.R'); install.packages(packages, repos='https://cloud.r-project.org')"

COPY . .

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
