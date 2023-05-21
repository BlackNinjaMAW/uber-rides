FROM rocker/shiny:4.0.5

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libudunits2-dev \
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled \
	shiny \
	DT \
	leaflet \
	lubridate \
	data.table \
	KernSmooth \
	magrittr \
	leafsync \
	mapview \
	ggplot2 \
	ggdark \
	raster \
	rgdal \
	sf

COPY . /srv/shiny-server/

USER shiny

EXPOSE 3838

# Enable Logging from stdout
ENV SHINY_LOG_STDERR=1

CMD ["/usr/bin/shiny-server"]