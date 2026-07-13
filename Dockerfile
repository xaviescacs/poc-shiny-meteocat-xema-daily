FROM rocker/shiny:4.4.1

WORKDIR /app

# Copy project files
COPY . .

# Update libgdal-dev to a higher version to be able to install terra package
RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable && apt-get update

# Install system dependencies required for R packages
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libpoppler-cpp-dev \
    cmake  \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN Rscript -e "install.packages(c(\
    'Rcpp',\
    'box',\
    'shiny',\
    'leaflet',\
    'sf',\
    'readr',\
    'stringr',\
    'dplyr',\
    'DT',\
    'ggplot2',\
    'glue',\
    'here'\
    ), repos='https://cran.rstudio.com/')"

# Set environment variables
ENV APP_ENV="container"

EXPOSE 80

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=80)"]
