FROM rocker/shiny-verse:4.2.2

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    build-essential \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libglpk-dev \
    cmake

## update system libraries
RUN apt-get update && \
    apt-get install --only-upgrade libstdc++6 && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY renv.lock /renv.lock
## app folder
COPY /mirTox /app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore(lockfile = "/renv.lock")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
