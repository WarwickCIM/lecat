FROM openanalytics/r-base

MAINTAINER James Tripp "james.tripp@warwick.ac.uk"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libgit2-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the euler app
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('warwickcim/lecat')"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/usr/lib/R/library/lecat/shiny-apps/lecat-app', port = 3838, host = 0.0.0.0)"]
