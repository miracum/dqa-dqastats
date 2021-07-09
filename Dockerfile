FROM rocker/shiny-verse:4.1.0

ENV DEBIAN_FRONTEND=noninteractive \
    JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

# install necessary system dependencies for r packages (e.g. devtools, RPostgres)
RUN apt-get update -qq && apt-get install -y -q --no-install-recommends \
    apt-utils \
    git \
    libbz2-dev \
    liblzma-dev \
    openjdk-8-jdk \
    pandoc \
    pandoc-citeproc \
    r-cran-rjava \
    && rm -rf /tmp/downloaded_packages

# Update where R expects to find various Java files:
RUN R CMD javareconf

RUN echo JAVA_HOME="${JAVA_HOME}" >> /etc/environment

ENV RSESSION_USER=shiny

RUN echo "options('repos' = 'https://cloud.r-project.org/')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e "install.packages(c('devtools', 'DIZutils'))"
RUN R -e "install.packages('remotes')"

ARG packages="Rcpp \
    cli \
    cpp11 \
    systemfonts \
    parallelly \
    listenv \
    globals \
    ggplot2 \
    svglite \
    webshot \
    future \
    proxy \
    RPostgres \
    kableExtra \
    future.apply \
    e1071"

## Install and cleanup:
RUN for package in $packages; do \
    R -q -e "p <- \"$package\"; remotes::update_packages(packages = p, build_manual = FALSE, quiet = TRUE, upgrade = \"always\")"; \
    done && \
    rm -rf /tmp/*

## Copy code if this package:
COPY . /home/${RSESSION_USER}/dqastats/

## Install our R-Package(s):
RUN cd /home/${RSESSION_USER}/dqastats/ && \
    R -q -e "devtools::install('.', upgrade = 'always', quick = TRUE, quiet = TRUE)"

## Update all existing packages:
RUN R -e "devtools::update_packages(packages = TRUE, quiet = TRUE)"
