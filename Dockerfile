FROM rocker/verse:4.1.0

ENV DEBIAN_FRONTEND=noninteractive \
    JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

## Install necessary system dependencies for R packages (e.g. devtools, RPostgres)
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

## Update where R expects to find various Java files:
RUN R CMD javareconf

RUN echo JAVA_HOME="${JAVA_HOME}" >> /etc/environment

ENV RSESSION_USER=shiny

RUN echo "options('repos' = 'https://cloud.r-project.org/')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e "install.packages(c('devtools', 'DIZutils'))"
RUN R -e "install.packages('remotes')"

## Dependencies for dqastats (to speedup re-build process, keep them cached here):
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

## Dependencies for latex (to speedup re-build process, keep them cached here):
ARG texpackages="amsmath \
    latex-amsmath-dev \
    iftex \
    geometry \
    hyperref \
    pdftexcmds \
    infwarerr \
    kvoptions \
    etoolbox \
    titling \
    caption \
    babel-german \
    float \
    pdflscape \
    epstopdf-pkg"

RUN for package in $texpackages; do \
    R -q -e "p <- \"$package\"; tinytex::tlmgr_install(pkgs = p)"; \
    done

RUN R -q -e "devtools::install_git(url = 'https://gitlab.miracum.org/miracum/dqa/miRacumdqa.git', ref = 'master')"

## Copy code of this package:
COPY ./data-raw /home/${RSESSION_USER}/dqastats/data-raw
COPY ./inst /home/${RSESSION_USER}/dqastats/inst
COPY ./man /home/${RSESSION_USER}/dqastats/man
COPY ./R /home/${RSESSION_USER}/dqastats/R
COPY ./tests /home/${RSESSION_USER}/dqastats/tests
COPY ./DESCRIPTION /home/${RSESSION_USER}/dqastats/DESCRIPTION
COPY ./NAMESPACE /home/${RSESSION_USER}/dqastats/NAMESPACE

## Install our R-Package(s):
RUN cd /home/${RSESSION_USER}/dqastats/ && \
    R -q -e "devtools::install('.', upgrade = 'always', quick = TRUE, quiet = TRUE)"
