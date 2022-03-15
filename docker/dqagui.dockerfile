FROM rocker/shiny-verse:4.1.3

ENV DEBIAN_FRONTEND=noninteractive \
    JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

ENV RSESSION_USER=shiny
ENV USER_UID=1111

## Install necessary system dependencies for R packages (e.g. devtools, RPostgres)
RUN apt-get update -qq && apt-get install -y -q --no-install-recommends \
    apt-utils \
    git \
    libbz2-dev \
    liblzma-dev \
    libmagick++-dev \
    openjdk-8-jdk \
    pandoc \
    pandoc-citeproc \
    r-cran-rjava \
    && rm -rf /tmp/downloaded_packages

## Update where R expects to find various Java files:
RUN R CMD javareconf

RUN echo JAVA_HOME="${JAVA_HOME}" >> /etc/environment

RUN echo "options('repos' = 'https://cloud.r-project.org/')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e "install.packages(c('devtools', 'DIZutils', 'remotes'))"

## Dependencies for dqastats (to speedup re-build process, keep them cached here):
RUN install2.r --error --deps TRUE --skipinstalled \
    cli \
    cpp11 \
    e1071 \
    fansi \
    future \
    future.apply \
    ggplot2 \
    globals \
    glue \
    jsonlite \
    kableExtra \
    listenv \
    parallelly \
    proxy \
    RPostgres \
    Rcpp \
    svglite \
    systemfonts \
    webshot \
    yaml

USER ${RSESSION_USER}
RUN R -q -e "tinytex::install_tinytex(force = TRUE)"
USER root

ENV PATH="/home/${RSESSION_USER}/bin:/home/${RSESSION_USER}/.TinyTeX/bin/x86_64-linux:${PATH}"

## Dependencies for LaTeX (to speed up the re-build process, keep them cached here):
ARG texpackages="amsfonts \
    amsmath \
    atveryend \
    babel \
    babel-german \
    caption \
    colortbl \
    dehyph-exptl \
    ec \
    environ \
    epstopdf-pkg \
    etoolbox \
    float \
    geometry \
    graphics \
    graphics-def \
    hycolor \
    hyperref \
    hyphen-german \
    iftex \
    koma-script \
    latex-graphics-dev \
    latexconfig \
    lm \
    makecell \
    multirow \
    oberdiek \
    pdfcro \
    pdflscape \
    tabu \
    tex-gyre-math \
    texlive-scripts \
    threeparttable \
    threeparttablex \
    titling \
    tools \
    trimspaces \
    ulem \
    varwidth \
    wrapfig \
    ulem \
    url \
    xco \
    xcolor"

RUN R -q -e "tinytex::tlmgr_conf()"
RUN for package in $texpackages; do \
    R -q -e "p <- \"$package\"; tinytex::tlmgr_install(pkgs = p)"; \
    done
RUN R -q -e "tinytex::tlmgr_update(self = TRUE)"

## Install miracumdqa:
# RUN R -q -e "devtools::install_git(url = 'https://gitlab.miracum.org/miracum/dqa/miRacumdqa.git', ref = 'development')"



RUN mkdir -p /data/output/logs && \
    chown -R ${USER_UID}:${USER_UID} /data

RUN mkdir -p /var/run/s6 && \
    chown -R ${USER_UID}:${USER_UID} /var/run/s6

# ## Copy the code of this package:
# COPY ./data-raw /dqastats/data-raw
# COPY ./inst /dqastats/inst
# COPY ./man /dqastats/man
# COPY ./R /dqastats/R
# COPY ./tests /dqastats/tests
# COPY ./DESCRIPTION /dqastats/DESCRIPTION
# COPY ./NAMESPACE /dqastats/NAMESPACE

# ## Install our R-Package(s):
# RUN R -q -e "devtools::install('/dqastats', upgrade = 'always', quick = TRUE, quiet = TRUE)"

RUN R -q -e "install.packages('DQAgui')"

# Add shiny app
COPY ./docker/app.R /srv/shiny-server/
# Add custom server conf (running shiny as user 'shiny' is more secure than running as 'root')
COPY ./docker/shiny-server.conf /etc/shiny-server/
## Add log-script
COPY ./docker/show-log.sh /

# fix permissions
RUN chown -R ${RSESSION_USER}:${RSESSION_USER} /srv/shiny-server/
RUN chmod +x show-log.sh

# create log dir
RUN mkdir /home/${RSESSION_USER}/logs && \
    chown -R shiny:shiny /home/${RSESSION_USER}/logs

USER ${RSESSION_USER}

# https://stackoverflow.com/questions/51080857/preserve-environment-variables-when-spawning-shiny-processes-within-a-container?rq=1
ENTRYPOINT env >> /home/${RSESSION_USER}/.Renviron && \
    chown ${RSESSION_USER}.${RSESSION_USER} /home/${RSESSION_USER}/.Renviron && \
    /usr/bin/shiny-server
