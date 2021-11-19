FROM rocker/verse:4.1.2

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
    xco"

RUN for package in $texpackages; do \
    R -q -e "p <- \"$package\"; tinytex::tlmgr_install(pkgs = p)"; \
    done
RUN R -q -e "tinytex::tlmgr_update()"

## Install miracumdqa:
RUN R -q -e "devtools::install_git(url = 'https://gitlab.miracum.org/miracum/dqa/miRacumdqa.git', ref = 'development')"

ENV RSESSION_USER=ruser
ENV USER_UID=1111

RUN mkdir -p /data/output/logs && \
    chown -R ${USER_UID}:${USER_UID} /data

RUN mkdir -p /var/run/s6 && \
    chown -R ${USER_UID}:${USER_UID} /var/run/s6

## Copy the code of this package:
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

## Switch to non-root user:
# USER docker
USER ${USER_UID}:${USER_UID}
