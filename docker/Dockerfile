FROM rocker/verse:4.5.2

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

RUN R -q -e "tex_path <- tinytex::tinytex_root(); \
    tinytex::uninstall_tinytex(force = TRUE, dir = tex_path); \
    tinytex::install_tinytex(force = TRUE, dir = tex_path)"

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

RUN for package in $texpackages; do \
    R -q -e "p <- \"$package\"; tinytex::tlmgr_install(pkgs = p)"; \
    done
RUN R -q -e "tinytex::tlmgr_update(self = TRUE)"

## Install miracumdqa:
# RUN R -q -e "devtools::install_git(url = 'https://gitlab.miracum.org/miracum/dqa/miRacumdqa.git', ref = 'development')"

ENV RSESSION_USER=ruser
ENV USER_UID=1111

RUN mkdir -p /data/output/logs && \
    chown -R ${USER_UID}:${USER_UID} /data

RUN mkdir -p /var/run/s6 && \
    chown -R ${USER_UID}:${USER_UID} /var/run/s6

## Copy the code of this package:
COPY ./data-raw /dqastats/data-raw
COPY ./inst /dqastats/inst
COPY ./man /dqastats/man
COPY ./R /dqastats/R
COPY ./tests /dqastats/tests
COPY ./DESCRIPTION /dqastats/DESCRIPTION
COPY ./NAMESPACE /dqastats/NAMESPACE

## Install our R-Package(s):
RUN R -q -e "devtools::install('/dqastats', upgrade = 'always', quick = TRUE, quiet = TRUE)"

# RUN R -q -e "install.packages('DQAstats')"

## Switch to non-root user:
# USER docker
USER ${USER_UID}:${USER_UID}
