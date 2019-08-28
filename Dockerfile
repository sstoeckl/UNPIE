FROM rocker/r-base
MAINTAINER name Snorri Pall Sigurdsson <sps@schantz.com>

RUN apt-get update -qq && apt-get install -y \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpq-dev \
	libsodium-dev \
	libxml2-dev
	
RUN R -e 'install.packages("devtools")'
RUN R -e 'devtools::install_github("rstudio/plumber")'
RUN R -e 'devtools::install_github("eaoestergaard/UNPIE")'

ADD /api/* /api/

EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000,swagger=TRUE)"]
CMD ["/api/api.R"]
