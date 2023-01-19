# Install R version 3.6
FROM r-base:3.6.0

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages('shinydashboard', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('shinyjs', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('plyr', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('dplyr', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('lubridate', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('readr', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('shinyalert', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('methods', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('DT', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('forcats', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('ggplot2', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('stringr', repos = 'http://cran.us.r-project.org')"

# Copy configuration files into the Docker image
RUN mkdir /srv/shiny-server/sportapp
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY app.R /srv/shiny-server/sportapp/
COPY Constants.R /srv/shiny-server/sportapp/
COPY AlgorithmTrainings.R /srv/shiny-server/sportapp/
COPY dfTrainings.rds /srv/shiny-server/sportapp/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
