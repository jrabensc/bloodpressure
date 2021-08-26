FROM rocker/tidyverse:latest
RUN apt-get update
RUN apt-get install -y locales locales-all
WORKDIR /main/02-src
ENV RENV_VERSION 0.14.0
RUN echo "options(renv.consent = TRUE)" >> .Rprofile
COPY renv.lock .
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "renv::restore(confirm = FALSE)"
CMD ["Rscript", "create_plots.R"]
