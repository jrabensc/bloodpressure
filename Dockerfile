FROM rocker/tidyverse:4.0.3
RUN apt-get update
RUN apt-get install -y locales locales-all
WORKDIR /main/02-src
CMD ["Rscript", "create_plots.R"]

