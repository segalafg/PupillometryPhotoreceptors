FROM rocker/verse:4.3.3

RUN apt-get update -qq
RUN apt-get -y install cmake
RUN apt-get -y --no-install-recommends install libglu1-mesa-dev
RUN apt-get -y install libxml2-dev
RUN apt-get -y install ghostscript
RUN apt-get -y install libxt-dev
RUN apt-get -y install git

RUN install2.r –error \
--deps TRUE \
knitr \
remotes \
tictoc \
R.matlab \
bookdown \
grImport \
png \
tiff \
pals \
ez \
gtoosl \
signal \
boot \
rstan \
bayestestR \
rstatix \
coda \
parallel \
utils \
osfr \
grid \
gridExtra \
cowplot \
ggsignif

