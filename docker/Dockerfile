FROM rocker/verse:4.2.2

RUN apt-get update -qq
RUN apt-get -y install git

RUN install2.r –error \
--deps TRUE \
rmarkdown \
bookdown \
kableExtra \
knitr \
e1071 \
signal \
osfr \
tictoc \
BayesFactor \
reshape2 \
powerplus \
R.matlab
