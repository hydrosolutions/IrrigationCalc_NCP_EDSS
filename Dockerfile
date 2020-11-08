# The Dockerfile instructs Docker to build image step by step. 

FROM cocomcie/rshiny-base:v1.0


# copy the app to the image
RUN mkdir /root/shinyapp
COPY app /root/shinyapp

# set up dependency inside the Ubuntu
RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN sudo apt update && sudo apt -y upgrade
RUN sudo apt install -y openjdk-8-jdk
RUN sudo R CMD javareconf

RUN Rscript /root/shinyapp/dpp.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"]
