library(shiny)
library(ggplot2)
library(plyr)
library(leaflet)
library(raster)
library(rgdal)
#library(mailR)
library(dygraphs)
library(xts)
library(zoo)
library(reshape2)
library(DT)
library(shinyjs)
library(xlsx)
library(shinythemes)
library(shinyBS)
library(rhandsontable)
library(scales)
library(tools)
library(openssl)
library(xts)
#library(randomcoloR)
library(RSQLite)
library(reshape)
library(showtext)

source("bin/ETmodel.R", chdir=T)
source("bin/waterbalancemodel.R", chdir=T)

showtext_auto()

## Read db.sql for liveNDVI data
data_db <- dbConnect(drv = RSQLite::SQLite(), dbname="data-ndvi/heilonggang-ndvi.sql")
data_root_path <- "data-ndvi/ndvi-tifs/"

## Load translation files
dictionary=as.data.frame(read.csv("data/dictionary.csv",header=TRUE,
                                  colClasses = "character",sep='\\',
                                  check.names=FALSE))
dictionaryUI=as.data.frame(read.csv("data/dictionary_UI.csv",header=TRUE,
                                    colClasses = "character",sep='\\',
                                    check.names=FALSE))
rownames(dictionary) <- dictionary[,1]
dictionary[,1] <- NULL
rownames(dictionaryUI) <- dictionaryUI[,1]
dictionaryUI[,1] <- NULL

crop <- read.table("data/croptypes.csv",header=TRUE,row.names = 1,sep=",")
soil <- read.table("data/soiltypes.csv",header=TRUE,row.names = 1,sep=",")
irrigationefficiency<-read.table("data/irrigationefficiency.csv",header=TRUE,
                                 sep=",")
conveyanceefficiency<-read.table("data/conveyanceefficiency.csv",header=TRUE,
                                 sep=",")
watersourcenorm<-read.table("data/watersource.csv",header=TRUE,sep=",")
norm <- read.table("data/irrigationnorm.csv", header=TRUE, sep=",")

## Select region shaepfile
init_shp<-readOGR("data/shapefiles/Ir_N_RegID_7.shp", encoding = "GB2312")
bbox = extent(init_shp)
NDVI_shp <- readOGR("data/shapefiles/NCP_political.shp")

## Define & Load local meteodata
datafiles<-list("data/meteodata/GUANTAO_ST.csv",		
                "data/meteodata/CLIMWAT_FAO.csv",
                "data/meteodata/WMO_ST_548080_1997-2017.csv",		
                "data/meteodata/BAXIAN_54518099999_1999-01-01_1997-07-20_.csv",		
                "data/meteodata/CANGZHOU_54616099999_1999-01-01_1995-12-31_.csv",		
                "data/meteodata/HUIMIN_54725099999_1999-01-01_2019-02-04_.csv",		
                "data/meteodata/POTOU_54618099999_1999-01-01_2019-02-04_.csv",		
                "data/meteodata/SHIJIAZHUANG_53698099999_1999-01-01_2019-02-04_.csv",		
                "data/meteodata/user_dummy.csv")
names<-list("guantaostationlabel",
            "climwatlabel",		 
            "wmostationlabel",	
            "baxianstationlabel",	
            "cangzhoustationlabel",		
            "huiminstationlabel",		
            "potoustationlabel",		
            "shijiazhuangstationlabel",		
            "userstationlabel")		
lats<-list(36.53313,36.6,36.2333,39.05,38.333,37.5,38.0833333,38.0666666,36.6)		
lons<-list(115.3111,114.5,115.666667,116.4,116.833,117.5333333,116.55,114.35,
           115.25)		
alt <- list(57,57,69,8,11,12,13,104.8,57)
meteo_df <- do.call(rbind, Map(data.frame, NAME=names, DATAFILE=datafiles, 
                               LAT=lats, LON=lons, ALT=alt))
rm(list = c("names","lats","lons","alt"))

## Pre-compute ET0 for all stations
clim <- list()
loc <- list()
for (item in 1:(length(meteo_df$NAME))) {
  temp_clim <- read.csv(as.character(meteo_df[item,"DATAFILE"]),header = TRUE, 
                        stringsAsFactors = FALSE)
  loc[[item]] <- data.frame(LAT=meteo_df[item,"LAT"],LON=meteo_df[item,"LON"],
                            ALT=meteo_df[item,"ALT"])
  temp_clim$RAD <- monthly.radiation(temp_clim,loc[[item]])
  temp_clim$ET <- as.numeric(monthly.PenmanFAO(temp_clim,loc[[item]]))
  if (!("PRCP_25" %in% colnames(temp_clim))) {
    temp_clim$PRCP_25 <- c(1:12)*NA
  }
  clim[[item]] <- temp_clim
}
default_climate <- clim
default_location <- loc
rm(list = c("clim","loc","item","temp_clim"))

## Record App access in logfile. TODO: This feature does not work with SSL. 
## Need to figure out why
logfile = "log/access.log"
if (!file.exists(logfile)) {
  if (!dir.exists(dirname(logfile))) {
    dir.create(dirname(logfile))
  }
  write(paste('TIME','USER IP','LOCATION','COUNTRY','CITY',sep='\t'),
        file=logfile)
}

## Some Constants
outputtypes <- c("ETo","ETc","PRCP","PRCPeff","Water_Deficit","PRCP_25",
                 "PRCPeff_25","Water_Deficit_25","CROPDAYS")
outputcolors <- c("#E41A1C","#FF7F00","#253494","#1F78B4","#1B9E77","#80b1d3",
                  "#a6cee3","#a6d854","#4D4D4D")
irrigationstrategies <- c("monthly","minimum")
monthnames <- c<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                   "Nov","Dec")

# User Database
db <- dbConnect(drv = RSQLite::SQLite(), dbname="data/userdb.sql")
if (length(dbListTables(db))==0) {
  query <- dbSendStatement(conn = db,"CREATE TABLE user (ID INTEGER PRIMARY KEY, username TEXT,password TEXT, email TEXT, dataid INTEGER)")
  dbClearResult(query)
  query <- dbSendStatement(conn = db,"CREATE TABLE data (ID INTEGER PRIMARY KEY, username TEXT, data BLOB, summary BLOB, IDcounter BLOB, colors BLOB)")
  dbClearResult(query)
}

## Some Helper Functions
doy2date <- function(doy,year) {
  # argument doy can be a vector, year must be a single value
  origin = as.Date(paste(year,'-01-01',sep=''))-1
  func <- function(x) {
    date <- origin + x
    if (year(date)!=year) {
      return(NA)
    } else {
      return(date)
    }
  }
  as.Date(sapply(doy, func))
}

captchachoices <- 6
generatecaptcha <- function() {
  randvec <- runif(5, 0, 1)  
  vec <- randvec/mean(randvec)*round(runif(1,1,captchachoices))  #generate 5 random numbers with mean between 1 and the value of captchachoices, solution is the mean
  return(vec)
}
