library(lubridate)

monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

interpolate_climate <- function(vec_monthly,doy) {
  approxfun(vec_monthly)
}

monthly.radiation <- function(clim,loc) {
  # based on http://www.fao.org/docrep/X0490E/x0490e07.htm#radiation
  # TODO: Check if either SUNH or SRAD is available
  days <- vector(mode="numeric", length=12)
  radiation <- vector(mode="numeric", length=12)
  for (doy in 1:365){
    month=doy2month(doy)
    dr <- 1 + 0.033 * cos(2*pi/365*doy) ##0.03
    j <- loc$LAT/360*2*pi
    d <- 0.409*sin(2*pi/365*doy - 1.39)
    ws <- acos(-tan(j)*tan(d))
    Ra <- 24*60/pi*0.0820*dr*(ws*sin(j)*sin(d)+cos(j)*cos(d)*sin(ws))  # MJ/m^2/day
    if (is.null(clim$SRAD)) {
      N <- 24/pi*ws
      as <- 0.25
      bs <- 0.5
      n <- clim$SUNH[month]
      Rs <-(as+bs*n/N)*Ra  # MJ/m^2/day 
    } else {
      Rs <- clim$SRAD[month]
    }
    a <- 0.23
    Rns <- (1-a)*Rs
    Rso <- (0.75+2*10**-5*loc$ALT)*Ra  # MJ/m^2/day
    tmax <- clim$TMAX[month]
    tmin <- clim$TMIN[month]
    esmax <- 0.6108*exp(17.27*tmax/(tmax+237.3))
    esmin <- 0.6108*exp(17.27*tmin/(tmin+237.3))
    es <- (esmax+esmin)/2
    if ("HUM" %in% colnames(clim)) {
      rh <- clim$HUM[month]
      ea <- rh/100*es  # kPa
    } else if ("TDEW" %in% colnames(clim)) {
      tdew <- clim$TDEW[month]
      ea <- 0.6108*exp(17.27*tdew/(tdew+237.3))
    }
    Rnl <- 4.903*10**-9 *((tmax+273.15)**4+(tmin+273.15)**4)/2*(0.34-0.14*sqrt(ea))*(1.35*Rs/Rso-0.35)
    Rn <- Rns - Rnl # MJ/m^2/day
    days[month] <- days[month]+1
    radiation[month] <- radiation[month] + Rn
  }
  return(radiation/days)
}

monthly.PenmanFAO <- function(clim,loc) {
  ET0 <- vector(mode="numeric", length=12)
  for (month in 1:12) {
    tmax <- clim$TMAX[month]
    tmin <- clim$TMIN[month]
    
    mp <- month -1
    mf <- month +1
    if (month==1) {
      mp <- 12
    } 
    else if (month==12) {
      mf <- 1
    }
      
    tmax.previous <- clim$TMAX[mp]
    tmin.previous <- clim$TMIN[mp]
    
    tmax.following <- clim$TMAX[mf]
    tmin.following <- clim$TMIN[mf]
    
    u2 <- clim$WND[month]  # m/s
    Rn <- clim$RAD[month]
    
    tmean <- (tmax + tmin)/2 # C°
    tmean.previous <- (tmax.previous + tmin.previous)/2 
    tmean.following <- (tmax.following + tmin.following)/2 
    
    etmx <- 0.611*exp((17.27*tmax)/(tmax+237.3))
    etmn <- 0.611*exp((17.27*tmin)/(tmin+237.3))
    es <- (etmx+etmn)/2
    
    if ("HUM" %in% colnames(clim)) {
      rh <- clim$HUM[month]
      ea <- rh/100*es  # kPa
    } else if ("TDEW" %in% colnames(clim)) {
      tdew <- clim$TDEW[month]
      ea <- 0.6108*exp(17.27*tdew/(tdew+237.3))
    }
    
    es.minus.ea <- es - ea
    
    
    #es.minus.ea <- (1-rh/100)*es  # kPa
    
    slope <- 2504*exp((17.27*tmean)/(tmean+237.3))/(tmean+237.3)**2  # kPa/C°
    
    G <- 0.07 * (tmean.following-tmean.previous)  # MJ/m^2/day

    
    P <- 101.3*((293-0.0065*loc$ALT)/293)**5.26
    
    lambda <- 2.501 - 2.361e-3*tmean
    y <- 1.63e-3*P/lambda

    ET0[month] <- 0.9*(0.408*slope*(Rn-G)+y*900/(tmean+273)*u2*(es.minus.ea)) / (slope + y*(1+0.34*u2))  # mm/day
    
  }
  return(ET0*monthdays)
}


doy2month <- function(doy) {
  date <- as.Date(doy, format = "%j", origin = "1.1.0")-1
  month(date)
}

doy2month_cwd <- function(doy) {
  date <- as.Date(doy-1, origin = "2011-01-01")
  month(date)
}

# 
# ## Read Climate Data File
#climate <- read.csv(file="/home/jules/Desktop/IrrigCalc/irrigwater2/example_data/handanclim_monthly.csv", header=TRUE, sep=",")
# 
# ## Read Location Data File
#location <- read.csv(file="/home/jules/Desktop/IrrigCalc/irrigwater2/example_data/handanloc.csv", header=TRUE, sep=",")
# 
#climate$RAD <- monthly.radiation(climate,location)
#climate$ET <- monthly.PenmanFAO(climate,location)
