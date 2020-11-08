  source("ETmodel.R",chdir=T)
  library(lubridate)
  
  Kc_by_day <- function(crop,day){
    #TODO: adjust KC by climate (wind and RH)
    #TODO: smoother interpolation
    anchor_days=c(0,crop$DAYSINIT,crop$DAYSINIT+crop$DAYSDEVELOP-1,crop$DAYSINIT+crop$DAYSDEVELOP+crop$DAYSMID-1,crop$DAYSINIT+crop$DAYSDEVELOP+crop$DAYSMID+crop$DAYSLATE)
    values=c(crop$KCINIT,crop$KCINIT,crop$KCMID,crop$KCMID,crop$KCEND)
    interp <- approxfun(anchor_days,values)
    result <- interp(day)
    if (is.na(result)) {
      return(0)
    } else {
      return(interp(day)) 
    }
  }
  
  Zroot_by_day <- function(crop,day){
    anchor_days=c(0,crop$DAYSINIT+crop$DAYSDEVELOP,crop$DAYSINIT+crop$DAYSDEVELOP+crop$DAYSMID+crop$DAYSLATE)
    values=c(crop$ZROOTINIT,crop$ZROOTMAX,crop$ZROOTMAX)
    interp <- approxfun(anchor_days,values)
    result <- interp(day)
    if (is.na(result)) {
      return(0)
    } else {
      return(interp(day)) 
    }
  }
  
  seasonlength <- function(crop) {
    return(crop$DAYSINIT+crop$DAYSDEVELOP+crop$DAYSMID+crop$DAYSLATE)
  }
  
  seasondays <- function(crop,plantingdate) {
    seq(as.Date(plantingdate), by = "day", length.out = seasonlength(crop))
  }
  
  monthly.generate_randomrainfall <- function(climate) {
    # Randomly generate rainfall within the daily amount of the previous and next month while preserving the monthly mean and number of wetdays <- floor(climate$WETD)
    prcp_monthly <- data.frame(matrix(ncol = 0, nrow = 12))
    n <- floor(climate$WETD)
    prcp_monthly$dailyamout <- climate$PRCP/n
    prcp_monthly$days <- n
    
    out <- data.frame(matrix(0,ncol=1,nrow=365))
    names(out)<-"prcp"
    for (month in 1:12) {
        monthp <- month -1
          monthn <- month +1
        if (month==1) {
            monthp <- 12
        } else if (month==12) {
            monthn <- 1
        }
        min_precip <- min(prcp_monthly$dailyamout[monthp],prcp_monthly$dailyamout[month],prcp_monthly$dailyamout[monthn])
        max_precip  <- max(prcp_monthly$dailyamout[monthp],prcp_monthly$dailyamout[month],prcp_monthly$dailyamout[monthn])
          
        # Randomly generate rainfall events within bounds and while preserving mean.
        events <- runif(prcp_monthly$days[month], min_precip, max_precip)
        events <- events*prcp_monthly$dailyamout[month]/mean(events)
        
        # Randomly choose days when rainfall occurs
        firstdate <- as.Date(sprintf("2003-%02d-01", month))
        firstdoy <- unclass(as.POSIXlt(firstdate))$yday+1
        maxdoy <- firstdoy+days_in_month(firstdate)-1
        days <- sample(firstdoy:maxdoy,prcp_monthly$days[month],replace=FALSE)
        out[days,"prcp"]<-events
    }
    return(out)
  }
  
  monthly.generate_rainfall <- function(climate) {
    # equally generate rainfall within the daily amount of the previous and next month while preserving the monthly mean and number of wetdays <- floor(climate$WETD)
    
    out <- data.frame(matrix(0,ncol=1,nrow=365))
    names(out)<-"prcp"
    for (month in 1:12) {
      firstdate <- as.Date(sprintf("2003-%02d-01", month))
      firstdoy <- unclass(as.POSIXlt(firstdate))$yday+1
      maxdoy <- firstdoy+days_in_month(firstdate)-1
      monthdays <- maxdoy-firstdoy+1
      n <- max(floor(climate$WETD[month]),1)
      #n <- max(monthdays,1)
      if (n>monthdays) {
        n <- monthdays
      }
      events <- rep(climate$PRCP[month]/n,n)
      days <- floor(seq(firstdoy+(monthdays/n-1),maxdoy,length.out=n)) ##equally distributing rainfall events during month 
      out[days,"prcp"]<-events
    }
    return(out)
  }
  
  monthly.generate_rainfall_25 <- function(climate) {
    # equally generate rainfall within the daily amount of the previous and next month while preserving the monthly mean and number of wetdays <- floor(climate$WETD)
    
    out <- data.frame(matrix(0,ncol=1,nrow=365))
    names(out)<-"prcp"
    for (month in 1:12) {
      firstdate <- as.Date(sprintf("2003-%02d-01", month))
      firstdoy <- unclass(as.POSIXlt(firstdate))$yday+1
      maxdoy <- firstdoy+days_in_month(firstdate)-1
      monthdays <- maxdoy-firstdoy+1
      n <- max(floor(climate$WETD[month]),1)
      #n <- max(monthdays,1)
      if (n>monthdays) {
        n <- monthdays
      }
      events <- rep(climate$PRCP_25[month]/n,n)
      days <- floor(seq(firstdoy+(monthdays/n-1),maxdoy,length.out=n)) ##equally distributing rainfall events during month 
      out[days,"prcp"]<-events
    }
    return(out)
  }
  
  data.day2month <- function(dataframe) {
    months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    variablenames <- colnames(dataframe)[colnames(dataframe)!="DOY"]
    aggregated_data <- data.frame(matrix(0,ncol = length(variablenames)+2, nrow = 12))
    names(aggregated_data)<-c("MONTH","CROPDAYS",variablenames)
    days <- 0
    month <- doy2month_cwd(dataframe[1,"DOY"])
    for (type in variablenames) {
      for (i in rownames(dataframe)) {
        doy<-dataframe[i,"DOY"]
        if (doy2month_cwd(doy)!=month) {
          aggregated_data[month,"CROPDAYS"] <- days
          days <- 0
        }
        month <- doy2month_cwd(doy)
        aggregated_data[month,type] <- aggregated_data[month,type]+dataframe[i,type]
        days <- days + 1
      }
    }
    aggregated_data$MONTH<-months
    for (i in variablenames) {
      if (colSums(is.na(aggregated_data))[i] > 0) {
        aggregated_data[,i] <- c(1:length(months))*NA
      }
    }
    return(aggregated_data)
  }
  
  daily.runoff <- function(prcp) {
    
    # CNII <- soil$CNAMC2
    # f <- Dr/TAW
    # CNI <- -16.91+1.348*CNII-0.01379*CNII**2+0.0001172*CNII**3  ##Curve Number approach, Chapter 3 Aquacrop Version 6: Calculation procedures: http://www.fao.org/3/a-br248e.pdf
    # CNIII <- 2.5838+1.9449*CNII-0.014216*CNII**2+0.000045829*CNII**3
    # 
    # wCNIII <- max(1-f*2,0) ##Interpolation of AMC classes 1 to 3. Assumes Field Capacity for CNIII,  wilting point for CNI, halfway between is CNII
    # wCNII <- min(2*f,2-2*f)
    # wCNI <- max(-1+f*2,0)
    # 
    # CN <- wCNI*CNI + wCNII*CNII + wCNIII*CNIII
    # S <- 254*(100/CN-1)
    # if (prcp>0.2*S) {
    #   RO <- (prcp-0.2*S)**2/(prcp+S-0.2*S)
    # } else {
    #   RO <- 0
    # }
    # return(RO)
    return(0)
  }
  
  monthly.soil_water_balance <- function(crop, soil, climate, plantingdate, irrigation_method="threshold", irrigation_threshold=1) {
    # irrigation_method:
    # "threshold" --> irrigation_threshold is fraction of RAW. Defined the moment when Soil Water storage (SWS) is completely refilled
    # "minimum" --> irrigation occurs at RAW=0 / irrigation_threshold=1 and covers ETc for one day. Daily irrigation an minimum SWS
    # "maximum" --> irrigation occurs everytime SWS is below 100%. Irrigation is terminated, such that at harvest SWS is depleted.
    # "monthly" --> SWS is refilled to 100% end of month. If irrigation_threshold is reached, irrigation occurs also during month.
    
    #for monthly climate data
    days<-seasondays(crop,plantingdate)
    df <- data.frame(matrix(ncol = 13, nrow = length(days)))
    names(df)<-c("DOY","Kc","Ks","Dr","RAW","ETo","ETc","ETa","PRCP","PRCPeff","DP","RO","Water_Deficit")
    rainfall <- monthly.generate_rainfall(climate)
    Dr <- 0 # in fraction
    pdef <- crop$PDRY 
    Ks<-1
    ETc<-0
    
    for (i in 1:length(days)) {
      
      day <- days[i]
      doy <- unclass(as.POSIXlt(day))$yday+1
      month <- doy2month(doy)
      
      water_deficit <- 0
      
      # ET calculation
      ET <- climate$ET[month]/monthdays[month]
      Kc <- Kc_by_day(crop, day-as.Date(plantingdate))
      ETc <- Kc*ET
      ETa <- Ks*ETc
      Dr <- Dr + ETa #water deficit after ET
      
      # Soil Water
      TAW <- 1000*(soil$QFC-soil$QWP)*Zroot_by_day(crop,day-as.Date(plantingdate))  #m3/m3 * m *1000mm/m= mm
      p <- pdef #min(max(pdef + 0.04*(5 - ETc),0.1),0.8)
      RAW = p*TAW
      
      # rainfall event
      PRCP <-rainfall$prcp[doy]
      if (PRCP > 0) {
        RO <- daily.runoff(PRCP)
      } else {
        RO <- 0
      }
      DP <- -(min(Dr-(PRCP-RO),0))
      PRCPeff <- PRCP - RO - DP
      Dr <- Dr - PRCPeff
      
      # Irrigation
      if (any(irrigation_method==c("threshold","monthly")) && Dr>irrigation_threshold*RAW) {  #Minimum Methode
        water_deficit <- Dr
        Dr <- 0
      } else if (irrigation_method=="minimum" && Dr>RAW) {
        water_deficit <- Dr-RAW
        Dr <- RAW
      } else if (irrigation_method=="maximum") {
        water_deficit <- Dr
        Dr <- 0
      } else if (irrigation_method=="monthly") {
        if (month != doy2month(doy+1) || i==length(days)) { 
          water_deficit <- Dr
          Dr <- 0
        }
      }
      
      df[i,]<-c(doy,Kc,Ks,Dr,RAW,ET,ETc,ETa,PRCP,PRCPeff,DP,RO,water_deficit)
    }
    
    # TODO: recalculate Dr and PRCPeff
    # surplus <- RAW-Dr
    # if (surplus>0) {
    #   for (i in length(days):1) {
    #     if (surplus >0 & df$Water_Deficit[i]>0) {
    #       surplus<-surplus-df$Water_Deficit[i]
    #       df$Dr[i]=df$Dr[i]+df$Water_Deficit[i]
    #       df$Water_Deficit[i]=0
    #       if (surplus<0) {
    #         df$Water_Deficit[i]<- -surplus
    #       }
    #     }
    #   }
    # }
    
    # If 1st quantile data of monthly P is available, compute the irrigation water demand for 
    # 75% reliability
    if("PRCP_25" %in% colnames(climate)) {
      if (sum(is.na(climate$PRCP_25))==0) {
      days_25<-seasondays(crop,plantingdate)
      df_25 <- data.frame(matrix(ncol = 13, nrow = length(days_25)))
      names(df_25)<-c("DOY_25","Kc_25","Ks_25","Dr_25","RAW_25","ETo_25","ETc_25","ETa_25","PRCP_25","PRCPeff_25","DP_25","RO_25","Water_Deficit_25")
      rainfall_25 <- monthly.generate_rainfall_25(climate)
      Dr_25 <- 0 # in fraction
      pdef <- crop$PDRY 
      Ks_25<-1
      ETc_25<-0
      
      for (i in 1:length(days_25)) {
        
        day <- days_25[i]
        doy_25 <- unclass(as.POSIXlt(day))$yday+1
        month <- doy2month(doy_25)
        
        water_deficit_25 <- 0
        
        # ET calculation
        ET_25 <- climate$ET[month]/monthdays[month]
        Kc_25 <- Kc_by_day(crop, day-as.Date(plantingdate))
        ETc_25 <- Kc_25*ET_25
        ETa_25 <- Ks_25*ETc_25
        Dr_25 <- Dr_25 + ETa_25 #water deficit after ET
        
        # Soil Water
        TAW_25 <- 1000*(soil$QFC-soil$QWP)*Zroot_by_day(crop,day-as.Date(plantingdate))  #m3/m3 * m *1000mm/m= mm
        p <- pdef #min(max(pdef + 0.04*(5 - ETc),0.1),0.8)
        RAW_25 = p*TAW_25
        
        # rainfall event
        PRCP_25 <-rainfall_25$prcp[doy_25]
        if (PRCP_25 > 0) {
          RO_25 <- daily.runoff(PRCP_25)
        } else {
          RO_25 <- 0
        }
        DP_25 <- -(min(Dr_25-(PRCP_25-RO_25),0))
        PRCPeff_25 <- PRCP_25 - RO_25 - DP_25
        Dr_25 <- Dr_25 - PRCPeff_25
        
        # Irrigation
        if (any(irrigation_method==c("threshold","monthly")) && Dr_25>irrigation_threshold*RAW_25) {  #Minimum Methode
          water_deficit_25 <- Dr_25
          Dr_25 <- 0
        } else if (irrigation_method=="minimum" && Dr_25>RAW_25) {
          water_deficit_25 <- Dr_25-RAW_25
          Dr_25 <- RAW_25
        } else if (irrigation_method=="maximum") {
          water_deficit_25 <- Dr_25
          Dr_25 <- 0
        } else if (irrigation_method=="monthly") {
          if (month != doy2month(doy_25+1) || i==length(days)) { 
            water_deficit_25 <- Dr_25
            Dr_25 <- 0
          }
        }
        
        df_25[i,]<-c(doy_25,Kc_25,Ks_25,Dr_25,RAW_25,ET_25,ETc_25,ETa_25,PRCP_25,PRCPeff_25,DP_25,RO_25,water_deficit_25)
      } 
      } else {
        days_25 <- seasondays(crop,plantingdate)
        df_25 <- data.frame(matrix(ncol = 13, nrow = length(days_25)))
        names(df_25)<-c("DOY_25","Kc_25","Ks_25","Dr_25","RAW_25","ETo_25","ETc_25","ETa_25","PRCP_25","PRCPeff_25","DP_25","RO_25","Water_Deficit_25")
        df_25[i,] <- c(1:13)*NA
      }
      return(data.frame(df,df_25))
    } else { 
      return(df)
    }
    
    
  }
  

