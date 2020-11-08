#server.r

shinyServer(function(input, output, session) {

  ##############GENERAL FUNCTION DEFINITION ####################################
  observeEvent(input$language,{
    tryCatch(Sys.setlocale("LC_TIME", tr('locale')))  
  })
  
  removeUI(
    selector = ".navbar-header"  # removes empty navbar header, that causes white space below titlebar
  )
  
  
  output$languageswitch <- renderUI({
    languages <- colnames(dictionary)
    choices <- 1:length(languages)
    names(choices)<-languages
    radioButtons(inputId = "language", label = "", choices = choices, 
                 selected=1, inline=TRUE)
  })
  tr <- function(text) {
    ## returns the lookupvalue of "text" in the global data.frame "dictionary" 
    ## for the current language
    ## Does also handle lists and vectors. Returns "TRANSLATION MISSING" if no 
    ## translation can be found
    choice <- as.numeric(input$language)
    if (is.null(input$language)) {choice<-1}
    tryCatch({
      sapply(
        text,
        FUN = function(x) {
          index <- match(x,rownames(dictionary))
          if (is.na(index)) {
            out <- paste(x,"NOT FOUND")
          } else {
            translated_string <- dictionary[index,choice]
            if (nchar(translated_string)==0) {
              out <- paste(x,"NOT TRANSLATED")
            } else {
              out <- translated_string
            }
          }
        out
        },
        USE.NAMES = FALSE
      )
    })
  }
  
  mround <- function(x, base) {
    ##### rounds argument "x" to the next multiple of argument "base" #####
    base * ceiling(x / base)
  }
  
  isValidEmail <- function(x) {
    ##### checks if the string in argument "x" is in the format of a valid 
    ##### e-mail adress 
    grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(x),
      ignore.case = TRUE
    )
  }
  
  my_ceiling <- function(x) {
    ##### returns the ceiling/floor of the positive/negative double reduced to 
    ##### one significant digit. 
    ##### e.g. 37.44 -> 40, -0.0067 -> -0.007 
    
    num_string <- format(x, scientific = TRUE)
    n <- strsplit(num_string, "e")
    n1 <- sapply(n, function(x)
      as.numeric(x[1]))
    n2 <- sapply(n, function(x)
      as.numeric(x[2]))
   
    if (n1 >= 0) {
      ceiling(n1) * 10 ^ (n2)
    } else {
      floor(n1) * 10 ^ (n2)
    }
  }
  
  colpaletteplots <- "Dark2"  #RColorBrewer
  
  # Get User IP
  
  IP <- reactive({ input$getIP })
  
  observe({
    userinfo <- IP()
    if (!is.null(userinfo)) {
      tryCatch({
        write(paste(as.character(Sys.time()),userinfo$ip,userinfo$loc,
                    userinfo$country,userinfo$city,sep='\t'),file=logfile,
              append=TRUE)
      })
    }
  })
  
  
  ############## GENERATE REACTIVE OUTPUT ######################################
  
  
  ## TEXTOUTPUT objects as defined in dictionary_UI.csv 
  observe({
    choice <- as.numeric(input$language)
    if (!is.null(input$language)) {
      lapply(1:length(dictionaryUI[[1]]), function(i, choice) {
        output[[paste0(rownames(dictionaryUI)[i])]] <- renderText({
          paste0(dictionaryUI[[i, choice]])
        })
      }, choice = choice)}
  })
  
  #------------- TOOLTIPS -------------------------------------------------------
  observeEvent(input$language,{
    ### The way it works: 1. remove tooltips, 2nd: artificial delay of 0.3s, 
    ### re-create tooltips in other language. Other approaches did not work.
    ### TODO: Create Tooltips with loop. Give tooltip dictonary keys a "tooltip" 
    ### prefix and loop over them

    lang<-input$language
    removeTooltip(
      session=session,
      "timeseriesoraverage"
    )
    removeTooltip(
      session=session,
      "meteo_choice_but2"
    )
    removeTooltip(
      session=session,
      "crop2"
    )
    removeTooltip(
      session=session,
      "soil2"
    )
    removeTooltip(
      session=session,
      "plantingdate2"
    )
    removeTooltip(
      session=session,
      "addtoirrigationplanner2"
    )
    removeTooltip(
      session=session,
      "archive"
    )
    removeTooltip(
      session=session,
      "choosecolormap"
    )
    removeTooltip(
      session=session,
      "radiusselector"
    )
    removeTooltip(
      session=session,
      "timeseriesyesselector"
    )
    removeTooltip(
      session=session,
      "timeseriescontrol"
    )
    removeTooltip(
      session=session,
      "computetimeseries"
    )
    removeTooltip(
      session=session,
      "timeseriesoraverageselector"
    )
    Sys.sleep(0.3)
    addTooltip(
      session=session,
      "timeseriesoraverage",
      title = tr("tooltip_timeseriesoraverage"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "meteo_choice_but2",
      title = tr("tooltip_meteochoice"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "crop2",
      title = tr("tooltip_croptype"),
      placement = "right",
      trigger = "focus",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "soil2",
      title = tr("tooltip_soiltype"),
      placement = "right",
      trigger = "focus",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "plantingdate2",
      title = tr("tooltip_plantingdate"),
      placement = "right",
      trigger = "focus",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "addtoirrigationplanner2",
      title = tr("tooltip_addtoplanningtable"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "archive",
      title = tr("tooltip_archive"),
      placement = "right",
      trigger = "focus",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "choosecolormap",
      title = tr("tooltip_colormap"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "radiusselector",
      title = tr("tooltip_radiusofselection"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "timeseriesyesselector",
      title = tr("tooltip_radiusofselection"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "timeseriescontrol",
      title = tr("tooltip_lengthoftimeseries"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "computetimeseries",
      title = tr("tooltip_computetimeseries"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
    addTooltip(
      session = session,
      "timeseriesoraverageselector",
      title = tr("tooltip_timeseriesoraverageselector"),
      placement = "right",
      trigger = "hover",
      options=list(container="body")
    )
  })
  
  
  #------------- BACKGROUND INFORMATION TAB ------------------------------------
  
  output$mymap <- renderLeaflet({
    lat <- meteo_df$LAT
    lon <- meteo_df$LON
    pp <- paste("[",1:9,"] ",tr(meteo_df$NAME),sep="")
    dat <- data.frame(lat, lon, pp)
    m <- leaflet(dat) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers( ~ lon, ~ lat, label = pp) %>%
      addPolygons(
        data = init_shp,
        weight = 2,
        col = 'grey',
        label = tr("Guantao County")
      )
    m
  })
  
  output$backgroundinfo <- renderUI({
    HTML(paste(tr("backgroundinfotext")))
  })
  
  
  #------------- CLIMATE TAB ---------------------------------------------------

  output$climatedata_selector <- renderUI({
    choices = 1:length(meteo_df$NAME)
    names(choices) = paste("[",1:length(meteo_df$NAME),"] ",tr(meteo_df$NAME)
                           ,sep="")
    choices2 = list(
      "TMIN",
      "TMAX",
      "PRCP",
      "PRCP_25",
      "HUM",
      "WND",
      "ET",
      "WETD",
      "SUNH"
    )
    names(choices2) = tr(choices2)
    tagList(
      checkboxGroupInput(
        "meteodata2view",
        tr("Data Source"),
        choices = choices,
        selected = 1
      ),
      radioButtons("meteodatatype2view", tr("Data Type"), choices = choices2)
    )
    
  })
  
  output$plottimeseriesoraverageselector <- renderUI({
      plotOutput("meteostatistics", height="520px")
  })
  
  output$meteostatistics <- renderPlot({
    if (!is.null(input$meteodata2view)) {
      data = as.data.frame(prepareaveragemeteodata())
      data2show = reshape::melt(data,id.vars = "Month")
      p1 <- ggplot(data2show, aes(x = Month,  y = value,fill=variable)) +
        geom_bar(stat = "identity",position = "dodge") + 
        scale_fill_brewer(palette=colpaletteplots) +
        ylab(tr(input$meteodatatype2view)) +
        xlab("") + theme(legend.position="top")
      p1$labels$fill <- tr("Data Source")
      p1
    }
  })
  
  output$pleaseEditTable <- renderUI({
    HTML(paste(tr("pleaseEditTableText")))
  })
  
  output$stationLongitudeText <- renderUI({
    HTML(paste(tr("stationLongitudeText")))
  })
  
  output$stationLatitudeText <- renderUI({
    HTML(paste(tr("stationLatitudeText")))
  })
  
  output$stationAltitudeText <- renderUI({
    HTML(paste(tr("stationAltitudeText")))
  })
  
  output$editableClimateTable <- renderRHandsontable({
    data2show <- reactiveClimate$climate[[9]]
    data2show <- data2show[ , !(names(data2show)) %in% c("RAD")]
    names(data2show) <- tr(names(data2show))
    rhandsontable(data2show, stretchH = 'all', height = 360, rowHeaders = NULL) %>%
      hot_col(col = tr("MONTH"), readOnly = TRUE) %>%
      #hot_cols(colWidths = c(42,70,70,55,50,55,50,70,60)) %>%
      hot_col(col = tr("ET"), readOnly = TRUE) %>%
      hot_validate_numeric(col = tr("TMIN"), min = -50, max = 80) %>%
      hot_validate_numeric(col = tr("TMAX"), min = -50, max = 80) %>%
      hot_validate_numeric(col = tr("PRCP"), min = 0, max = 500) %>%
      hot_validate_numeric(col = tr("PRCP_25"), min = 0, max = 500) %>%
      hot_validate_numeric(col = tr("HUM"), min = 0, max = 100) %>%
      hot_validate_numeric(col = tr("WND"), min = 0, max = 500) %>%
      hot_validate_numeric(col = tr("WETD"), min = 0, max = 31) %>%
      hot_validate_numeric(col = tr("SUNH"), min = 0, max = 24)
  })
  
 
  #------------- IRRIGATION DEMAND CALCULATOR TAB ------------------------------
  
  output$meteo_choices2 <- renderUI({
    choices = 1:length(meteo_df$NAME)
    names(choices) = paste("[",1:length(meteo_df$NAME),"] ",tr(meteo_df$NAME),
                           sep="")
    
    meteo_choice <- isolate(input$meteo_choice)
    if (is.null(meteo_choice)) {
      default = 1
    } else {
      default = meteo_choice
    }
    # Defaults to Guantao meteo station 
    radioButtons("meteo_choice", label = "", choices = choices, selected=3)  
  })
  
  # Button to press to select station for meteo data
  output$meteo_choice_but2 <- renderUI({  
    bsButton("meteo_choice_but",
             tr("meteodata"),
             type = "toggle",
             style = "default",
             block=TRUE,
             icon = icon("cloud", lib="glyphicon"),
             value = 1  # Default to first value in list of meteo choices
             )
  })
  
  output$crop2 <- renderUI({
    choices <- 1:nrow(crop)
    names(choices) <- tr(rownames(crop))
    
    crop <- isolate(input$crop)
    if (is.null(crop)) { # Preserving User choice when language is switched
      default = 1
    } else {
      default = crop
    }
    selectInput('crop', tr("textcroptype"), choices = choices, selected=default)
  })
  
  

  output$plantingdate2 <- renderUI({
    cropchoice<-as.numeric(req(input$crop))
    startdate <- as.Date(crop$DATERANGESTART[cropchoice], format = '%d.%m.%y')
    enddate <- as.Date(crop$DATERANGEEND[cropchoice], format = '%d.%m.%y')
    
    # Preserving User choice when language is switched
    if (!is.null(isolate(input$plantingdate)) && (isolate(rv2$v) == input$crop)) {
       value <- input$plantingdate
     } else {
       averagedate <- floor((enddate-startdate)/2)+startdate
       value <- averagedate - day(averagedate) + 1
       rv2$v <- input$crop
     }
    
    ## ugly workaround to invalidate daily_results when croptype changes but not 
    ## the plantingdate
    rv$v <- value+runif(1,0,1)/1000 
    langkey <- tr("dateinputlangvalue")
    dateInput(
      "plantingdate",
      tr("plantingdate"),
      language = langkey,
      value = value,
      max = enddate,
      min = startdate,
      format = "d-MM"
    )
  })
  
  cropchoice <- reactiveValues(v = 0)
  
  output$soil2 <- renderUI({
    choices <- 1:nrow(soil)
    names(choices) <- tr(rownames(soil))
    
    soil <- isolate(input$soil)
    if (is.null(soil)) {
      default = 1
    } else {
      default = soil
    }
    
    selectInput('soil', tr("textsoiltype"), choices = choices, selected = default)
  })
  
  output$irrigationstrategy2 <- renderUI({
    # TODO: make slider for threshold
    choices <- 1:length(irrigationstrategies)
    names(choices) <- tr(irrigationstrategies)
    
    irrigationstrategy <- isolate(input$irrigationstrategy)
    # Preserving User choice when language is switched
    if (is.null(irrigationstrategy)) { 
      default = 1
    } else {
      default = irrigationstrategy
    }
    
    selectInput('irrigationstrategy', tr("irrigationstrategy"), 
                choices = choices, selected=default)
  })
  
  output$outputtypeselector <- renderUI({
    choices <- 1:length(outputtypes)
    names(choices) <- tr(outputtypes)
    
    outputtype <- isolate(input$outputtype)
    if (is.null(outputtype)) { # Preserving User choice when language is switched
      default = 5
    } else {
      default = outputtype
    }
    
    checkboxGroupInput(
      "outputtype",
      tr("showinplot"),
      choices = choices,
      selected = default
    )
  })
  
  output$actions <- renderText({
    req(results_monthly())
    tr("Actions")
    })
  
  output$WSbar_Results_show2 <- renderUI({
    req(results_monthly())
    tagList(
      bsButton("WSbar_Results_show",tr("showsoilwaterdepletion"),type = "action",
               icon=icon("area-chart"),block=TRUE),
      bsModal('WSbar_modal', title=tr("WSbar_title"), 
              trigger="WSbar_Results_show",size="large",
              plotOutput('WS_bar_Results'))
    )
  })
   
  output$addtoirrigationplanner2 <- renderUI({
    req(results_monthly())
    bsButton(
             "addtoirrigationplanner",
             tr("addtoplanningtable"),
             type = "action",
             style = "primary",
             icon=icon("copy", lib="glyphicon"),
             block=TRUE
           )
  })
  
  output$barChartormessage <- renderUI({
    #if (!is.null(input$meteo_choice)) {
      plotOutput("barResults",height="475px")
    #} else {
    #  return(tr("nometeodataselectedmessage"))
    #}
  })
  
  output$barResults <- renderPlot({
    outputtype <- req(input$outputtype)
    df.monthly <- req(results_monthly())
    df.reduced <- df.monthly[,c('MONTH',outputtypes[as.numeric(outputtype)])]
    df.reduced$MONTH <- factor(df.reduced$MONTH,levels = tr(monthnames))
    data2show = reshape::melt(df.reduced,id.vars = "MONTH")
    data2show$variable <- factor(tr(data2show$variable),
                                 levels=tr(outputtypes[as.numeric(outputtype)]))
    colors <- outputcolors[as.numeric(outputtype)]

    ggplot(data2show, aes(x = MONTH, y = value, fill = variable)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_fill_manual(values=colors) +
      theme(legend.position="bottom") +
      xlab(tr("month")) +
      ylab("") +
      guides(fill=guide_legend(title=NULL))
 })
  
   output$tableResults  <- DT::renderDataTable({
     df.monthly <- req(results_monthly())
     df.monthly <- df.monthly[,c('MONTH',outputtypes)]
     rownames(df.monthly) <- df.monthly$MONTH
     df.monthly$MONTH <- NULL
     df.monthly['SUM',] <- colSums(df.monthly)
     df.monthly <- round(df.monthly,0)
     names(df.monthly) <- tr(names(df.monthly))
     rownames(df.monthly)[13] <- tr(rownames(df.monthly)[13])
     datatable(t(df.monthly), rownames = TRUE, escape = F, 
               options = list(paging = FALSE, searching = FALSE, ordering=FALSE, 
                              bInfo=FALSE),selection="none",style="bootstrap")
     
   })
  
   
 output$WS_bar_Results <- renderPlot({
   input$language
   df.out <- req(results_daily())
   
   df.raw <- df.out[,c('DOY','RAW')]

   dummyyear<-2006
   DATE<-c(doy2date(df.raw$DOY[1],dummyyear))
   for (i in 2:nrow(df.raw)) {
     DOY <- df.raw$DOY[i]
     if (DOY < df.raw$DOY[i-1]) {
       dummyyear <- dummyyear+1
     }
     DATE[i] <- doy2date(DOY,dummyyear)
   }
   df.raw$DATE <- DATE
   df.raw$DOY <- NULL
   
   df.out<-df.out[,c('DOY','Dr','PRCPeff','Water_Deficit')]
   df.out$DATE <- DATE
   df.out$DOY <- NULL
   names(df.out)[names(df.out) == 'Water_Deficit'] <- 'Irrigation'
   df.show = reshape::melt(df.out,id.vars = "DATE")
   df.show$variable<-factor(tr(df.show$variable),
                            tr(c('PRCPeff','Irrigation','Dr')))
   ggplot(df.show) + 
         geom_bar(position="stack",stat="identity",width = 1, 
                  aes(x = DATE, y = value, fill=variable)) +
         scale_x_date(breaks = date_breaks("months"),labels=date_format("%b")) +
         scale_fill_manual(values=c("#1F78B4", "#1B9E77", "#4D4D4D")) +
         theme(legend.position="bottom") +
         xlab(tr("month")) +
         ylab(tr("volumeinmm")) +
         #ylim(0, max(df.raw$RAW)*1.1) +
         geom_line(data = df.raw, 
                   aes(x=DATE, y=RAW, linetype=tr("wiltingpoint"))) +
         labs(fill="",linetype="") 
       })
 
 output$downloadCropSoilCoefficients2 <- renderUI({
     downloadButton("downloadCropSoilCoefficients",
                    tr("downloadCropSoilCoefficients"), 
                    icon = icon("download", lib = "glyphicon"),
                    class="btn-block")
 })
 
 output$downloadCropSoilCoefficients <- downloadHandler(
   filename = function() {
     paste('crop_soil_coefficients', Sys.Date(), '.xlsx', sep='')
   },
   content = function(con) {
     DT <- crop
     rownames(DT) <- tr(rownames(DT))
     DT2 <- soil
     rownames(DT2) <- tr(rownames(DT2))
     write.xlsx(DT, file=con, sheetName = tr("textcroptype"))
     write.xlsx(DT2, file=con, sheetName = tr("textsoiltype"),append=TRUE)
   }
 )
 

 #------------- IRRIGATION PLANNING TABLE -------------------------------
 
  output$nodatainplanningtable <- renderUI({
    summarydata <- irrigationplanning$summary
    if (empty(summarydata)) {
      return(tr("nodata2show"))
    }
  })
  
  output$planningtable <- DT::renderDataTable({
    summarydata <- irrigationplanning$summary
    if (!empty(summarydata)) {
      summarydata$plantingdate <- format(as.Date(summarydata$plantingdate), 
                                         "%d-%b")
      summarydata$applicationefficiency <- NULL
      summarydata$conveyanceefficiency <- NULL
      
      summarydata$Actions <- paste0(
        '<div class="btn-group" role="group" aria-label="Basic example">
       <button type="button" class="btn btn-danger delete" id=delete_',
        1:nrow(irrigationplanning$summary),'>',tr("delete"),'</button></div>')
      
      index=match(gsub(x=isolate(input$checked_rows[-1]),"ID",""),
                  summarydata$ID)
      index <- index[!is.na(index)]
      string <- paste0('<input type="checkbox" name="row_selected" value="ID',
                       irrigationplanning$summary$ID,'">')
      summarydata$showinplot <- sapply(seq_along(string),FUN = function(x) {
        if (is.element(x,index)) {
          gsub("name=\"row_selected\"","name=\"row_selected\" checked",string[x])
        } else {
          string[x]
        }
      })
      
      totalarea = paste(tr("Total"), ": ", sum(summarydata$area), sep = "")
      totaldemand = paste(tr("Total"), ": ", sum(summarydata$demand), sep ="")
      totaldemand_25 = paste(tr("Total"), ": ", sum(summarydata$demand_25), sep="")
      totalnorm = paste(tr("Total"), ": ", sum(summarydata$norm_50), sep="")
      totalnorm_25 = paste(tr("Total"), ": ", sum(summarydata$norm_75), sep="")
      averageeff = paste(tr("volumetricaverage"), ": ", 
                         round(sum(summarydata$demand*summarydata$efficiency)/
                                 sum(summarydata$demand)), 
                         sep = "")
      
      summarydata$crop <- tr(summarydata$crop)
      summarydata$soil <- tr(summarydata$soil)
      summarydata$irrigationstrategy <- tr(summarydata$irrigationstrategy)
      colnames(summarydata) <- tr(colnames(summarydata))
      summarydata <- summarydata[,c(1,2,5,3,4,6,7,8,9,10,11,12,13,14)] #change order
      sketch <- htmltools::withTags(table(tableHeader(names(summarydata)), 
                                          tableFooter(c("", "", "",totalarea, 
                                                        averageeff, "", "", "", 
                                                        totaldemand, 
                                                        totaldemand_25, 
                                                        totalnorm, totalnorm_25, 
                                                        "",""))
                                          ))
      # Display NA in datatable instead of blank
      options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
      datatable(
        summarydata,
        container = sketch,
        rownames = FALSE,
        escape = F,
        options = list(paging = FALSE, searching = FALSE, ordering=FALSE, 
                       bInfo=FALSE,
                       columnDefs = list(list(targets = 1,
                                              render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}")))
        ),
        selection="none",
        style="bootstrap") %>% 
        formatStyle("ID", 
                    backgroundColor = styleEqual(summarydata$ID, 
                                                 irrigationplanning$colors))
    }
  })
 
  
    output$barChart2 <- renderPlot({
      dat <- irrigationplanning$data
      if (!empty(dat)) {
        total=colSums(dat)
        index=match(gsub(x=input$checked_rows[-1],"ID",""),dat$ID)
        index <- index[!is.na(index)]
        if (length(index)!=0) {
          dat<-dat[as.numeric(index),]
          allothers <- total - colSums(dat)
          dat<-rbind(dat,allothers)
        } else {
          dat <- dat[1,]
          dat[1,] <- total
        }

        ylabel <- "demand"
        
        dat$ID[nrow(dat)]<-tr("unselected")
        dat$ID <- factor(dat$ID, 
                         levels = c(tr("unselected"),
                                    dat$ID[dat$ID!=tr("unselected")]))
        names(dat) <- tr(names(dat))
        dat2 <- melt(dat, id.vars = "ID")
        dat2$value <- as.numeric(dat2$value)
        colors <- c("#000000",irrigationplanning$colors[index])
        plotout <- ggplot(dat2, aes(x = variable, y = value, fill = ID)) +
          geom_bar(position="stack", stat = "identity") +
          scale_fill_manual(values=colors) +
          theme(legend.position="bottom") +
          xlab(tr("month")) +
          ylab(tr(ylabel))
        plotout
        
      }
    })
    
    output$relativedemand3 <- renderUI({
      dat <- irrigationplanning$data
      if (!empty(dat)) {
        checkboxInput("relativedemand", label="test",value=FALSE)
      }
    })
    
    output$downloadIrrigationplanningtable2 <- renderUI({
      if (empty(irrigationplanning$summary)) {
        disabled(downloadButton("downloadIrrigationplanningtable",
                                tr("downloadtable"), 
                                icon = icon("download", lib = "glyphicon"),
                                class="btn-block"))
      } else {
        downloadButton("downloadIrrigationplanningtable",tr("downloadtable"), 
                       icon = icon("download", lib = "glyphicon"),
                       class="btn-block")
      }
    })
      
    
    output$downloadIrrigationplanningtable <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.xlsx', sep='')
      },
      content = function(con) {
        DT<-irrigationplanning$summary
        if (!empty(DT)) {
          #DT$plantingdate <-
          #  paste(format(as.Date(DT$plantingdate), "%d"), tr(format(as.Date(
          #    DT$plantingdate
          #  ), "%b")), sep = ".")
          names(DT) <- tr(names(DT))
          names(DT)[9] <- paste(names(DT[9]),tr("dateformatxlsx"),sep=" ")
          DT[, 7] <- tr(DT[, 7])
          DT[, 8] <- tr(DT[, 8])
          DT[,10] <- tr(DT[,10])
        }
        
        DT2 <- irrigationplanning$data
        names(DT2)<-tr(names(DT2))
        
        DT2_25 <- irrigationplanning$data_25
        names(DT2_25) <- tr(names(DT2_25))
        
        write.xlsx(DT, file=con, sheetName = tr("summary"))
        write.xlsx(DT2, file=con, sheetName = tr("demandexcel"),append=TRUE)
        write.xlsx(DT2_25, file=con, sheetName = tr("demandexcel_25"),
                   append=TRUE)
      }
    )
  
  output$storeplanningtablebutton <- renderUI({
    DT <- irrigationplanning$summary
    if (empty(DT)) {
      disabled(bsButton("storetable",tr("storetable"),
                        icon = icon("import", lib="glyphicon"),block=TRUE))
    } else {
      bsButton("storetable",tr("storetable"),
               icon = icon("import", lib="glyphicon"),block=TRUE)
    }
  })
  
  observeEvent(input$storetable, {
    output$userloginmessage <- renderText("")
    showModal(
      modalDialog(
        uiOutput("userloginmessage"),
        checkboxInput("createnewuser",tr("createnewuser"), value = FALSE),
        textInput("storageusername", label = tr("username"), value = ""),
        passwordInput("storagepassword", label = tr("password"), value = ""),
        #shinyjs::hidden(passwordInput("storagepassword2", 
        #                              label = tr("repeatpassword"), 
        #                              value = "")),
        #shinyjs::hidden(textInput("storageemail", label = tr("youremail"), 
        #                          value = "")),
        #shinyjs::hidden(textInput("emailverification", 
        #                          label=tr("emailverificationcode"))),
        footer = tagList(
          modalButton(tr("cancel")),
          actionButton("okstorenow", tr("OK"))
        )
      ))
  })
  
  observeEvent(input$createnewuser, {
    output$userloginmessage <- renderText("")
    if (input$createnewuser) {
      #shinyjs::show("storageemail")
      #shinyjs::show("storagepassword2")
    } else {
      #shinyjs::hide("storageemail")
      #shinyjs::hide("storagepassword2")
    }
  })
  
  observeEvent(input$okstorenow, {
    output$userloginmessage <- renderText("")
    withProgress({
    username <- input$storageusername
    password <- sha256(input$storagepassword)
    #password2 <- sha256(input$storagepassword2) 
    if (!input$createnewuser) {
      query <- dbGetQuery(conn = db,"SELECT * FROM user WHERE username = ?",username)
      if (nrow(query) == 0) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameorpasswordiswrong")))
      } else if (query$password != password & password!=sha256('save')) {  # 'save' is the secret password for editing the example table of user hydrosolutions
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameorpasswordiswrong")))
      } else if (username=='hydrosolutions' & password!=sha256('save')) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameorpasswordiswrong")))
      } else {
        data <- rawToChar(serialize(irrigationplanning$data,connection=NULL,ascii = TRUE))
        summary <- rawToChar(serialize(irrigationplanning$summary,connection=NULL,ascii = TRUE))
        IDcounter <- rawToChar(serialize(irrigationplanning$IDcounter,connection=NULL,ascii = TRUE))
        colors <- rawToChar(serialize(irrigationplanning$colors,connection=NULL,ascii = TRUE))
        query <- dbSendQuery(conn = db,"UPDATE data SET data = ?,summary = ?, IDcounter = ?, colors = ? WHERE username = ?",list(data,summary,IDcounter,colors, username))
        dbClearResult(query)
        removeModal()
      }
    } else {
      #email <- input$storageemail
      email <- "dummy@dummy.du"
      #verificationcode <- substr(md5(paste(username,email,password)),4,8)
      query <- dbGetQuery(conn = db,"SELECT * FROM user WHERE username = ?",username)
      if (nrow(query) > 0) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameexists"))) 
      } else if (username=="" || grepl('[^[:alnum:]]', username)) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameemptyorspecial"))) 
      } else if (input$storagepassword=="") {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("passwordempty"))) 
      #} else if (password2!=password) {
      #  output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("passwordsarenotidentical"))) 
      #} else if (!isValidEmail(email)) {
      #  output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("theprovidedemailisnotvalid"))) 
      #} else if (input$emailverification=="") {
       # 
      #  shinyjs::show("emailverification")
        shinyjs::disable("storageusername")
        #shinyjs::disable("storageemail")
        shinyjs::disable("storagepassword")
        #shinyjs::disable("storagepassword2")
        shinyjs::disable("createnewuser")
        
      } else {  # if (input$emailverification==verificationcode) {
        query <- dbSendQuery(conn = db,"INSERT INTO user (username,password,email) VALUES (?,?,?)",list(username,password,email))
        dbClearResult(query)
        data <- rawToChar(serialize(irrigationplanning$data,connection=NULL,ascii = TRUE))
        summary <- rawToChar(serialize(irrigationplanning$summary,connection=NULL,ascii = TRUE))
        IDcounter <- rawToChar(serialize(irrigationplanning$IDcounter,connection=NULL,ascii = TRUE))
        colors <- rawToChar(serialize(irrigationplanning$colors,connection=NULL,ascii = TRUE))
        query <- dbSendQuery(conn = db,"INSERT INTO data (username,data,summary, IDcounter, colors) VALUES (?,?,?,?,?)",list(username,data,summary,IDcounter,colors))
        dbClearResult(query)
        removeModal()
      } #else {
        #output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("wrongverificationcode")))
      #} 
    }
     }, message = tr("sending"))
  })
  
  output$loadplanningtablebutton <- renderUI({
    bsButton("loadtable",tr("loadtable"),icon = icon("export", lib="glyphicon"), block=TRUE)
  })
  
  observeEvent(input$loadtable, {
    output$userloginmessage <- renderText("")
    showModal(
      modalDialog(
        uiOutput("userloginmessage"),
        textInput("loadingusername", label = tr("username"), value = ""),
        passwordInput("loadingpassword", label = tr("password"), value = ""),
        footer = tagList(
          modalButton(tr("cancel")),
          actionButton("okloadnow", tr("OK"))
        )
      ))
  })
  
  observeEvent(input$okloadnow, {
      output$userloginmessage <- renderText("")
      username <- input$loadingusername
      userquery <- dbGetQuery(conn = db,"SELECT * FROM user WHERE username = ?",username)
      if (nrow(userquery) == 0) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameorpasswordiswrong")))
      } else if (userquery$password != sha256(input$loadingpassword)) {
        output$userloginmessage <- renderText(HTML("<div style='color:red'>",tr("usernameorpasswordiswrong")))
      } else {
        dataquery <- dbGetQuery(conn = db,"SELECT * FROM data WHERE username = ?",username)
        irrigationplanning$data <- unserialize(charToRaw(dataquery$data))
        irrigationplanning$summary <- unserialize(charToRaw(dataquery$summary))
        irrigationplanning$IDcounter <- unserialize(charToRaw(dataquery$IDcounter))
        irrigationplanning$colors <- unserialize(charToRaw(dataquery$colors))
        removeModal()
      }
      
  })
  
  observeEvent(input$clearplanningtable, {
    irrigationplanning$summary <- data.frame()
    irrigationplanning$data <- data.frame()
    irrigationplanning$IDcounter <- 1
    irrigationplanning$colors <- NULL
  })
  
  output$clearplanningtable2 <- renderUI({
    if (empty(irrigationplanning$summary)) {
      disabled(bsButton("clearplanningtable",tr("cleartable"),type = "action",style = "danger",block=TRUE,icon = icon("trash", lib="glyphicon")))
    } else {
      bsButton("clearplanningtable",tr("cleartable"),type = "action",style = "danger",block=TRUE,icon = icon("trash", lib="glyphicon"))
    }
    })
  
  
  #------------- LIVE NDVI TAB --------------------------------------------------
  
  filelist <- reactive({
    files <- dbGetQuery(data_db,"SELECT date,filepath FROM geotiffs WHERE catchmentid=?",req(input$region))
  })
  
  output$jlkbfakbfk <- renderUI({
    choices <- dbGetQuery(data_db,"SELECT ID FROM settings")$ID[-1]  # NDVI map for NCP cannot be displayed, remove masterregion from list
    names(choices) <- tr(dbGetQuery(data_db,"SELECT name FROM settings")$name[-1])  # NDVI map for NCP cannot be displayed, remove masterregion from list
    selectInput('region',
                tr("regiontext"),
                choices = choices)
  })
  
  
  output$archive <- renderUI({
    filelist <- filelist()
    filelist <- filelist[order(filelist$date,decreasing=TRUE),]
    choices <- filelist$filepath
    names(choices) <- as.character(as.Date(filelist$date)+15)
    selectInput('archive',
                tr("archivetext"),
                choices = choices)
  })
  

  
  output$choosecolormap <- renderUI({
    choices <- as.list(c("natural", "highcontrast"))
    names(choices) <-
      sapply(c("natural", "highcontrast"), function(x) {
        tr(x)
      })
    radioButtons(
      inputId = "colormap",
      label = tr("colormap"),
      choices = choices
    )
  })
  

  
  output$radiusselector <- renderUI({
    sliderInput(
      "radius",
      tr("radiusofselection"),
      min = 125,
      max = 10000,
      value = 500,
      step = 125
    )
  })
  

  
  output$timeseriesyesselector <- renderUI({
    checkboxInput("timeseriesyes",
                  label = tr("computetimeseries"),
                  value = FALSE)
  })
  

  
  output$timeseriescontrol <- renderUI({
    files <- filelist()
    length <- nrow(files)
    sliderInput(
      "timeserieslength",
      tr("lengthoftimeseries"),
      min = 1,
      max = length,
      value = 3,
      step = 1
    )
  })
  

  
  output$NDVIsource <- renderUI({
    selected = req(input$archive)
    files <- filelist()
    files = setNames(as.character(files$filepath),
                     as.character(files$date))
    index = match(selected, files)
    helpText(tr("dateofacquisition"),
             ': ',
             as.character(names(files[index])),
             ' - ',
             as.character(as.Date(names(files[index])) + 15))
  })
  

  
  
  #------------- HELP TAB -------------------------------------------------------  
  
  output$helpinfo1 <- renderUI({
    HTML(paste(tr("helpinfotext1")))
  })
  
  output$normFactorTable <- DT::renderDataTable({
    data <- data.frame(Category = c(tr("conveyanceefficiency"),
                                      tr("conveyanceefficiency"),
                                      tr("conveyanceefficiency"),
                                      tr("conveyanceefficiency"),
                                      tr("conveyanceefficiency"),
                                      tr("conveyanceefficiency"),
                                      tr("applicationefficiency"),
                                      tr("applicationefficiency"),
                                      tr("applicationefficiency"),
                                      tr("applicationefficiency"),
                                      tr("applicationefficiency"),
                                      tr("watersource"),
                                      tr("watersource")),
                       Factor=c(tr("largesw"),tr("mediumsw"),tr("smallsw"),
                               tr("largegw"),tr("mediumgw"),tr("smallgw"),
                               tr("channelliningsw"),tr("floodsw"),
                               tr("boardirriggw"),tr("pipeirriggw"),
                               tr("dripirriggw"),tr("wssurfacewater"),
                               tr("wsgroundwater")),
                       Temp = c(1.12,1.05,1,1.15,1.07,1,0.92,1,0.95,0.88,0.5,
                                  1.05,1))
    datatable(data, rownames = FALSE, editable = FALSE, 
              colnames = tr(c("category","factor","value")),
              options = list(paging = FALSE, searching = FALSE, ordering=FALSE, 
                             bInfo=FALSE))
  })

  output$helpinfo2 <- renderUI({
    HTML(paste(tr("helpinfotext2")))
  })
  
  output$helpinfo3 <- renderUI({
    HTML(paste(tr("helpinfotext3")))
  })
  
  output$slidercaptcha2 <- renderUI({
    captcha$result
    sliderInput("slidercaptcha", tr("bringdotsinline"), 0, captchachoices-1, 
                value=0, step=1)
  })
  
  output$barcaptcha <- renderPlot({
    vec <- captcha$result
    
    data <- vec+(mean(vec)-vec)**2/(req(input$slidercaptcha)-vec)
    data=data.frame(name=1:5,value=data)
    # Barplot
    ggplot(data, aes(x=value, y=name)) + geom_point() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank())
  },width = 300, height = 80)
  
   output$messagebox <- renderUI({
     textAreaInput("message",
                   label = tr("message"),
                   value = tr("yourfeedback"))
   })
   
   output$sender2 <- renderUI({
     textInput("sender", tr("youremail"), value = "")
   })
   
   output$subject2 <- renderUI({
     textInput("subject", tr("subject"), value = "")
   })
  
  
  output$send2 <- renderUI({
    actionButton("send", tr("submit"))
  })
  
  
  ############## SERVER REACTIVES ################################################
  
  #------------- CLIMATE TAB ----------------------------------------------------
  reactiveClimate <- reactiveValues(climate = default_climate, location = default_location)  # Compute ET0 in global.r for each meteo station
  
  # reactiveClimate <- reactiveValues()
  
  # observe({
  #   if (input$recalculateET==0) {
  #     meteoselection <- 1:(length(meteo_df$NAME))
  #     clim <- list()
  #     loc <- list()
  #     for (item in meteoselection) {
  #       temp_clim <- read.csv(as.character(meteo_df[item,"DATAFILE"]),header = TRUE, stringsAsFactors = FALSE)
  #       loc[[item]] <- data.frame(LAT=meteo_df[item,"LAT"],LON=meteo_df[item,"LON"],ALT=meteo_df[item,"ALT"])
  #       if (item == 4) {
  #         if (!is.null(input$stationLatitude)) {
  #           loc[[4]]$LAT <- input$stationLatitude
  #         }
  #         if (!is.null(input$stationLongitude)) {
  #           loc[[4]]$LON <- input$stationLongitude
  #         }
  #         if (!is.null(input$stationAltitude)) {
  #           loc[[4]]$ALT <- input$stationAltitude
  #         }
  #       }
  #       temp_clim$RAD <- monthly.radiation(temp_clim,loc[[item]])
  #       temp_clim$ET <- as.numeric(monthly.PenmanFAO(temp_clim,loc[[item]]))
  #       clim[[item]] <- temp_clim
  #     }
  #     reactiveClimate$climate <- clim
  #     reactiveClimate$location <- loc
  #   }
  # })
  
  observeEvent(input$recalculateET, {
    reactiveClimate$location[[9]]$LAT <- input$stationLatitude
    reactiveClimate$location[[9]]$LON <- input$stationLongitude
    reactiveClimate$location[[9]]$ALT <- input$stationAltitude
    if (!is.null(input$editableClimateTable)) {
      reactiveClimate$climate[[9]] <- hot_to_r(input$editableClimateTable)
      names(reactiveClimate$climate[[9]]) <- c("MONTH","WETD","PRCP","PRCP_25",
                                               "TMAX","TMIN","HUM","SUNH","WND",
                                               "ET")
      reactiveClimate$climate[[9]]$RAD <- monthly.radiation(reactiveClimate$climate[[9]],
                                                            reactiveClimate$location[[9]])
      reactiveClimate$climate[[9]]$ET <- as.numeric(monthly.PenmanFAO(reactiveClimate$climate[[9]],
                                                                      reactiveClimate$location[[9]]))
    }
  })
  
  prepareaveragemeteodata <- reactive({
    clim <- reactiveClimate$climate
    meteoselection <- input$meteodata2view
    datatype <- input$meteodatatype2view
    xtslist=data.frame(matrix(ncol=0,nrow = 12))
    for (item in meteoselection) {
      df = clim[[as.numeric(item)]]
      index <- match(datatype,colnames(df))
      if (is.na(index)) {
        data <- cbind(rep(NA,1,12))
      } else {
        data <- df[index]
      }
      xtslist$y = signif(unname(data), 2)
      names(xtslist)[names(xtslist) == "y"] <- tr(meteo_df[item,"NAME"])
    }
    xlabel = tr(
      c(
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec"
      )
    )
    xtslist$Month = xlabel
    xtslist$Month <- factor(xtslist$Month, levels = xlabel)
    xtslist
  })
  

  #------------- IRRIGATION DEMAND CALCULATOR TAB -------------------------------
  # Jules implementation, non-reactive to user specified climate data
  #climate <- reactive({
  #  choice <- req(input$meteo_choice)
  #  climate <- read.csv(as.character(meteo_df[choice,"DATAFILE"]),header = TRUE, stringsAsFactors = FALSE)
  #  location <- data.frame(LAT=meteo_df[choice,"LAT"],LON=meteo_df[choice,"LON"],ALT=meteo_df[choice,"ALT"])
  #  climate$RAD <- monthly.radiation(climate,location)
  #  climate$ET <- as.numeric(monthly.PenmanFAO(climate,location))
  #  climate
  #})
  
  # Reactive climate
  climate <- reactive({
    # Assign default meteo choice
    if(!is.null(input$meteo_choice)) {
      choice <- as.numeric(req(input$meteo_choice))
    } else {
      choice <- 1
    }
    climate <- reactiveClimate$climate[[choice]]
    climate
  })
  
  rv <- reactiveValues(v = 0)
  rv2 <- reactiveValues(v = 0)
  
  observeEvent(input$plantingdate,{
    if (abs(rv$v-input$plantingdate)>0.001) {rv$v <- input$plantingdate} #differences between croptype change or plantingdate change
  })

   results_daily <- reactive({
     plantingdate <- round(rv$v)
     cropparam<-isolate(crop[as.numeric(req(input$crop)),])
     soilparam<-soil[as.numeric(req(input$soil)),]

     climate <- climate()
     selected_strategy <- irrigationstrategies[as.numeric(req(input$irrigationstrategy))]
     out <- monthly.soil_water_balance(cropparam, soilparam, climate, plantingdate, irrigation_threshold=1, irrigation_method=selected_strategy)
   })
   
   results_monthly <- reactive({
     df.daily <- req(results_daily())
     df.monthly<-data.day2month(df.daily)
     df.monthly$MONTH <- tr(df.monthly$MONTH)
     climate <- climate()
     df.monthly$ETo <- climate$ET
     df.monthly$PRCP <- climate$PRCP
     df.monthly$PRCP_25 <- climate$PRCP_25
     df.monthly
   })
   

   observeEvent(input$addtoirrigationplanner, {
     appeffchoices=as.list(as.numeric(irrigationefficiency$eff))
     names(appeffchoices)=tr(irrigationefficiency$type)
     coneffchoices=as.list(as.numeric(conveyanceefficiency$eff))
     names(coneffchoices)=tr(conveyanceefficiency$type)
     watersourcechoices=as.list(as.numeric(watersourcenorm$eff))
     names(watersourcechoices)=tr(watersourcenorm$type)
     showModal(
       modalDialog(
         title = tr("addcroptoplanner"),
           numericInput("plantingarea", tr("plantingarea"), value = 1),
         tags$hr(style="border-color: grey;"),  # a grey line 
         selectizeInput(
           "watersource",
           label=tr("watersource"),
           choices=c(watersourcechoices),
           selected=1),
         tags$hr(style="border-color: grey;"),  # a grey line 
         selectInput(
           "applicationefficiency",
           label=tr("applicationefficiency"),
           choices=appeffchoices,
           selected=1
         ),
         sliderInput("applicationefficiency2",label="",min=0.40,max=1,value=1),
         tags$hr(style="border-color: grey;"),
         selectizeInput(
           "conveyanceefficiency",
           label=tr("conveyanceefficiency"),
           choices=coneffchoices,
           selected=1,
           options = list(duplicates=TRUE)
         ),
         sliderInput("conveyanceefficiency2",label="",min=0.40,max=1.60,value=1),
         tags$hr(style="border-color: grey;"),  # a grey line 
         textInput("entrylabel",label=tr("label"),width="70%"),
         
          bsTooltip(
            "plantingarea",
            title = tr("tooltip_plantingarea"),
            placement = "right",
            trigger = "focus",
            options=list(container="body")
          ),
 
          bsTooltip(
            "applicationefficiency",
            title = tr("tooltip_applicationefficiency"),
            placement = "right",
            trigger = "focus",
            options=list(container="body")
          ),
         bsTooltip(
           "conveyanceefficiency",
           title = tr("tooltip_conveyanceefficiency"),
           placement = "right",
           trigger = "focus",
           options=list(container="body")
         ),
         footer = tagList(
           modalButton(tr("cancel")),
           actionButton("okadditnow", tr("OK"))
         )
       )
     )
     
     
   })
   
   observeEvent(input$plantingarea,{
     if (!is.numeric(input$plantingarea)) {
      updateNumericInput(session=session,"plantingarea",value=1) 
     }
   })
   
   observeEvent(input$applicationefficiency,{
     if (input$applicationefficiency!="NA") {
       val=req(as.numeric(input$applicationefficiency))
       updateSliderInput(session=session,"applicationefficiency2",value=val)
 }
   })
 
   observeEvent(input$applicationefficiency2,{
       if (input$applicationefficiency!=input$applicationefficiency2) {
         appeffchoices=as.list(as.numeric(irrigationefficiency$eff))
         names(appeffchoices)=tr(irrigationefficiency$type)
         updateSelectInput(session=session, inputId="applicationefficiency",selected=appeffchoices[1])
     }}
   )
   
   observeEvent(input$conveyanceefficiency,{
     if (input$conveyanceefficiency!="NA") {
       val=req(as.numeric(input$conveyanceefficiency))
       updateSliderInput(session=session,"conveyanceefficiency2",value=val)
     }
   })
   
   observeEvent(input$conveyanceefficiency2,{
     if (input$conveyanceefficiency!=input$conveyanceefficiency2) {
       # Ugly workaround to the issue that selectInput does not support duplicate values...
       if (!input$conveyanceefficiency=="NA" && abs(as.numeric(input$conveyanceefficiency)-input$conveyanceefficiency2)>=1) {
         coneffchoices=as.list(as.numeric(conveyanceefficiency$eff))
         names(coneffchoices)=tr(conveyanceefficiency$type)
         updateSelectizeInput(session=session, inputId="conveyanceefficiency",selected=coneffchoices[1])
       }
     }}
   )
   
   observeEvent(input$watersourcechoices,{
     if (input$watersourcechoices!="NA") {
       selectizeInput(session=session,"watersource",value=2) 
     }
   })

  
  
  #------------- IRRIGATION PLANNING TAB ---------------------------------------
  
   irrigationplanning <- reactiveValues(summary = data.frame(), 
                                        data = data.frame(), 
                                        data_25 = data.frame(), IDcounter=1, 
                                        colors=NULL)
   
   observeEvent(input$okadditnow, {
     totalirrigationefficiency<-as.numeric(input$applicationefficiency2)*
       as.numeric(input$conveyanceefficiency2)*
       as.numeric(input$watersource)
     id = irrigationplanning$IDcounter
     irrigationplanning$IDcounter <- irrigationplanning$IDcounter+1
     out <- results_monthly()
     waterdemand <-round(sum(out$Water_Deficit) * totalirrigationefficiency * 
                           1 / 1000 * input$plantingarea * 667, 1)
     waterdemand_25 <-round(sum(out$Water_Deficit_25) * 
                              totalirrigationefficiency * 1 / 1000 * 
                              input$plantingarea * 667, 1)
     crop_temp <- row.names(crop[as.numeric(input$crop),])
     soil_temp <- row.names(soil[as.numeric(input$soil),])
     norm_temp_50 <- norm[(norm$crop_name==crop_temp)&
                            (norm$soil_type==soil_temp)&
                            (norm$reliability_percent==50),]$cwd_m3_per_mu
     if (length(norm_temp_50)==0) {
       norm_temp_50 <- NA
     }
     norm_temp_75 <- norm[(norm$crop_name==crop_temp)&
                            (norm$soil_type==soil_temp)&
                            (norm$reliability_percent==75),]$cwd_m3_per_mu
     if (length(norm_temp_75)==0) {
       norm_temp_75 <- NA
     }
     # Implement factors for farm size, irrigation method, and water source
     norm_temp_50 <- norm_temp_50 * totalirrigationefficiency
     norm_temp_75 <- norm_temp_75 * totalirrigationefficiency
     
     # Calculate area
     area_temp <- input$plantingarea
     norm_temp_50 <- norm_temp_50 * area_temp
     norm_temp_75 <- norm_temp_75 * area_temp
     
     # Multiply water demand for vegetables by a factor of 2
     if (length(grep("Cucumber",crop_temp)) > 0) {
       waterdemand[crop_temp=="Cucumber"] = 2 * waterdemand[crop_temp=="Cucumber"]
       waterdemand_25[crop_temp=="Cucumber"] = 2 * 
         waterdemand_25[crop_temp=="Cucumber"]
     } 
     if (length(grep("Tomato",crop_temp)) > 0) {
       waterdemand[crop_temp=="Tomato"] = 2 * waterdemand[crop_temp=="Tomato"]
       waterdemand_25[crop_temp=="Tomato"] = 2 * 
         waterdemand_25[crop_temp=="Tomato"]
     }
     if (length(grep("Vigna Radiata",crop_temp)) > 0) {
       waterdemand[crop_temp=="Vigna Radiata"] = 2 * 
         waterdemand[crop_temp=="Vigna Radiata"]
       waterdemand_25[crop_temp=="Vigna Radiata"] = 2 * 
         waterdemand_25[crop_temp=="Vigna Radiata"]
     }
     if (length(grep("Vegetables",crop_temp)) > 0) {
       waterdemand[crop_temp=="Vegetables"] = 2 * 
         waterdemand[crop_temp=="Vegetables"]
       waterdemand_25[crop_temp=="Vegetables"] = 2 * 
         waterdemand_25[crop_temp=="Vegetables"]
     }
     if (length(grep("Roots",crop_temp)) > 0) {
       waterdemand[crop_temp=="Roots"] = 2 * waterdemand[crop_temp=="Roots"]
       waterdemand_25[crop_temp=="Roots"] = 2 * waterdemand_25[crop_temp=="Roots"]
     }
     
     summary <-
       data.frame(
         ID = id,
         label = input$entrylabel,
         area = area_temp,
         conveyanceefficiency = round(input$applicationefficiency2, digits=2),
         applicationefficiency = round(input$conveyanceefficiency2, digits=2),
         efficiency = round(totalirrigationefficiency, digits=2),
         crop = crop_temp,
         soil = soil_temp,
         plantingdate = input$plantingdate,
         irrigationstrategy = irrigationstrategies[as.numeric(input$irrigationstrategy)],
         demand = waterdemand,
         demand_25 = waterdemand_25,
         norm_50 = round(norm_temp_50,1),
         norm_75 = round(norm_temp_75,1),
         stringsAsFactors = FALSE
       )
     irrigationplanning$summary <- rbind(irrigationplanning$summary, summary)
     
     monthlydata <- out$Water_Deficit / totalirrigationefficiency * 1 / 1000 * 
       input$plantingarea * 667
     monthlydata_25 <- out$Water_Deficit_25 / totalirrigationefficiency * 
       1 / 1000 * input$plantingarea * 667
     monthlydata <-data.frame(t(c(id, monthlydata)), stringsAsFactors = FALSE)
     monthlydata_25 <-data.frame(t(c(id, monthlydata_25)), stringsAsFactors = FALSE)
     colnames(monthlydata) <- c('ID','Jan','Feb','Mar','Apr','May','Jun','Jul',
                                'Aug','Sep','Oct','Nov','Dec')
     colnames(monthlydata_25) <- c('ID','Jan','Feb','Mar','Apr','May','Jun',
                                   'Jul','Aug','Sep','Oct','Nov','Dec')
     irrigationplanning$data <- rbind(irrigationplanning$data, monthlydata)
     irrigationplanning$data_25 <- rbind(irrigationplanning$data_25, 
                                         monthlydata_25)
     #irrigationplanning$colors <- distinctColorPalette(nrow(irrigationplanning$summary))
     irrigationplanning$colors <- sample(grDevices::colors()[grep('gr(a|e)y',
                                                                  grDevices::colors(), 
                                                                  invert = T)], 
                                         nrow(irrigationplanning$summary))
     removeModal()
   })
   
   observeEvent(input$lastClick, {
     row_to_del = as.numeric(gsub("delete_", "", input$lastClickId))
     irrigationplanning$data <- irrigationplanning$data[-row_to_del, ]
     irrigationplanning$summary <- irrigationplanning$summary[-row_to_del, ]
     irrigationplanning$colors <- irrigationplanning$colors[-row_to_del]
   })
   
   
  
  #------------- LIVE NDVI TAB --------------------------------------------------
   
  rastermap <- reactiveValues()
  
  observeEvent(input$archive, {
    # load Map Raster of selected Region
    selection <- file.path(data_root_path,req(input$archive))
    r <- raster(selection)/100
    r[r<0] <- -1
    rastermap$r <- r
  })

  
  dygraph_reset <- reactive({
    click <- input$NDVI_click
    output$timeseries <- renderDygraph({
      data <- getregionaltimeseries()
      if (!is.null(data)) {
        dygraph(data) %>%
          dySeries("V1", label = tr("enhancedvegetationindex")) %>%
          dyAxis(
            name = 'y',
            valueRange = c(0, 1),
            label = tr("enhancedvegetationindex")
          ) %>%
          dyOptions(fillGraph = TRUE,colors = RColorBrewer::brewer.pal(3, colpaletteplots)) %>%
          dyRangeSelector(height = 20)
      }
    })
  })
  
  # get colormap
  pal <- reactive({
    if (input$colormap == "natural") {
      colormap <-
        read.table("data/colormap_natural.csv",
                   header = TRUE,
                   sep = ",")
    } else {
      colormap <-
        read.table("data/colormap_highcontrast.csv",
                   header = TRUE,
                   sep = ",")
    }
    palette <-
      colorNumeric(rgb(colormap / 255), c(-1, 1), na.color = 'transparent')
  })
  
  # Render Leaflet Map and reset Checkbox "Compute Timeseries"
  observeEvent(input$archive, {
    output$NDVI <- renderLeaflet({
      withProgress(message = 'Reading Data ...', {
        #updateCheckboxInput(session, 'timeseriesyes',  label = "Compute Time Series", value = FALSE)
        #updateButton(session,inputId='computetimeseries',disabled=TRUE)
        palette <- pal()
        r <- req(isolate(rastermap$r))
        incProgress(0.5)
        NDVI <- leaflet()
        incProgress(0.2)
        m <-
          leafletProxy('NDVI') # use the proxy to save computation
        removeShape(map = m, layerId = "circle")
        removeMarker(map = m, layerId = "popup")
        NDVI %>% addTiles() %>% addRasterImage(
          r,
          colors = palette,
          opacity = 1,
          group = "EVI",
          project = FALSE
        ) %>%
          leaflet::addLegend(
            pal = palette,
            values = c(0, 1),
            bins = 10,
            title = "EVI"
          )  %>%
          leaflet::addLegend(colors = c("#D3D3D3"), labels = "NA/Cloud/Snow") %>%
          addLayersControl(
            overlayGroups = c("EVI"),
            options = layersControlOptions(collapsed = FALSE),
            position = "bottomright"
          ) %>%
          addPolygons(data = NDVI_shp,
                      fill = FALSE,
                      color = " #2E2D3A",
                      weight=1.5, opacity=0.8, options = pathOptions(clickable = FALSE))
      })
    })
  })
  
  # Load timeseries.csv of the seleced region. In case it does not exist, return NULL
  getregionaltimeseries <- reactive({
    filepath <- dbGetQuery(data_db,"SELECT filepath FROM timeseries WHERE catchmentid=?",input$region)$filepath[1]
    req(filepath)
    values <-
      tryCatch(
        read.table(
          file.path(data_root_path,filepath),
          header = TRUE,
          sep = ","
        ),
        warning = function(w) {
          NULL
        },
        error = function(e) {
          NULL
        }
      )
    if (!is.null(values)) {
      regionaldata <- xts(values$value/100, order.by = as.Date(values$date)+15)
    } else {
      regionaldata = NULL
    }
  })
  
  observeEvent(input$archive, {
    click <- isolate(input$NDVI_click)
    clat <- click$lat
    clng <- click$lng
    if (!is.null(clat)) {
      m <- leafletProxy('NDVI') # use the proxy to save computation
      removeShape(map = m, layerId = "circle")
      removeMarker(map = m, layerId = "popup")
      addCircles(
        map = m,
        layerId = 'circle',
        lng = clng,
        lat = clat,
        radius = isolate(input$radius),
        color = "#F00",
        weight = 3,
        options = pathOptions(clickable = FALSE)
      )
      if (!input$timeseriesyes) {
        r <- req(rastermap$r)
        value = mean(unlist(raster::extract(
          x = r,
          y = cbind(clng, clat),
          buffer = input$radius
        )), na.rm = TRUE)
        if (!is.na(value)) {
          addLabelOnlyMarkers(
            map = m,
            layerId = 'popup',
            lng = clng,
            lat = clat,
            label = as.character(round(value, 2)),
            labelOptions = labelOptions(noHide = T, textOnly = FALSE)
          )
        } else {
          dygraph_reset()
        }
      }
    }
  })
  
  # Compute timeseries of within selected Circle. Update Progress Bar. Check for a clicked Stop Button (computetimeseries).
  # httpuv:::service() updates all input variables. Otherwise interruption would not work, but it slows down the computation
  # because this command takes some second. Therefore input values are only updated every 10 loop-round -> if (i %% 10 == 0)
  getpointtimeseries <- function(lat, lng) {
    updateButton(
      session,
      inputId = 'computetimeseries',
      label = 'Stop',
      disabled = FALSE,
      style = 'warning'
    )
    
    withProgress(message = 'Reading Data ... ', {
      continue = input$computetimeseries
      files = filelist()
      files <- files[order(files$date,decreasing=TRUE),]
      values <- vector(mode = "numeric", length = min(nrow(files), input$timeserieslength))  
      dates <- vector(mode = 'character', length = length(values)) 

      for (i in 1:length(values)) {
        filepath <- file.path(data_root_path,files$filepath[i])
        r <- raster(as.character(filepath))/100
        r[r < 0] = NA
        values[i] = mean(unlist(raster::extract(
          x = r,
          y = cbind(lng, lat),
          buffer = input$radius
        )), na.rm = TRUE)
        dates[i] = as.character(as.Date(files$date[i])+15)
        incProgress(1 / length(values))
        if (i %% 3 == 0) {
          httpuv:::service()
        }
        continue2 <- input$computetimeseries
        if (!(continue == continue2)) {
          updateButton(
            session,
            inputId = 'computetimeseries',
            value = FALSE,
            label = 'Start',
            style = 'success'
          )
          break
        }
      }
      data = data.frame(values[1:i], dates[1:i])
      updateButton(
        session,
        inputId = 'computetimeseries',
        value = FALSE,
        label = 'Start',
        style = 'success'
      )
      return(data)
    })
  }
  
  # Click Event on Map. Update Circle and either show Popup with current EVI value or wait for the trigger to compute timeseries
  observeEvent(input$NDVI_click, {
    click <- input$NDVI_click
    clat <- click$lat
    clng <- click$lng
    m <- leafletProxy('NDVI') # use the proxy to save computation
    removeShape(map = m, layerId = "circle")
    removeMarker(map = m, layerId = "popup")
    addCircles(
      map = m,
      layerId = 'circle',
      lng = clng,
      lat = clat,
      radius = input$radius,
      color = "#F00",
      weight = 3,
      options = pathOptions(clickable = FALSE)
    )
    if (!input$timeseriesyes) {
      r <- req(rastermap$r)
      r[r == -1] = NA
      value = mean(unlist(raster::extract(
        x = r,
        y = cbind(clng, clat),
        buffer = input$radius
      )), na.rm = TRUE)
      if (!is.na(value)) {
        addLabelOnlyMarkers(
          map = m,
          layerId = 'popup',
          lng = clng,
          lat = clat,
          label = as.character(round(value, 2)),
          labelOptions = labelOptions(noHide = T, textOnly = FALSE)
        )
      }
    } else {
      dygraph_reset()
    }
    updateButton(session,
                 inputId = 'computetimeseries',
                 disabled = FALSE,
                 style = 'success')
  })
  
  # Render Dygraph when Region is switched. Function is overwritten later on when timeseries are computed
  observeEvent(input$region, {
    dygraph_reset()
  })
  
  observeEvent(input$region, {
    updateButton(session,
                 inputId = 'computetimeseries',
                 disabled = TRUE,
                 style = 'success')
  })
  
  observeEvent(input$timeseriesyes, {
    if (input$timeseriesyes) {
      m <- leafletProxy('NDVI') # use the proxy to save computation
      removeMarker(map = m, layerId = "popup")
      
      output$timeseries <- renderDygraph({
        data <- getregionaltimeseries()
        if (!is.null(data)) {
          dygraph(data) %>%
            dySeries("V1",
                     label = tr("regionalaverage")) %>%
            dyAxis(
              name = 'y',
              valueRange = c(0, 0.8),
              label = tr("enhancedvegetationindex")
            ) %>%
            dyOptions(fillGraph = TRUE,colors = RColorBrewer::brewer.pal(3, colpaletteplots)) %>%
            dyRangeSelector(height = 20)
        }
      })
      
    } else {
      m <- leafletProxy('NDVI') # use the proxy to save computation
      removeMarker(map = m, layerId = "popup")
      removeShape(map = m, layerId = "circle")
    }
  })
  
  # Trigger the computation of a timeseries of the circle selection and render Dygraph.
  observeEvent(input$computetimeseries, {
    if (input$computetimeseries) {
      if (input$timeseriesyes) {
        click <- input$NDVI_click
        clat <- click$lat
        clng <- click$lng
        valuespoint = getpointtimeseries(clat, clng)
        pointdata <-
          xts(valuespoint$values, order.by = as.Date(valuespoint$dates))
        regionaldata <- getregionaltimeseries()
        output$timeseries <- renderDygraph({
          if (input$timeseriesyes == FALSE) {
            click <- input$NDVI_click
            data <- getregionaltimeseries()
            if (!is.null(data)) {
              dygraph(data) %>%
                dySeries("V1", label = tr("enhancedvegetationindex")) %>%
                dyAxis(
                  name = 'y',
                  valueRange = c(0, 0.8),
                  label = tr("enhancedvegetationindex")
                ) %>%
                dyOptions(fillGraph = TRUE,colors = RColorBrewer::brewer.pal(3, colpaletteplots)) %>%
                dyRangeSelector(height = 20)
            }
          } else if (!is.null(regionaldata)) {
            dygraph(cbind(regionaldata, pointdata)) %>%
              dySeries("regionaldata",
                       label = tr("regionalaverage")) %>%
              dySeries("pointdata",
                       label = tr("selection")) %>%
              dyAxis(
                name = 'y',
                valueRange = c(0, 0.8),
                label = tr("enhancedvegetationindex")
              ) %>%
              dyOptions(fillGraph = TRUE,colors = RColorBrewer::brewer.pal(3, colpaletteplots)) %>%
              dyRangeSelector(height = 20,
                              dateWindow = c(min(as.Date(
                                valuespoint$dates
                              )), max(as.Date(
                                valuespoint$dates
                              ))))
          } else if (!is.null(pointdata)) {
            dygraph(pointdata) %>%
              dySeries("V1",
                       label = tr("selection")) %>%
              dyAxis(
                name = 'y',
                valueRange = c(0, 0.8),
                label = tr("enhancedvegetationindex")
              ) %>%
              dyOptions(fillGraph = TRUE,colors = RColorBrewer::brewer.pal(3, colpaletteplots)) %>%
              dyRangeSelector(height = 20,
                              dateWindow = c(min(as.Date(
                                valuespoint$dates
                              )), max(as.Date(
                                valuespoint$dates
                              ))))
          }
        })
      }
    }
  })
  
  #------------- HELP TAB -------------------------------------------------------
  
  
  

  
})
