#ui.r

shinyUI(
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(src="getIP.js"),
      tags$script(src='planningtabledeletebutton.js'),
      tags$script(src='planningtablerowselected.js')
    ),

    titlePanel(windowTitle = "Hebei Heilonggang Irrigation Calculator",
               title =
                 div(
                   img(
                     src = "titlebar_NCP.png",
                     width = 600,
                     style = "margin:0px 0px"
                   )
                 )
    ),
    navbarPage(title=NULL,fluid=TRUE,theme = shinytheme("readable"),
               windowTitle = NULL,collapsible=FALSE,
               tabPanel(fluid=TRUE,title = tags$div(textOutput("backgroundinformation"),
                                                    style = "display: inline-block; vertical-align: top;"), 
                        icon = icon("map-marker"),#----
                        fluidRow(
                          column(6,h4(textOutput("regionalandmodelinformation")),
                                 htmlOutput("backgroundinfo"),
                                 helpText(a("hydrosolutions.ch",     href="http://www.hydrosolutions.ch/",target="_blank"))
                          ),
                          column(6,h4(textOutput("guantaomap")),
                                 tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                 leafletOutput("mymap")
                          )
                        )
               ),
               tabPanel(title = tags$div(textOutput("climate"),style = "display: inline-block; vertical-align: top;"), 
                        value = "climate_tab", icon = icon("cloud", class = NULL, lib = "font-awesome"), #----
                        sidebarPanel(
                          h4(textOutput("select")),
                          uiOutput("climatedata_selector")
                        ),
                        mainPanel(#h4(textOutput("resultsdisplay2")),#
                          uiOutput("plottimeseriesoraverageselector"),
                          conditionalPanel(
                            condition = "input.meteodata2view.includes('9')",
                            uiOutput("pleaseEditTable"),
                            actionButton("recalculateET","Recalculate"),
                            numericInput("stationLongitude", 
                                         uiOutput("stationLongitudeText"), 
                                         value = round((bbox[1]+bbox[2])/2, 
                                                       digits = 2), 
                                         min = bbox[1], max = bbox[2]),
                            numericInput("stationLatitude", 
                                         uiOutput("stationLatitudeText"), 
                                         value = round((bbox[3]+bbox[4])/2, 
                                                       digits = 2), 
                                         min = bbox[3], max = bbox[4]),
                            numericInput("stationAltitude", 
                                         uiOutput("stationAltitudeText"), 
                                         value = 60, min = 0, max = 3000),
                            rHandsontableOutput("editableClimateTable")
                          )
                        )
               ),
               tabPanel(title = tags$div(textOutput("irrigationdemandcalculator"),
                                         style = "display: inline-block; vertical-align: top;"),
                        icon = icon("cog"), #----
                        sidebarPanel(h4(textOutput("selectparameters")),
                                     # Button to click to select different meteo stations
                                     uiOutput("meteo_choice_but2"),  
                                     tags$head(tags$style(HTML('
                                              .modal-lg {
                                              width: 95%;
                                              }
                                            '))),
                                     tags$head(tags$style(HTML('
                                              .modal-sm {
                                              width: 400px;
                                              }
                                            '))),
                                     bsModal("modal_climate", 
                                             textOutput("sourcemeteodata"), 
                                             "meteo_choice_but2", # trigger
                                             size = "small",
                                             uiOutput("meteo_choices2")
                                     ),
                                     HTML("<br>"),
                                     uiOutput("crop2"),
                                     uiOutput("soil2"),
                                     uiOutput('plantingdate2'),
                                     uiOutput('irrigationstrategy2'),
                                     uiOutput('outputtypeselector'),
                                     HTML("<br>"),
                                     h4(textOutput("actions")),
                                     uiOutput("WSbar_Results_show2"),
                                     HTML("<br>"),
                                     uiOutput("addtoirrigationplanner2"),
                                     HTML("<br>"),
                                     uiOutput("downloadCropSoilCoefficients2")
                                     
                        ),
                        mainPanel(
                          uiOutput("barChartormessage"),
                          DT::dataTableOutput('tableResults')
                        )
               ),
               tabPanel(title = tags$div(textOutput("irrigationplanningtable"),style = "display: inline-block; vertical-align: top;"), icon = icon("table", class = NULL, lib = "font-awesome"), #----
                        wellPanel( fluidRow(
                          column(3, uiOutput("loadplanningtablebutton")),
                          column(3, uiOutput("storeplanningtablebutton")),
                          column(3, uiOutput("downloadIrrigationplanningtable2")),
                          column(3, uiOutput("clearplanningtable2"))
                        ) ),
                        HTML("<br>"),
                        uiOutput("nodatainplanningtable"),
                        plotOutput("barChart2"),
                        DT::dataTableOutput("planningtable")
               ),
               tabPanel(title = tags$div(textOutput("livevegetationmap"),style = "display: inline-block; vertical-align: top;"), icon = icon("globe"), #----
                        sidebarPanel(h4(textOutput("mapcontrol")),
                                     uiOutput('jlkbfakbfk'),
                                     uiOutput('archive'),
                                     uiOutput('choosecolormap'),
                                     uiOutput('radiusselector'),
                                     uiOutput('timeseriesyesselector'),
                                     conditionalPanel(condition = "input.timeseriesyes == true",uiOutput("timeseriescontrol"),
                                                      bsButton("computetimeseries",textOutput("start"),type='toggle',disabled=TRUE), 
                                                      style='success')
                        ),
                        mainPanel(
                          leafletOutput("NDVI", height="465px"),
                          uiOutput('NDVIsource'),
                          helpText(textOutput("modissource")),
                          dygraphOutput("timeseries"),
                          plotOutput("NDVItrend"))
               ),
               tabPanel(title = tags$div(textOutput("help2"),
                                         style = "display: inline-block; vertical-align: top;"), 
                        icon = icon("question-circle"), # ----
                        fluidRow(
                          column(8,align="left", h4(textOutput("modelinformation")),
                                 htmlOutput("helpinfo1"),
                                 DT::dataTableOutput("normFactorTable"),
                                 htmlOutput("helpinfo2")
                          ),
                          column(4, h4(textOutput("help")),
                                 HTML("<div style='height: 100px;'>"),
                                 htmlOutput("helpinfo3")
                                 #uiOutput("sender2"),
                                 #uiOutput("subject2"),
                                 #uiOutput("messagebox"),
                                 #HTML("<div style='height: 100px;'>"),
                                 #plotOutput("barcaptcha"),
                                 #HTML("</div>"),
                                 #uiOutput("slidercaptcha2"),
                                 #uiOutput("send2"),
                                 #textOutput("value")
                          )
                        )
               )
    ),
    fluidRow(
      div(style="padding: 0px 25px",
          tags$hr(style="border-color: grey;"),
          uiOutput("languageswitch"))
    )
  )
)
