## ==== SASI Cycling Dashboard ======

## Version: 1.0

## Developer: Keegan Falconar
## Co-Author: Brett Aitken 

## ==================================


## ==== Load packages ===============

library(shiny)
library(tidyverse)
library(bs4Dash)
library(rhandsontable)
library(lubridate)
library(googlesheets4)
library(shinyalert)
library(shinymanager)
library(zoo)
library(plotly)
library(ggalt)
library(DT)
library(eeptools)
library(googledrive)
library(kableExtra)
library(janitor)
library(formattable)
library(cyclingtools)
library(SwimmeR)
library(flexdashboard)
library(reactable)
library(tinytex)
library(investr)


## =================================

## Server credentials ===============

credentials <- data.frame(
  user = 'sasicycling',
  password = 'tokyo2020',
  admin = TRUE,
  comment = "Please enter login details",
  stringsAsFactors = FALSE
)

## ========= Functions ==============

## Define custom color functions ----

# color format cells 

colorFormat <- function(x){
  if(x < -0.2) { 'yellow'} 
  else if (x >= -0.20 & x <= -0.10) {'lawngreen'}
  else if (x > -0.10 & x < 0.10) {'forestgreen'}
  else if (x >= 0.10 & x<= 0.20) {'darkorange'}
  else  {'orangered'} 
  
}

# color format text 

colorTextFormat <- function(x){
  if(x < -0.2) { 'black'} 
  else if (x >= -0.20 & x <= -0.10) {'black'}
  else if (x > -0.10 & x < 0.10) {'white'}
  else if (x >= 0.10 & x<= 0.20) {'white'}
  else  {'white'} 
  
}


## roll sv functions -----

rollSV <- function(x) {
  
  
  firstr <- round(abs((x$Diff[1]*1000)/x$Planned.Schedule[1]),0)
  for (i in 1:nrow(x)){
    firstr <- append(firstr,round(abs(((x$Diff[i+1]-x$Diff[i])*1000)/x$Planned.Schedule[i+1]),0))
  }
  firstr <- as.data.frame(firstr)
  avg <- summarise_all(firstr, mean, na.rm = TRUE)
  
  y <- round(100 - avg,0)
  
  y
  
}

#first km

rollSV1k <- function(x) {
  
  
  firstr <- round(abs((x$Diff[1]*1000)/x$Planned.Schedule[1]),0)
  x <- slice(x,1:4)
  
  for (i in 1:nrow(x)){
    firstr <- append(firstr,round(abs(((x$Diff[i+1]-x$Diff[i])*1000)/x$Planned.Schedule[i+1]),0))
  }
  firstr <- as.data.frame(firstr)
  avg <- summarise_all(firstr, mean, na.rm = TRUE)
  
  y <- round(100 - avg,0)
  
  y
  
}




## Google auth call -----

drive_auth(cache = ".secrets", email = "sasi1.cycling@gmail.com")
gs4_auth(token = drive_token())


url <- gs4_get('https://docs.google.com/spreadsheets/d/1Ql_JmFZrD_qoPanYYs4YFHxB-KmkozLvhwq0_IDYj_c/edit#gid=1964398593')



ui <- dashboardPage(
  
  dark = FALSE,
  
  
    dashboardHeader(
      
      actionButton("refresh",
                   "Refresh Dashboard",
                   status = "secondary",
                   icon = icon("fas fa-sync-alt"))
    ),
    
  ## Sidebar -----------------------------------
  
    dashboardSidebar(
      
      sidebarUserPanel("SASI cycling",image = "logo.jpg"),
      title = "SASI cycling",
        sidebarMenu(menuItem("Data entry", tabName = "entry", icon = icon("fas fa-file-upload")),
                    menuItem("Team pursuit analysis", tabName = "analysis",icon = icon("fas fa-users")),
                    menuItem("IP/TT analysis", tabName = "analysis1", icon = icon("fas fa-biking")),
                    menuItem("Performance monitoring", tabName = "trend", icon = icon("fas fa-chart-line")),
                    menuItem("Power Calculator", tabName = "power", icon = icon("fas fa-calculator")),
                    menuItem("Data entries", tabName = "data", icon = icon("fas fa-server")),
                    menuItem("Athlete list", tabName = "athletes", icon = icon("fas fa-user-cog")))

    ),
    
    dashboardBody(
      useShinyalert(),
      
      ## first tab --------------------------------------------------------------------
      
      tabItems(
        
      
        tabItem(tabName = "entry",
          
         ## TP data entry box ------------------------------------------------------
        fluidRow(
         box(
          fluidRow(inputPanel(
          
        ## Date input
          dateInput(inputId = "date",
                    label = "Enter Date",
                    format = "dd-mm-yyyy"),
          
          
        ## File input
         fileInput("file1", "Choose CSV file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values, text/plain",
                      ".csv")),
        
        numericInput(inputId = "effort",
                    label = "Select training effort (Leave blank if competition)",
                    min = 1,
                    max = 10,
                    value = ""),
        
        selectInput("effortTypeTP",
                    "Effort",
                    choices = c("1km TP Standing", "1.5km TP Standing", "2km TP Standing",
                                "2.5km TP Standing", "3km TP Standing", "4km TP Standing",
                                "4km TP Flying", "1km TP Flying", "1.5km TP Flying", "2km TP Flying",
                                "2.5km TP Flying", "3km TP Flying", "4km TP Flying")))),
        
        
        fluidRow(inputPanel(
        selectInput(inputId = "men_women",
                    label = "Select men or women TP",
                    choices = c("Jnr Men TP", "Jnr Women TP", "Snr Men TP", "Snr Women TP")),
        
        selectInput(inputId = "training_event",
                    label = "Training or Competition",
                    choices = c("Training", "Competition")),
        selectInput(inputId = "qf_f",
                    label = "Select competition race type (if competition)",
                    choices = c("Qualifier", "Final","", "Semi-final"),
                    selected = ""),
        selectInput("compEvent",
                    "Select Event",
                    choices = c("Track Nationals", "State Championships", "Oceania", "World Championships", "Commonwealth Games", 'Olympics',""),
                    selected = ""),
        selectInput("stateTeam",
                    "Select either state or country if competition",
                    choices = c("ACT","NSW","QLD","SA","TAS","VIC","WA","Australia","Canada","Denmark","France","Great Britain","Germany",
                                "Italy","New Zealand","Switzerland","USA",""),
                    selected = "")
        
        )),
        
        
        
        fluidRow(
          inputPanel(sliderInput(inputId = "splits",
                      label = "Select number of splits for session",
                      min = 1,
                      max = 17,
                      value = 17),
                     checkboxInput("flyingtp",
                                   "Tick box if flying effort",
                                   value = FALSE),
                     selectInput("tpTrial",
                                  "Select trial if trial (leave blank if not)",
                                  choices = c("Trial",""),
                                 selected = ""), 
                     textAreaInput("notesTP",
                                   "Input notes here:",
                                   value = "")
                     )),
         
         ## editable table 
         rHandsontableOutput("table1"),
        
        br(),
        
        ## save data to googlesheets
        
        actionButton(inputId = "save",
                     label = "Save data",
                     status = "success"),
         
         
        ## box formatting
        closable = FALSE, solidHeader = TRUE, width = 12, title = "Team Pursuit track split data entry", status = "navy"),
        
        
        ## IP data entry box -------------------------
        
        box(
          fluidRow(
          inputPanel(
            dateInput("date6",
                      "Enter date",
                      format = "dd-mm-yyyy"),
            
            selectInput("athlete6",
                        "Select Athlete",
                        choices = "",
                        selected = ""),
            numericInput(inputId = "effort1",
                         label = "Select training effort (Leave blank if competition)",
                         min = 1,
                         max = 10,
                         value = ""),
            selectInput("effortTypeIP",
                        "Effort",
                        choices = c("250m TT Standing","500m TT Standing", "750m TT Standing","1km TT Standing", "1km IP Standing",
                                    "1.5km IP Standing", "2km IP Standing", "2.5km IP Standing", "3km IP Standing",
                                    "4km IP Standing","2km IP Flying", "3km IP Flying", "4km IP Flying", "500 TT Flying", "1km TT Flying", "250m TT Flying",
                                    "500m TT Flying", "750m TT Flying","1km TT Flying", "1km IP Flying",
                                    "1.5km IP Flying", "2km IP Flying", "2.5km IP Flying", "3km IP Flying","4km IP Flying")))),
          
        fluidRow(
          inputPanel(
            fileInput("file2", "Choose CSV file",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values, text/plain",
                        ".csv")),
                      
            selectInput("training_comp",
                        "Training or Competition",
                        choices = c("Training","Competition"),
                        selected = ""),
            selectInput(inputId = "qf_f1",
                        label = "Select competition race type (if competition)",
                        choices = c("Qualifier", "Final","", 'Semi-final'),
                        selected = ""),
            selectInput("compEvent2",
                        "Select Event",
                        choices = c("Track Nationals", "State Championships", "Oceania", "World Championships", "Commonwealth Games", 'Olympics',""),
                        selected = ""))),
        
        fluidRow(
          inputPanel(sliderInput("ipslide",
                    "Select number of splits",
                    min = 1,
                    max = 17,
                    value = 17),
          checkboxInput("flyingip",
                        "Tick box if flying effort",
                        value = FALSE),
          selectInput("ipTrial",
                      "Select trial if trial (leave blank if not)",
                      choices = c("Trial",""),
                      selected = ""),  
          textAreaInput("notesIP",
                        "Input notes here:",
                        value = ""))),
     
        rHandsontableOutput("table3"),
        
        actionButton("save2",
                     "Save Data",
                     status = "success"),
        
          
   ## box formatting
    closable = FALSE, solidHeader = TRUE, width = 12, title = "IP track split data entry", status = "navy")),
        
        
        ### 120/60 entry -------------------------
        
        fluidRow(
        
        box(fluidRow(
          dateInput(inputId = "date4",
                               label = "Enter date",
                    format = "dd-mm-yyyy")),
            
            fluidRow(
              selectInput(inputId = "athlete4",
                          label = "Select athlete",
                          choices = "",
                          selected = "",
                          width = "200px")),
            
            fluidRow(
              numericInput(inputId = "power1",
                                  label = "Enter power (Watts)",
                                  value = "")),
            
            fluidRow(
              numericInput(inputId = "hr",
                                  label = "Enter heart rate (bpm)",
                                  value = "")),
            fluidRow(
            actionButton("submit2",
                         "Upload",
                         status = "success")
              ),
          
          
          # box formatting
          closable = FALSE, solidHeader = TRUE, width = 6, title = "120/60's data", status = "secondary"),
        
        
        ## power profile entry -----------------------
        
        box(fluidRow(
          dateInput("date5",
                    "Enter date",
                    format = "dd-mm-yyyy")),
          
          fluidRow(
            selectInput("athlete5",
                        "Select athlete",
                        choices = "",
                        selected = "",
                        width = "200px")),
        
            fluidRow(
              numericInput("duration",
                           "Enter duration (seconds)",
                           value = "")),
          
          fluidRow(
            numericInput("power2",
                         "Enter best average power (Watts)",
                         value = "")),
          fluidRow(
            numericInput("weight",
                         "Enter Weight (kg)",
                         value = "")),
          fluidRow(
            numericInput("cda",
                         "Enter CdA",
                         value = "",
                         )),
          fluidRow(
            selectInput("bike",
                         "Select bike used",
                         choices = c("Wattbike","SRM", "Track bike", "Road bike"),
                         selected = "")),
    
          fluidRow(
            actionButton("submit3",
                         "Upload",
                         status = "success")
          ),
          
          closable = FALSE, solidHeader = TRUE, width = 6, title = "Power Profile data", status = "secondary"))
        
     ),
      
        
       
       
       ## TP analysis tab -------------------------------------------------------
       
       tabItem(tabName = "analysis",
              
          tabsetPanel(
      
      
      ## training tab 
      tabPanel("Training",
      fluidPage(      
        fluidRow(box(inputPanel(
          dateInput(inputId = "date2",
                    label = "Select Date",
                    format = "dd-mm-yyyy"),
          
          selectInput(inputId = "men_women_plotTP_t",
                      label = "Select Women or Men TP",
                      choices = c("Jnr Men TP", "Jnr Women TP", "Snr Men TP", "Snr Women TP")),
          numericInput("effort7",
                       "Training effort",
                       min = 1,
                       max = 10,
                       value = 1),
          downloadButton(outputId = "tpTrainreport", "Generate pdf report")
          ), width = 12, collapsible = F,headerBorder = FALSE)),
        
        fluidRow(
          box(plotlyOutput("plot1"), width = 9, collapsible = F, title = "Acutal vs Planned lap split times", status = "primary"),
          box(plotlyOutput('svTpTrain'), width = 3, collapsible = F, title = "Smoothness Variability %")),
        
        fluidRow(column(9,
          box(reactableOutput("table2"), width = NULL, collapsible = F,headerBorder = FALSE)),
          column(3,
          box(reactableOutput("tpTimesKmTrain"), width = NULL, collapsible = F,headerBorder = FALSE),
          box(textOutput("pasteNotesTpTrain"), width = NULL, title = "Notes"))
          ))),
               

      ## Competition tab
      
      tabPanel("Competition",
            fluidPage(
              fluidRow(box(
                inputPanel(selectInput(inputId = "men_women_plotTP_comp",
                                   label = "Select Women or Men TP",
                                   choices = c("Jnr Men TP", "Jnr Women TP", "Snr Men TP", "Snr Women TP")),
                       selectInput("tpCompEventFilter",
                                   "Select Event",
                                   choices = "",
                                   selected = ""),
                       selectInput("ql_f4",
                                   "Qualifier or Final",
                                   choices = c("Qualifier", "Final", "Semi-final")),
                       selectInput("stateTeam1",
                                   "Select either state or country if competition",
                                   choices = c("ACT","NSW","QLD","SA","TAS","VIC","WA","Australia","Canada","Denmark","France","Great Britain","Germany",
                                               "Italy","New Zealand","Switzerland","USA"),
                                   selected = "SA")), collapsible = F, width = 12,headerBorder = FALSE)),
                
                fluidRow(
                  box(
                    plotlyOutput("plot14"),
                     title = "Acutal vs Planned lap split times", status = "primary", width = 9, collapsible = F),
                   box(
                     plotlyOutput("svFull"), 
                     width = 3, collapsible = F, title = "Smoothness Variability %")),
        
              fluidRow(column(width = 9,
                box(reactableOutput("table20"),width = NULL, collapsible = F,headerBorder = FALSE)),
                column(width = 3,
                  box(reactableOutput("tpTimesKmComp"), collapsible = F,width = NULL,headerBorder = FALSE),
                  box(textOutput("pasteNotesTpComp"), title = 'Notes', collapsible = F,width = NULL))),
              
            )),   
         
      tabPanel("Compare analysis",
               box(
                 fluidRow(column(width = 3,
                          inputPanel(
                            selectInput("tpMenWomen",
                                        "Select TP group",
                                        choices = c("Jnr Men TP", "Jnr Women TP", "Snr Men TP", "Snr Women TP")),
                            selectInput("tpCompareEffortType",
                                        "Select Effort Type",
                                        selected = "",
                                        choices = ""),
                            dateInput("tpAnalyseDate",
                                      "Select Date",
                                      format = "dd-mm-yyyy"),
                            radioButtons("TPtrainCompFilter",
                                         "Select training or competition",
                                         choices = c("Training", "Competition"),
                                         selected = "Training",
                                         inline = F),
                            numericInput("TrainTpCompareEffort",
                                         "Effort number",
                                         value = "",
                                         min = 1,
                                         max = 10),
                            selectInput("CompTpCompare",
                                        "Select Final or Qualifier",
                                        choices = c("Final", "Qualifier", "Semi-final"),
                                        selected = ""),
                            selectInput("teamCompTP",
                                        "Select either state or country",
                                        choices = c("ACT","NSW","QLD","SA","TAS","VIC","WA","Australia","Canada","Denmark","France","Great Britain","Germany",
                                                    "Italy","New Zealand","Switzerland","USA",""),
                                        selected = "")),
                          hr(),
                          inputPanel(
                            selectInput("tpMenWomen1",
                                        "Select TP group",
                                        choices = c("Jnr Men TP", "Jnr Women TP", "Snr Men TP", "Snr Women TP")),
                            selectInput("tpCompareEffortType1",
                                        "Select Effort Type",
                                        selected = "",
                                        choices = ""),
                            dateInput("tpAnalyseDate1",
                                      "Select Date",
                                      format = "dd-mm-yyyy"),
                            radioButtons("TPtrainCompFilter1",
                                         "Select training or competition",
                                         choices = c("Training", "Competition"),
                                         selected = "Training",
                                         inline = F),
                            numericInput("TrainTpCompareEffort1",
                                         "Effort number",
                                         value = "",
                                         min = 1,
                                         max = 10),
                            selectInput("CompTpCompare1",
                                        "Select Final or Qualifier",
                                        choices = c("Final", "Qualifier", "Semi-final"),
                                        selected = ""),
                            selectInput("teamCompTP1",
                                        "Select either state or country",
                                        choices = c("ACT","NSW","QLD","SA","TAS","VIC","WA","Australia","Canada","Denmark","France","Great Britain","Germany",
                                                    "Italy","New Zealand","Switzerland","USA",""),
                                        selected = ""))),
                          column(width = 9, plotlyOutput("tpPlotAnalyses", width = "100%"),
                                 hr(),
                                 tableOutput("tpanalysesTable"),
                                 tableOutput("tpCompareKiloPlot"))),
                 closable = FALSE, width = 12, headerBorder = FALSE, solidHeader = FALSE, collapsible = FALSE)
        
        
      ))),
        
      ## IP Analysis tab ----------------------------------------
        
        tabItem(tabName = "analysis1",
                
          tabsetPanel(
            
            tabPanel("Training",
            fluidRow(inputPanel(dateInput("date7",
                               "Select date",
                               format = "dd-mm-yyyy"),
                     
                     selectInput("athlete3",
                                 "Select athlete",
                                 choices = "",
                                 selected = ""),
                     numericInput("effort6",
                                  "Select training effort",
                                  min = 1,
                                  max = 10,
                                  value = 1))),
                
                box(
      
                  fluidRow(plotlyOutput("plot2")),
                  hr(),
                  
                  fluidRow(column(width = 7,
                                  tableOutput("table9")),
                           column(width = 4, offset = 1, 
                                  tableOutput("ipTimesKmTrain"))),
                  hr(),
                           textOutput("pasteNotesIpTrain"),
                    closable = FALSE, solidHeader = TRUE, width = 12, title = "Actual vs planned Track splits", status = "primary")
                
                ),
  
            tabPanel("Competition",
                     fluidRow(
                       inputPanel(
                                  selectInput("athlete9",
                                               "Select athlete",
                                               choices = "",
                                              selected = ""),
                                         selectInput("ipCompEventFilter",
                                                     "Select Event",
                                                      choices = ""),
                                         selectInput("ql_f",
                                                     "Qualifier or final",
                                                     choices = c("Qualifier", "Final", "Semi-Final")))),
                                         
                     
                     box(
                       fluidRow(plotlyOutput("plot7")),
                       hr(),
                       
                       fluidRow(tableOutput("table10")),
                       fluidRow(tableOutput("ipTimesKmComp")),
                       hr(),
                               textOutput("pasteNotesIpComp"),
                       closable = FALSE, solidHeader = TRUE, width = 12, title = "Actual vs planned Track splits", status = "primary")

                     ),
            
            
            
            tabPanel("Compare analyses",
                    box(
                      fluidRow(column(width = 3,
                        inputPanel(
                          selectInput("eventCompare",
                                      "Select Effort",
                                      choices = "",
                                      selected = ""),
                          selectInput("athleteCompareIp",
                                      "Select Athlete 1",
                                      choices = "",
                                      selected = ""),
                          dateInput("IpCompareDate",
                                    "Select Date",
                                    format = "dd-mm-yyyy"),
                          radioButtons("trainCompFilter",
                                       "Select training or competition",
                                       choices = c("Training", "Competition"),
                                       selected = "Training",
                                       inline = F),
                          numericInput("TrainIpCompareEffort",
                                       "Effort number",
                                       value = "",
                                       min = 1,
                                       max = 10),
                          selectInput("CompIpCompare",
                                      "Select Final or Qualifier",
                                      choices = c("Final", "Qualifier", "Semi-final"),
                                      selected = "")),
                        hr(),
                        
                               inputPanel(
                                 selectInput("eventCompare1",
                                             "Select Effort",
                                             choices = "",
                                             selected = ""),
                                 dateInput("IpCompareDate2",
                                           "Select Date",
                                           format = "dd-mm-yyyy"),
                                 
                                 selectInput("athleteCompareIp2",
                                             "Select Athlete 2",
                                             choices = "",
                                             selected = ""),
                                 radioButtons("trainCompFilter2",
                                              "Select training or competition",
                                              choices = c("Training", "Competition"),
                                              selected = "Training",
                                              inline = F),
                                 numericInput("TrainIpCompareEffort2",
                                              "Effort number",
                                              value = "",
                                              min = 1,
                                              max = 10),
                                 selectInput("CompIpCompare2",
                                             "Select Final or Qualifier",
                                             choices = c("Final", "Qualifier", "Semi-final"),
                                             selected = ""))),
                        
                        column(width = 9, plotlyOutput("ipComparePlot", width = "100%"),
                               hr(),
                               tableOutput('analysesTable'),
                               tableOutput("ipCompareKiloPlot"))),
                      
                        
                      closable = FALSE, width = 12, headerBorder = FALSE, solidHeader = FALSE, collapsible = FALSE)
                    )
                )
            ), 
          
   
   ### power trend tab -----------------------------
   
   tabItem(tabName = "trend",
           
           selectInput("athlete10",
                       label = "Select Athlete",
                       choices = "",
                       selected = ""),
           
           hr(),
           
           box(
           fluidRow(column(width = 6,
                           dataTableOutput("four_min")),
                    column(width = 6,
                           fluidRow(
                             inputPanel(selectInput("person1",
                                                    "Athlete 1",
                                                    choices = "",
                                                    selected = ""),
                                        selectInput("person2",
                                                    "Athlete 2",
                                                    choices = "",
                                                    selected = ""),
                                        selectInput("person3",
                                                    "Athlete 3",
                                                    choices = "",
                                                    selected = ""))), 
                   fluidRow(plotlyOutput("maxpp")))),
           
           hr(),
       
           
          
           fluidRow(inputPanel(selectInput("durationFilter",
                                  "Select Duration",
                                  choices = c(5,6,30,60,120,180,240,600,1200,3600),
                                  selected = 240),
                      selectInput("athletecomp",
                                  "Select athlete to compare",
                                  choices = "",
                                  selected = ""),
                      selectInput("powerFilter",
                                  "Choose Power, w/kg, w/CdA",
                                  choices = c("Power", "w_kg","w_CdA"),
                                  selected = "Absolute")
           )),
           plotlyOutput("plotCompare"),
           
           ## box formatting
           closable = FALSE, solidHeader = TRUE, title = "Power Profile", width = 12, status = "gray-dark"),
         
         
         ## comparison chart 
         
         box(
           ## 120/60's plot
           plotlyOutput("sixtiesPlot", height = "300px"),
           plotlyOutput("sixtiesPlot1", height = "280px"),
           
           
           ## box formatting
           closable = FALSE, solidHeader = TRUE, title = "120/60 monitoring", width = 12, status = "warning")
     
   ),
      
      
      
      
   ### Athlete entry tab --------------------
      tabItem(tabName = "athletes",
              
              fluidRow(column(width = 6,
                              
              dataTableOutput("athleteTable")),
              
              column(width = 6,
                    
                     textInput("name",
                               "Name",
                               ""),
                     
                     dateInput("D.O.B",
                               "Date of Birth",
                               format = "dd-mm-yyyy"),
                     numericInput("bCDA",
                                  "Baseline CDA",
                                  value = ""),
                     radioButtons("Sex",
                                  "Sex",
                                  choices = c("Male", "Female"),
                                  inline = TRUE),
                     
                     actionButton('submit',
                                  "Submit",
                                  status = "warning")
                     
                     )
                  )
              
      
              ),
   
   ### View raw data tab -----
   
   tabItem(tabName = "data",
           
           tabsetPanel(
             
             ## TP tab -----
             tabPanel("TP",
                  
                fluidRow(
                  inputPanel(
                    dateRangeInput("date11",
                            "Select Date",
                            format = "dd-mm-yyyy",
                            start = Sys.Date()-365,
                            end = Sys.Date()),
                    selectInput("tpGroup",
                                "Select TP group",
                                choices = "",
                                selected = ""),
                    selectInput("effort99",
                                "Effort Type",
                                choices =  c("1km TP Standing", "1.5km TP Standing", "2km TP Standing",
                                             "2.5km TP Standing", "3km TP Standing", "4km TP Standing",
                                             "4km TP Flying", "1km TP Flying", "1.5km TP Flying", "2km TP Flying",
                                             "2.5km TP Flying", "3km TP Flying", "4km TP Flying",""),
                                selected = ""),
                    
                    selectInput("tpCompEventF2",
                                "Select Event",
                                choices = c("Competition","Training"),
                                selected = "Training"),
                    checkboxInput("trialRawFilterTp",
                                  "Tick box to filter by Trial",
                                  value = FALSE))),
                rHandsontableOutput("rawData")),
             
             ## IP Tab ----
          
             tabPanel("IP",
                      
                 fluidRow(
                   inputPanel(
                    dateRangeInput("date19",
                             "Select Date",
                              format = "dd-mm-yyyy",
                              start = Sys.Date()-365,
                              end= Sys.Date()),
                    selectInput("athlete99",
                                "Athlete",
                                choices = "",
                                selected = ""),
                    selectInput("effort70",
                                "Effort Type",
                                choices = c("250m TT Standing","500m TT Standing", "750m TT Standing","1km TT Standing", "1km IP Standing",
                                            "1.5km IP Standing", "2km IP Standing", "2.5km IP Standing", "3km IP Standing",
                                            "4km IP Standing","2km IP Flying", "3km IP Flying", "4km IP Flying", "500 TT Flying", "1km TT Flying", "250m TT Flying",
                                            "500m TT Flying", "750m TT Flying","1km TT Flying", "1km IP Flying",
                                            "1.5km IP Flying", "2km IP Flying", "2.5km IP Flying", "3km IP Flying","4km IP Flying",""),
                                            selected = ""),
                    selectInput("ipCompEventF2",
                                "Competition or Training",
                                choices = c('Competition', "Training"),
                                selected = "Training"),
                    checkboxInput("trialRawFilterIp",
                                  "Tick box to filter by Trial",
                                  value = FALSE))),
                      rHandsontableOutput("rawData1")),
             
             ## PP tab ----
             
             tabPanel("Power Profile",
                      fluidRow(
                        inputPanel(
                          dateRangeInput("date45",
                                         "Select Date",
                                         format = "dd-mm-yyyy",
                                         start = Sys.Date()-1095,
                                         end= Sys.Date()),
                          selectInput("athlete85",
                                      "Athlete",
                                      choices = "",
                                      selected = "")
                        )
                      ),
                      hr(),
                      rHandsontableOutput("ppRawData")
                      
                      
                      ),
             
             ## 120/60 tab ----
             
             tabPanel("120/60",
                      fluidRow(
                        inputPanel(
                          dateRangeInput("date46",
                                         "Select Date",
                                         format = "dd-mm-yyyy",
                                         start = Sys.Date()-365,
                                         end= Sys.Date()),
                          selectInput("athlete86",
                                      "Athlete",
                                      choices = "",
                                      selected = "")
                        )
                      ),
                      hr(),
                      rHandsontableOutput("onetwentyRawData")
                  )
             
             )
           
           ),
   ## Power calculator tab ------
   
   tabItem(tabName = "power",
          fluidRow(box(inputPanel(
                          selectInput("athletePred",
                                          "Select Athlete",
                                          choices = "",
                                          selected = ""),
                              sliderInput("cda",
                                          "Select cda",
                                          min = 0.15,
                                          max = 0.30,
                                          step = 0.005,
                                          value = 0.20),
                              numericInput("airDen",
                                           "Air Density",
                                           value = 1.17),
                              numericInput("Fw",
                                           "Weight (Rider + Bike)",
                                           value = 79),
                              sliderInput("coef",
                                          "Select Time Coeffient",
                                          min = 1.0100,
                                          max = 1.0350,
                                          step = 0.0005,
                                          value = 1.0200),
                              numericInput("halfLapTar",
                                           "Half Lap Target",
                                           value = 18.30),
                              sliderInput("startPwr",
                                          "Starting Power",
                                          min = 500,
                                          max = 1500,
                                          step = 10,
                                          value = 690),
                              numericInput("lap",
                                           "Lap time",
                                           value = 16.5),
                              sliderInput("crrF",
                                          "Power correction (%)",
                                          min = .85,
                                          max = 1.0,
                                          step = .01,
                                          value = 1.0),
                          dateRangeInput("calDate",
                                         "Select date range for power curve",
                                         start = Sys.Date()-150,
                                         end = Sys.Date(),
                                         format = "dd-mm-yyyy")),
                       #inputPanel(p(strong("Critcal power settings")),
                        #          radioButtons("cpModel",
                         #                      "Select CP model",
                          #                     choices = c("linear","3-hyp","2-hyp","1/time"),
                           #                    selected = "linear",
                            #                   inline = TRUE),
                             #     selectInput("calDura",
                              #                "Select race distance",
                               #               choices = c(4000,3000,2000,1000),
                                          #  selected = 4000),
                       closable = FALSE, collapsible = TRUE, width = 12)),
    fluidRow(box(plotlyOutput("calcPlot"), width = 12,maximizable = TRUE, closable = FALSE, collapsible = FALSE)),

    fluidRow(box(reactableOutput("testdf"), closable = FALSE, collapsible = FALSE, width = 12))
                         
                  
           )
 
          )
               
        )
       
      )
     
      
## wrap ui in secure function 

ui <- secure_app(ui, background = "linear-gradient(rgba(0, 0, 255, 0.5), rgba(255, 255, 0, 0.5))")


### server -------------------------------------------------------------

server <- function(input, output, session) {
  
## shiny manager code -------  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  

  ## Save new athlete to GS --------------
  
  observeEvent(input$submit,{
    
    x <- data.frame(Name = input$name,
              D.O.B = input$D.O.B,
              Sex = input$Sex,
              bCDA = input$bCDA)
    
    sheet_append(url, x, sheet = 5)
    showNotification("Athlete information uploaded", duration = 4, closeButton = TRUE, type = "message")
    
  })
  
  ## dashboard refresh shiny alert -----------
  
  observeEvent(input$refresh,{
    shinyalert(text = "Data refresh complete", closeOnClickOutside = TRUE, type = "success", timer = 2000)
  })
  
  
  ## Athlete list loaded from GS ---------
  
 athlete_list <- reactive({
    input$refresh
    x <- read_sheet(url,sheet = 5)
    x <- as.data.frame(x)
    x$D.O.B <- as.Date(x$D.O.B)
    
    x <- arrange(x, Name)
    
  })
  
 

  ## Dynamic athlete list selection 
  
  ## 120/60's athlete filter ------
  
 observe({
   
    x <- athlete_list()$Name
    
    if(is.null(x))
         x <- character(0)
    
   updateSelectInput(session,
                    inputId = "athlete4",
                     choices = x,
                     selected = head(x,1))
  })
 
 ## Athlete Filter for IP Compare drop downs ------
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athleteCompareIp",
                     choices = x,
                     selected = head(x,1))
 })
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athleteCompareIp2",
                     choices = x,
                     selected = head(x,1))
 })
 
 ## critical power athlete list updater ------
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "cpAthleteFilter",
                     choices = x,
                     selected = head(x,1))
 })
 
## update athlete list in power calculator tab -----
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = 'athletePred',
                     choices = x,
                     selected = head(x,1))
 })
 
 
## athlete comparison power profile filter ----
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athletecomp",
                     choices = x,
                     selected = head(x,1))
 })
 ## raw pp athlete filter  ----
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athlete85",
                     choices = x,
                     selected = head(x,1))
 })
 
 ## Raw data TP group filter update ------
 
 observe({
   
   x <- tp_Data()$Men.Women
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "tpGroup",
                     choices = x,
                     selected = head(x,1))
 })
 
 
 ## raw 120/60 athlete filer -----
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athlete86",
                     choices = x,
                     selected = head(x,1))
 })
 
 
 
 
  ## power athlete filter profile ----
  
  observe({
    
    x <- athlete_list()$Name 
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "athlete5",
                      choices = x,
                      selected = head(x,1))
    
  })
 
 
 ## Athlete lists for inputs max pp plot filters -----
 
 observe({
   
   x <- athlete_list()$Name 
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "person1",
                     choices = x,
                     selected = "")
   
 })
 
 observe({
   
   x <- athlete_list()$Name 
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "person2",
                     choices = x,
                     selected = "")
   
 })
 
 observe({
   
   x <- athlete_list()$Name 
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "person3",
                     choices = x,
                     selected = "")
   
 })
 
 
 ## all data view IP athlete list updater ------
 
 observe({
   
   x <- athlete_list()$Name 
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athlete99",
                     choices = x,
                     selected = head(x,1))
   
 })

  ## IP track split entry form ----
  
  observe({
    
    x <- athlete_list()$Name
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "athlete6",
                      choices = x,
                      selected = head(x,1))
  })
  
  
  
  ## IP training Athlete filter -----
  
  observe({
    
    x <- athlete_list()$Name
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "athlete3",
                      choices = x,
                      selected = head(x,1))
  })
  
  ## IP competition athlete filter ----
 
 observe({
   
   x <- athlete_list()$Name
   
   if(is.null(x))
     x <- character(0)
   
   updateSelectInput(session,
                     inputId = "athlete9",
                     choices = x,
                     selected = head(x,1))
 })
 
 
 
  
  ## Performance monitoring athlete filter ----
  
  observe({
    
    x <- athlete_list()$Name
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "athlete10",
                      choices = x,
                      selected = head(x,1))
  })
  
  
  ## 120/60 data set -------------------
  
  onetwentysix <- reactive({
    x <- read_sheet(url,sheet = 3)
    
  })
  
  
  ## Athlete list table view ----------
  
  output$athleteTable <- renderDataTable({
    
    x <- athlete_list()
    
    x$D.O.B <- as.character(x$D.O.B)
   
    datatable(x, options = list("pageLength" = 40))
   
  
  })
  
  
  ## upload 120/60's to GS -------------
  
  observeEvent(input$submit2,{
    x <- tibble(Date = input$date4,
                Athlete = input$athlete4,
                Power = input$power1,
                HR = input$hr)
    
    dob <- athlete_list() %>% filter(Name %in% input$athlete4) %>% select(D.O.B)
    
    x <- x %>% mutate(Age = round(age_calc(dob = dob$D.O.B, enddate = Date , units = 'years'),2))
    

    sheet_append(url, x, sheet = 3)
    showNotification("120/60 data uploaded", duration = 4, closeButton = TRUE, type = "message")
  })
  
  ## upload best power effort to GS -----
  
  observeEvent(input$submit3,{
    x <- tibble(Date = input$date5,
                Athlete = input$athlete5,
                Duration = input$duration,
                Power = input$power2,
                Weight = input$weight,
                Bike = input$bike,
                CdA = input$cda)
    
    dob <- athlete_list() %>% filter(Name %in% input$athlete5) %>% select(D.O.B)
    
    x <- x %>% mutate(Age = round(age_calc(dob = dob$D.O.B, enddate = Date , units = 'years'),1))

    sheet_append(url, x, sheet = 4)
    
    showNotification("Power data uploaded", duration = 4, closeButton = TRUE, type = "message")
  })
  
  
  

  ## TP data frame ----
  
  output$table1 <- renderRHandsontable({
    if (is.null(input$file1)){
      return(NULL) } else {
    rhandsontable(data()) }
  })
  
  ## IP data frame ----
  
  output$table3 <- renderRHandsontable({
    
    if (is.null(input$file2)) {
      
      return(NULL) } else {
    rhandsontable(data_ip()) }
  })
  
  ## updated IP table data frame - upload to google sheets ----
   observeEvent(input$save2, {
    x <- hot_to_r(input$table3)
    sheet_append(url, x, sheet = 2)
    showNotification("IP data uploaded", duration = 4, closeButton = TRUE, type = "message")
    
  })
    
  
  ## updated TP table data frame - upload to google sheets ----
    observeEvent(input$save, {
    x <- hot_to_r(input$table1)
    sheet_append(url, x, sheet = 1)
    showNotification("TP data uploaded", duration = 4, closeButton = TRUE, type = "message")
  })
  

  ## loaded csv file for TP -----
  
    data <- reactive({
      
      if (is.null(input$file1)) 
        
        return(NULL) 
      
      x <- read.csv(input$file1$datapath)
      
      y <- data.frame(lap.splits = if (input$flyingtp == FALSE) { c(0.25,0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75,12.75,13.75,14.75,15.75)
        } else {c(0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75,12.75,13.75,14.75,15.75,16.75)})%>%slice(1:input$splits) 
      
    
      
      x <- x %>% mutate(Date = as.Date(input$date),
                        Athlete = "",
                        Men.Women = input$men_women,
                        Session = input$training_event,
                        Comp.type = input$qf_f,
                        Training.effort = input$effort) %>% slice(1:input$splits)
      xy <- cbind(x,y)
      
      xy <- xy %>% mutate(Effort.type = input$effortTypeTP,
                          Notes = input$notesTP,
                          Event = input$compEvent,
                          Team = input$stateTeam,
                          Trial = input$tpTrial)
      
      xy
    
  })
    
    ## Second database - total times TP training ----
    
    data_times_tp_training <- reactive({
      
      t <- tp_Data() %>%
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)
        
      x <- data.frame(total.times.planned = rollapplyr(t$Planned.Schedule, nrow(t) ,sum, partial = TRUE),
                      total.times.actual = rollapplyr(t$Actual.Schedule, nrow(t) ,sum, partial = TRUE))

      
      y <- tibble(Distance.km = c("1", "2", "3", "4"),
                  Planned.Time = if (t[1,9] == 0.75) { c(x[4,1] + (t[4,1]/4), x[8,1] + (t[8,1]/4), x[12,1] + (t[12,1]/4), x[16,1] + (t[16,1]/4))} else {
                    
                    c(x[5,1] + (t[5,1]/4), x[9,1] + (t[9,1]/4), x[13,1] + (t[13,1]/4), x[17,1] + (t[17,1]/4))},
                  
                  Actual.Time = if (t[1,9] == 0.75) { c(x[4,2] + (t[4,2]/4), x[8,2] + (t[8,2]/4), x[12,2] + (t[12,2]/4), x[16,2] + (t[16,2]/4))} else {
                    
                    c(x[5,2] + (t[5,2]/4), x[9,2] + (t[9,2]/4), x[13,2] + (t[13,2]/4), x[17,2] + (t[17,2]/4))})
      
      
      ## Acutal times
      one <- as.numeric(y$Actual.Time[1])
      two <- as.numeric(y$Actual.Time[2])
      three <- as.numeric(y$Actual.Time[3])
      four <- as.numeric(y$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      
      ##Planned times
      
      oneP <- as.numeric(y$Planned.Time[1])
      twoP <- as.numeric(y$Planned.Time[2])
      threeP <- as.numeric(y$Planned.Time[3])
      fourP <- as.numeric(y$Planned.Time[4])
      twokP <- twoP - oneP
      threekP <- threeP - twoP
      fourkP <- fourP - threeP
      totalP <- sum(oneP,twokP,threekP,fourkP,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              'Actual Times' = c(mmss_format(one), mmss_format(two - one), 
                                                 mmss_format(three - two), mmss_format(four - three), 
                                                 mmss_format(total)),
                              'Planned Times'= c(mmss_format(oneP), mmss_format(twoP - oneP), 
                                                 mmss_format(threeP - twoP), mmss_format(fourP - threeP), 
                                                 mmss_format(totalP)))
      
      km_splits
     
      
    })
    
    ## Second database - total times TP Competition ---- 
    
    data_times_tp_competition <- reactive({
      
      t <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event,year(Date), sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4 & eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1) 
      
      x <- data.frame(total.times.planned = rollapplyr(t$Planned.Schedule, nrow(t) ,sum, partial = TRUE),
                      total.times.actual = rollapplyr(t$Actual.Schedule, nrow(t) ,sum, partial = TRUE))
      
      
      y <- tibble(Distance.km = c("1", "2", "3", "4"),
                  Planned.Time = if (t[1,9] == 0.75) { c(x[4,1] + (t[4,1]/4), x[8,1] + (t[8,1]/4), x[12,1] + (t[12,1]/4), x[16,1] + (t[16,1]/4))} else {
                    
                    c(x[5,1] + (t[5,1]/4), x[9,1] + (t[9,1]/4), x[13,1] + (t[13,1]/4), x[17,1] + (t[17,1]/4))},
                  
                  Actual.Time = if (t[1,9] == 0.75) { c(x[4,2] + (t[4,2]/4), x[8,2] + (t[8,2]/4), x[12,2] + (t[12,2]/4), x[16,2] + (t[16,2]/4))} else {
                    
                    c(x[5,2] + (t[5,2]/4), x[9,2] + (t[9,2]/4), x[13,2] + (t[13,2]/4), x[17,2] + (t[17,2]/4))})
      
      ## Acutal times
      one <- as.numeric(y$Actual.Time[1])
      two <- as.numeric(y$Actual.Time[2])
      three <- as.numeric(y$Actual.Time[3])
      four <- as.numeric(y$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      
      ##Planned times
      
      oneP <- as.numeric(y$Planned.Time[1])
      twoP <- as.numeric(y$Planned.Time[2])
      threeP <- as.numeric(y$Planned.Time[3])
      fourP <- as.numeric(y$Planned.Time[4])
      twokP <- twoP - oneP
      threekP <- threeP - twoP
      fourkP <- fourP - threeP
      totalP <- sum(oneP,twokP,threekP,fourkP,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              Actual = c(mmss_format(one), mmss_format(two - one), 
                                                 mmss_format(three - two), mmss_format(four - three), 
                                                 mmss_format(total)),
                              Planned = c(mmss_format(oneP), mmss_format(twoP - oneP), 
                                                 mmss_format(threeP - twoP), mmss_format(fourP - threeP), 
                                                 mmss_format(totalP)))
      
      km_splits
      
      
      
    })
    
    ## second database for total times IP training ----
    
    data_times_IP <- reactive({
      
      t <- ip_Data()%>% 
        filter(Date == input$date7 & Athlete %in% input$athlete3 & Session %in% "Training" & Training.effort == input$effort6)
      
      x <- data.frame(total.times.planned = rollapplyr(t$Planned.Schedule, nrow(t) ,sum, partial = TRUE),
                      total.times.actual = rollapplyr(t$Actual.Schedule, nrow(t) ,sum, partial = TRUE))
      
      y <- tibble(Distance.km = c("1", "2", "3", "4"),
                  Planned.Time = if (t[1,8] == 0.75) { c(x[4,1] + (t[4,1]/4), x[8,1] + (t[8,1]/4), x[12,1] + (t[12,1]/4), x[16,1] + (t[16,1]/4))} else {
                    
                    c(x[5,1] + (t[5,1]/4), x[9,1] + (t[9,1]/4), x[13,1] + (t[13,1]/4), x[17,1] + (t[17,1]/4))},
                  
                  Actual.Time = if (t[1,8] == 0.75) { c(x[4,2] + (t[4,2]/4), x[8,2] + (t[8,2]/4), x[12,2] + (t[12,2]/4), x[16,2] + (t[16,2]/4))} else {
                    
                    c(x[5,2] + (t[5,2]/4), x[9,2] + (t[9,2]/4), x[13,2] + (t[13,2]/4), x[17,2] + (t[17,2]/4)) } )
      
      
      ## Acutal times
      one <- as.numeric(y$Actual.Time[1])
      two <- as.numeric(y$Actual.Time[2])
      three <- as.numeric(y$Actual.Time[3])
      four <- as.numeric(y$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      
      ##Planned times
      
      oneP <- as.numeric(y$Planned.Time[1])
      twoP <- as.numeric(y$Planned.Time[2])
      threeP <- as.numeric(y$Planned.Time[3])
      fourP <- as.numeric(y$Planned.Time[4])
      twokP <- twoP - oneP
      threekP <- threeP - twoP
      fourkP <- fourP - threeP
      totalP <- sum(oneP,twokP,threekP,fourkP,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              'Actual Times' = c(mmss_format(one), mmss_format(two - one), 
                                                 mmss_format(three - two), mmss_format(four - three), 
                                                 mmss_format(total)),
                              'Planned Times'= c(mmss_format(oneP), mmss_format(twoP - oneP), 
                                                 mmss_format(threeP - twoP), mmss_format(fourP - threeP), 
                                                 mmss_format(totalP)))
      
      km_splits
      
    })
    
    ## second database for total times IP competition ----
    
    data_times_IP_comp <- reactive({
      
      t <- ip_Data()%>% 
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event,year(Date), sep = " "))%>%
        filter(Athlete %in% input$athlete9 & Comp.type %in% input$ql_f & eventList %in% input$ipCompEventFilter)
      
      x <- data.frame(total.times.planned = rollapplyr(t$Planned.Schedule, nrow(t) ,sum, partial = TRUE),
                      total.times.actual = rollapplyr(t$Actual.Schedule, nrow(t) ,sum, partial = TRUE))
      
      y <- tibble(Distance.km = c("1", "2", "3", "4"),
                  Planned.Time = if (t[1,8] == 0.75) { c(x[4,1] + (t[4,1]/4), x[8,1] + (t[8,1]/4), x[12,1] + (t[12,1]/4), x[16,1] + (t[16,1]/4))} else {
                    
                  c(x[5,1] + (t[5,1]/4), x[9,1] + (t[9,1]/4), x[13,1] + (t[13,1]/4), x[17,1] + (t[17,1]/4))},
                  
                  Actual.Time = if (t[1,8] == 0.75) { c(x[4,2] + (t[4,2]/4), x[8,2] + (t[8,2]/4), x[12,2] + (t[12,2]/4), x[16,2] + (t[16,2]/4))} else {
                    
                  c(x[5,2] + (t[5,2]/4), x[9,2] + (t[9,2]/4), x[13,2] + (t[13,2]/4), x[17,2] + (t[17,2]/4))})
      
      ## Acutal times
      one <- as.numeric(y$Actual.Time[1])
      two <- as.numeric(y$Actual.Time[2])
      three <- as.numeric(y$Actual.Time[3])
      four <- as.numeric(y$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      
      ##Planned times
      
      oneP <- as.numeric(y$Planned.Time[1])
      twoP <- as.numeric(y$Planned.Time[2])
      threeP <- as.numeric(y$Planned.Time[3])
      fourP <- as.numeric(y$Planned.Time[4])
      twokP <- twoP - oneP
      threekP <- threeP - twoP
      fourkP <- fourP - threeP
      totalP <- sum(oneP,twokP,threekP,fourkP,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              'Actual Times' = c(mmss_format(one), mmss_format(two - one), 
                                                 mmss_format(three - two), mmss_format(four - three), 
                                                 mmss_format(total)),
                              'Planned Times'= c(mmss_format(oneP), mmss_format(twoP - oneP), 
                                                 mmss_format(threeP - twoP), mmss_format(fourP - threeP), 
                                                 mmss_format(totalP)))
      
      km_splits
    })
    
    
    ## IP data frame from csv file input ----
    
    data_ip <- reactive({
      
      if (is.null(input$file2)) 
        
        return(NULL) 
      
      x <- read.csv(input$file2$datapath) 
      
      y <- data.frame(lap.splits = if (input$flyingip == FALSE) { c(0.25,0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75,12.75,13.75,14.75,15.75)
      } else {c(0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75,12.75,13.75,14.75,15.75,16.75)})%>%slice(1:input$ipslide) 
      x <- x %>% mutate(Date = as.Date(input$date6),
                        Athlete = input$athlete6,
                        Session = input$training_comp,
                        Comp.type = input$qf_f1,
                        Training.effort = input$effort1)%>% slice(1:input$ipslide)
      
      xy <- cbind(x,y)
      
      xy <- xy %>% mutate(Effort.type = input$effortTypeIP,
                          Notes = input$notesIP,
                          Event = input$compEvent2,
                          Trial = input$ipTrial)
      
    })
    
    
    
### Loaded Data frame from google sheets 
    
    ## TP data frame loaded from GS----
    
    tp_Data <- reactive({
      input$refresh
      x <- read_sheet(url, sheet = 1)
    })
    
    ## IP data frame loaded from GS----
    
    ip_Data <- reactive({
      input$refresh
      x <- read_sheet(url, sheet = 2)
    })
    
    ## 120/60 data frame-----
    
    twosix_Data <- reactive({
      input$refresh
      x <- read_sheet(url, sheet = 3)
    })
    
    ## power profile data frame ----
    
    pp_Data <- reactive({
      input$refresh
      x <- read_sheet(url, sheet = 4)
    })
    
    
  
 ### Plots & times tables for IP & TP analysis  
    
    ## TP Training graph----
    
    output$plot1 <- renderPlotly ({
      
      title <- tp_Data() %>%filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)
        
                                   
      
      title <- as.character(title[1,10])
      
      tp_Data() %>% 
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        
              
       
        plot_ly(x = ~lap.splits)%>%
        add_trace(y = ~Planned.Schedule, name='Planned', mode = 'lines', line = list(dash = 'dash'))%>%
        add_trace(y = ~Actual.Schedule, name = 'Actual', mode = 'lines+markers', text = ~Athlete, line = list(shape = 'spline'))%>%
        layout(yaxis = list(autorange = 'reversed', title = "<b>Planned / Acutal split times (seconds)</b>", size= 14),
               xaxis = list(title = "<b>Lap splits</b>", type = 'category', size = 14),
               legend = list(x = 0.9, y= 0.9),
               title = title)
      
        
    }) 
    
## SV TP competition -----
    
    ## total SV
    
    svFullTpComp <- reactive ({
 x <- tp_Data()%>%
      filter(Session %in% "Competition")%>%
      mutate(eventList = paste(Event, year(Date), sep = " "))%>%
      filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4, eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
      mutate(Diff = Actual.Schedule - Planned.Schedule)%>% 
      slice(-1)
 
     rollSV(x)

    })
    
    ##1st KM SV
    
    svKmTpComp <- reactive({
      x <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4, eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule)%>%
        slice(-1)
      
      
      
      rollSV1k(x)
        
  
      
    })
    
    ##1st lap SV
    
    svLapOne <- reactive ({
      x <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4, eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule, 
               SV = round(abs((Diff*1000)/Planned.Schedule),0))%>%
        select(SV)%>%
        slice(-1)%>%
        slice(1)
      
      y <- round((100 - x),0)
      
      y
      
    })
    
## SV TP Training ------
    
    ## total SV
    
    svFullTpTrain <- reactive ({
      x <- tp_Data()%>% 
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        select(Athlete ,Actual.Schedule, Planned.Schedule, lap.splits)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule)%>%
        slice(-1)
      
        rollSV(x)
      
    })
    
    ## 1st Km SV
    
    svKmTpTrain <- reactive ({
      x <- tp_Data()%>% 
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        select(Athlete ,Actual.Schedule, Planned.Schedule, lap.splits)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule)%>%
        slice(-1)
      
      rollSV1k(x)
     
    })
    
    ## 1st lap SV
    
    svLapOneTrain <- reactive ({
      x <- tp_Data()%>% 
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        select(Athlete ,Actual.Schedule, Planned.Schedule, lap.splits)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule, 
               SV = round(abs((Diff*1000)/Planned.Schedule),0))%>%
        select(SV)%>%
        slice(-1)%>%
        slice(1)

      y <- round((100 - x),0)
      
      y
    })
  
  ## render SV TP training ------
    
    ## full SV
    
output$svTpTrain <- renderPlotly({
  x <- as.numeric(svFullTpTrain())
  x2 <- as.numeric(svKmTpTrain())
  x3 <- as.numeric(svLapOneTrain())
  
  fig <- plot_ly()
  fig <- fig %>% add_trace(
      type = "indicator",
      mode = "number+gauge",
      gauge = list(shape = "bullet", axis = list(range = list(0,100))),
      value = x,
      domain = list(x = c(0.25, 1), y = c(0.08, 0.25)),
      title= list(text = "<b>Overall</b>"))
  
  fig<-fig %>%
    add_trace(type = "indicator",
              mode = "number+gauge",
              gauge = list(shape = "bullet",axis = list(range = list(0,100))),
              value = x2,
              domain = list(x = c(0.25, 1), y = c(0.4, 0.6)),
              title= list(text = "<b>First Km</b>"))
  
  fig<- fig %>% add_trace(
    type = "indicator",
    mode = "number+gauge",
    gauge = list(shape = "bullet", axis = list(range = list(0,100))),
    value = x3,
    domain = list(x = c(0.25, 1), y = c(0.7, 0.9)),
    title= list(text = "<b>First Lap</b>"))
  
  fig
  
})
    
    
## Render SV TP Competition -----
    

    
  output$svFull <- renderPlotly({
    
    x <- as.numeric(svFullTpComp())
    x2<- as.numeric(svKmTpComp())
    x3<- as.numeric(svLapOne())
  
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type = "indicator",
      mode = "number+gauge",
      gauge = list(shape = "bullet", axis = list(range = list(0,100))),
      value = x,
      domain = list(x = c(0.25, 1), y = c(0.08, 0.25)),
      title= list(text = "<b>Overall</b>"))
      
    fig<-fig %>%add_trace(type = "indicator",
                mode = "number+gauge",
                gauge = list(shape = "bullet",axis = list(range = list(0,100))),
                value = x2,
                domain = list(x = c(0.25, 1), y = c(0.4, 0.6)),
                title= list(text = "<b>First Km</b>"))
      
      fig<- fig %>% add_trace(
        type = "indicator",
        mode = "number+gauge",
        gauge = list(shape = "bullet", axis = list(range = list(0,100))),
        value = x3,
        domain = list(x = c(0.25, 1), y = c(0.7, 0.9)),
        title= list(text = "<b>First Lap</b>"))
      
      fig
    
  })  
    
    
    
    
 ## TP Competition graph -----
    
    output$plot14 <- renderPlotly({
      
      title <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4 & eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)
      
      title <- as.character(title[1,10])
      
      tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4, eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
        mutate(Diff = Actual.Schedule - Planned.Schedule,
               SV = round(abs((Diff*1000)/Planned.Schedule),0))%>%
        
        
        plot_ly()%>%
        add_trace(x = ~lap.splits,y = ~Planned.Schedule, name='Planned', mode = 'lines', line = list(dash = 'dash'), type = 'scatter')%>%
        add_trace(x = ~lap.splits,y = ~Actual.Schedule, name = 'Actual', mode = 'lines+markers', text = ~Athlete, line = list(shape = 'spline'))%>%
        layout(yaxis = list(autorange = 'reversed', title = "<b>Planned / Acutal split times (seconds)</b>", size= 14),
               xaxis = list(title = "<b>Lap splits</b>", type = 'category', size = 14),
               legend = list(x = 0.9, y= 0.9),
               title = title)
      
    })
    
 ### data table for TP training graph----
    
    tp_data_new_t <- reactive ({
      
      x <- tp_Data()%>% 
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        select(Athlete ,Actual.Schedule, Planned.Schedule, lap.splits)%>%
        mutate(Difference = round(Actual.Schedule - Planned.Schedule, 2))
      
      
      y <- x %>%
        pivot_wider(names_from = Athlete, values_from = Actual.Schedule)
      
      y <- sapply(y, as.character)    
      
      y[is.na(y)] <- "-"
      
      y <- as.data.frame(y)
      
      y$Difference <- as.numeric(y$Difference)
      
      y
    })

    
    output$table2 <- renderReactable ({
      
      tp_data_new_t()%>%
        rename(Planned = Planned.Schedule)%>%
        reactable(defaultColDef = colDef(
          header = function(value) gsub(".", " ", value, fixed = TRUE),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          headerStyle = list(background = "#000099", color = 'white', fontweight = 'bold')),
          bordered = TRUE,
          highlight = TRUE,
          showSortIcon = FALSE,
          pagination = FALSE,
          compact = TRUE,
          columns = list(Difference = colDef(
            style = function(value) {
              colorback <- if(value <= -0.20) {'yellow'} 
              else if (value >= -0.21 & value <= -0.10) {'lawngreen'}
              else if (value >= -0.11 & value <= 0.10) {'forestgreen'}
              else if (value >= 0.11 & value <= 0.20) {'darkorange'}
              else  {'orangered'}
              
              colortext <- if(value <= -0.2) { 'black'} 
              else if (value >= -0.21 & value <= -0.10) {'black'}
              else if (value >= -0.11 & value <= 0.10) {'white'}
              else if (value >= 0.11 & value <= 0.20) {'white'}
              else  {'white'} 
              
              list(background = colorback, fontweight = 'bold', color = colortext)
            }
          )))
      
    })
    
    
    
### data table for TP competition graph ----
    
    
    tp_data_new_c <- reactive ({
      
      x <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date) ,sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4 & eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
        select(Athlete ,Actual.Schedule, Planned.Schedule, lap.splits)%>%
        mutate(Difference = round(Actual.Schedule - Planned.Schedule,2))
      
      y <- x %>%
        pivot_wider(names_from = Athlete, values_from = Actual.Schedule)
       
      y <- sapply(y, as.character)    
   
      y[is.na(y)] <- "-"
      
      y <- as.data.frame(y)
      
      y$Difference <- as.numeric(y$Difference)
      
      y
      
      
    })
    
    
    output$table20 <- renderReactable({
      
      tp_data_new_c()%>%
        rename(Planned = Planned.Schedule)%>%
        reactable(defaultColDef = colDef(
                       header = function(value) gsub(".", " ", value, fixed = TRUE),
                       cell = function(value) format(value, nsmall = 1),
                       align = "center",
                       headerStyle = list(background = "#000099", color = 'white', fontweight = 'bold')),
                  bordered = TRUE,
                  highlight = TRUE,
                  showSortIcon = FALSE,
                  pagination = FALSE,
                  compact = TRUE,
                  columns = list(Difference = colDef(
                    style = function(value) {
                      
                    colorback <- if(value <= -0.20) {'yellow'} 
                      else if (value >= -0.21 & value <= -0.10) {'lawngreen'}
                      else if (value >= -0.11 & value <= 0.10) {'forestgreen'}
                      else if (value >= 0.11 & value <= 0.20) {'darkorange'}
                      else  {'orangered'}
                    
                    colortext <- if(value <= -0.2) { 'black'} 
                    else if (value >= -0.21 & value <= -0.10) {'black'}
                    else if (value >= -0.11 & value <= 0.10) {'white'}
                    else if (value >= 0.11 & value <= 0.20) {'white'}
                    else  {'white'} 
                    
                    list(background = colorback, fontweight = 'bold', color = colortext)
                    }
                  )))
                  
       
    })
  
 
### Render notes IP comp ----
    output$pasteNotesIpComp <- renderText({
      x <- ip_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Athlete %in% input$athlete9 & Comp.type %in% input$ql_f & eventList %in% input$ipCompEventFilter)%>%
        select(Notes)
      x<- x[1,1]
      y <- as.character("Notes:")
      
      paste(y,x,sep = " ")
    })
    
### Render notes IP train -----
    output$pasteNotesIpTrain <- renderText({
      x <- ip_Data()%>%
        filter(Date == input$date7 & Athlete %in% input$athlete3 & Session %in% "Training" & Training.effort == input$effort6)%>%
        select(Notes)
      x<- x[1,1]
      y <- as.character("Notes:")
      
      paste(y,x,sep = " ")
    })
    
    
## Render notes TP Train -----
    output$pasteNotesTpTrain <- renderText({
      x <- tp_Data()%>%
        filter(Date == input$date2 & Men.Women %in% input$men_women_plotTP_t & Session %in% "Training" & Training.effort == input$effort7)%>%
        select(Notes)
      x<- as.character(x[1,1])

    })
    
## Render notes TP Comp -----
    output$pasteNotesTpComp <- renderText({
      x <- tp_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date) ,sep = " "))%>%
        filter(Men.Women %in% input$men_women_plotTP_comp & Comp.type %in% input$ql_f4 & eventList %in% input$tpCompEventFilter & Team %in% input$stateTeam1)%>%
        select(Notes)
      x<- as.character(x[1,1])

      x
      
    })
    
### IP graph training ----
    
    output$plot2 <- renderPlotly ({
      
      title <- ip_Data() %>%
        filter(Date == input$date7 & Athlete %in% input$athlete3 & Session %in% "Training" & Training.effort == input$effort6)
      
      title <- as.character(title[1,9])
      
      ip_Data() %>%
        filter(Date == input$date7 & Athlete %in% input$athlete3 & Session %in% "Training" & Training.effort == input$effort6)%>%
        plot_ly(x = ~lap.splits)%>%
        add_trace(y = ~Planned.Schedule, name='Planned', mode = 'lines', line = list(dash = 'dash'))%>%
        add_trace(y = ~Actual.Schedule, name = 'Actual', mode = 'lines+markers', line = list(shape = 'spline'))%>%
        layout(yaxis = list(autorange = 'reversed', title = "<b>Planned / Acutal split times (seconds)</b>", size= 14),
               xaxis = list(title = "<b>Lap splits</b>", type = 'category', size = 14),
               legend = list(x = 0.9, y= 0.9),
               title = title)
      
    }) 

### IP graph competition ----
    
    output$plot7 <- renderPlotly ({
      
      x <- ip_Data()%>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Athlete %in% input$athlete9 & Comp.type %in% input$ql_f & eventList %in% input$ipCompEventFilter)
      
      effort <- as.character(x[1,9])
      date1 <- as.Date(x$Date[1])
      date2 <- format(date1, format ='%d-%B-%Y')
      title <- paste(effort, date2, sep = " ")              

      ip_Data() %>%
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Athlete %in% input$athlete9 & Comp.type %in% input$ql_f & eventList %in% input$ipCompEventFilter)%>%
        plot_ly(x = ~lap.splits)%>%
        add_trace(y = ~Planned.Schedule, name='Planned', mode = 'lines', line = list(dash = 'dash'))%>%
        add_trace(y = ~Actual.Schedule, name = 'Actual', mode = 'lines+markers',line = list(shape = 'spline'))%>%
        layout(yaxis = list(autorange = 'reversed', title = "<b>Planned / Acutal split times (seconds)</b>", size= 14),
               xaxis = list(title = "<b>Lap splits</b>", type = 'category', size = 14),
               legend = list(x = 0.9, y= 0.9),
               title = title)
    })
    
## IP Training data table for graph----
    
  output$table9 <- function(){
    
    ##actual
    x <- ip_Data()%>% 
      filter(Date == input$date7 & Athlete %in% input$athlete3 & Session %in% "Training" & Training.effort == input$effort6)%>%
      mutate(Difference = Actual.Schedule - Planned.Schedule)
      
    actual <- x%>%  
      select(lap.splits, Actual.Schedule)%>%
      pivot_wider(names_from = lap.splits, values_from = Actual.Schedule)
    
    ##planned 
    
    planned <- x%>%
      select(lap.splits, Planned.Schedule)%>%
      pivot_wider(names_from = lap.splits, values_from = Planned.Schedule)
    
    ## difference column make wide
    
    diff <- x%>%
      select(lap.splits, Difference)%>%
      mutate(Difference = cell_spec(round(Difference,2), background = lapply(Difference, colorFormat),  color = lapply(Difference, colorTextFormat)))%>%
      pivot_wider(names_from = lap.splits, values_from = Difference)
    
    
    ## row bind 
    
    new <- rbind(actual,planned,diff)
    
    ## add column actual and planned 
    
    A_P <- data.frame(Lap.Splits = c("Actual", "Planned", "Difference"))
    
    
    ## cbind 
    
    new1 <- bind_cols(new, A_P) 
    
    new1 <- select(new1, Lap.Splits, everything())
    
    new1 %>%
      kbl(escape = FALSE)%>%
      kable_styling(bootstrap_options = "condensed", full_width = T)%>%
      column_spec(1,bold = T, border_right = T)
    
  }
    
## IP competition data table for graph----
    
    output$table10 <- function(){
      
      ##actual
      x <- ip_Data()%>% 
        filter(Session %in% "Competition")%>%
        mutate(eventList = paste(Event, year(Date), sep = " "))%>%
        filter(Athlete %in% input$athlete9 & Comp.type %in% input$ql_f & eventList %in% input$ipCompEventFilter)%>%
        mutate(Difference = Actual.Schedule - Planned.Schedule)
      
      actual <- x%>%  
        select(lap.splits, Actual.Schedule)%>%
        pivot_wider(names_from = lap.splits, values_from = Actual.Schedule)
      
      ##planned 
      
      planned <- x%>%
        select(lap.splits, Planned.Schedule)%>%
        pivot_wider(names_from = lap.splits, values_from = Planned.Schedule)
      
      ## difference column make wide
      
      diff <- x%>%
        select(lap.splits, Difference)%>%
        mutate(Difference = cell_spec(round(Difference,2), background = lapply(Difference, colorFormat),  color = lapply(Difference, colorTextFormat)))%>%
        pivot_wider(names_from = lap.splits, values_from = Difference)
      
      
      ## row bind 
      
      new <- rbind(actual,planned,diff)
      
      ## add column actual and planned 
      
      A_P <- data.frame(Lap.Splits = c("Actual", "Planned", "Difference"))
        
        ## cbind 
        
        new1 <- bind_cols(new, A_P)
        
        new1 <- select(new1, Lap.Splits, everything())

        new1 %>%
        kbl(escape = FALSE)%>%
        kable_styling(bootstrap_options = "condensed", full_width = T)%>%
          column_spec(1,bold = T, border_right = T)
      
    }
  
## TP km split times & TOTAL competition table -------
    
    output$tpTimesKmComp <- renderReactable({
      
    x<-data_times_tp_competition()|>
        filter_all(all_vars(!is.na(.)))
    
    x|>
      reactable(defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        cell = function(value) format(value, nsmall = 1),
        align = "center",
        headerStyle = list(background = "black", color = 'white', fontweight = 'bold')),
        bordered = TRUE,
        highlight = TRUE,
        showSortIcon = FALSE,
        pagination = FALSE,
        compact = TRUE)
      
    })
    

## TP km split times & TOTAL training table -------
    
    output$tpTimesKmTrain <- renderReactable({
      
      x<- data_times_tp_training()|>
        filter_all(all_vars(!is.na(.)))
      
      x|>
        reactable(defaultColDef = colDef(
          header = function(value) gsub(".", " ", value, fixed = TRUE),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          headerStyle = list(background = "black", color = 'white', fontweight = 'bold')),
          bordered = TRUE,
          highlight = TRUE,
          showSortIcon = FALSE,
          pagination = FALSE,
          compact = TRUE)
      
    })
    
## IP km split times & total time training ----
    
    output$ipTimesKmTrain <- function(){
      
      x<- data_times_IP()|>
        filter_all(all_vars(!is.na(.)))
      
      x|>
        
        kbl()|>
        kable_styling(bootstrap_options = "striped", full_width = F, position = 'left')
      
    }
    
    
    
## IP km split times & total time competition ----
    output$ipTimesKmComp <- function(){
      
      x<-data_times_IP_comp()|>
        filter_all(all_vars(!is.na(.)))
      
      x|>
        kbl()|>
        kable_styling(bootstrap_options = "striped", full_width = F, position = 'left')
      
    }
    



    
## 120/60's graphs -----------------
    
    ##index plot
    output$sixtiesPlot <- renderPlotly({
     
      x <- onetwentysix()%>%
           mutate(index = (Power/HR)*100)%>%
           filter(Athlete %in% input$athlete10)%>%
           arrange(Age)%>%
           ggplot(aes(x = as.character(Age), y= index, color = index))+
           scale_color_gradient(low = "red", high = "blue")+
           geom_point()+
          # stat_smooth(method = 'lm', se = FALSE, linetype = 4)+
           theme_bw()+
           xlab("Age")
         
      
      ggplotly(x, tooltip = "y" )%>%
        config(displayModeBar = FALSE)
        
  
         })
    
    ## HR & power plot
    
    output$sixtiesPlot1 <- renderPlotly({
      
      onetwentysix()%>%
        filter(Athlete %in% input$athlete10)%>%
        arrange(Age)%>%
        plot_ly(x = ~Age, y = ~Power, name = "Power", type = "bar")%>%
        add_trace(x = ~Age, y = ~HR, name = "HR", yaxis = "y2", type = "scatter", mode = "markers")%>%
        layout(yaxis2 = list(overlaying = "y", title = "HR", side = "right", range = c(135,210)), 
               xaxis = list(type = 'category'),
               yaxis = list(range = c(180,450)),
               hovermode = "x unified")%>%
        config(displayModeBar = FALSE)
        
      
    
      
    })
    
    
    
    
## max power profile plot ------
    
  output$maxpp <- renderPlotly({
    
   x<- pp_Data()|>
      filter(Athlete %in% c(input$athlete10,input$person1,input$person2,input$person3))|>
      filter(Duration %in% c(5,60,240,600,1200,3600))|>
      group_by(Athlete, Duration)|>
      filter(Power == max(Power))|>
      arrange(Duration)
      
   x$Duration <- as.factor(x$Duration)
        
   y<-x|>
     ggplot(aes(x = Duration, y = Power, color = Athlete))+
     geom_point(stat = "identity",size = 2.0)+
     theme_bw()+
     ylab("Power (Watts)")+
     xlab("Duration (Seconds)")+
     ggtitle("Compare Power Profiles (Best efforts)")+
     theme(plot.title = element_text(hjust = 0.5, face = "bold"),
           legend.title = element_blank())
   
   ggplotly(y, tooltip = "y" )%>%
      config(displayModeBar = FALSE)
    
    
  })
    
    
## power profile main data table ----
    
    
    output$four_min <- renderDataTable({
      x <- pp_Data()|>
        filter(Athlete %in% input$athlete10)|>
        select(Age, Duration, Power, Bike)|>
        arrange(Age)
      
      x$Age <- as.character(x$Age)
      x$Duration <- as.character(x$Duration)
      x$Power <- as.character(x$Power)

      datatable(x,rownames = FALSE,filter = "top", options = list(dom = 'tip', pageLength = 12), class = 'cell-border stripe')
      
    })
    
    
    

    
## compare plot power profile ------
    
    output$plotCompare <- renderPlotly({
      

      x <- pp_Data()%>%
        mutate(w_kg = round(Power / Weight, 2),
               w_CdA = round(Power/(CdA*100), 3))%>%
        filter(Athlete %in% c(input$athletecomp, input$athlete10) & Duration == as.numeric(input$durationFilter))%>%
        ggplot(aes_string(x = 'Age', y = input$powerFilter , color = 'Athlete'))+
        geom_point(size = 1.5, aes(shape = Bike))+
        stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Athlete),se= FALSE, linetype = 3)+
        theme_bw()+
        theme(legend.title = element_blank())
      
      ggplotly(x)
      
    })
    
    #stat_smooth(method = 'lm', aes(color = Athlete),formula= (y ~ exp(x)), se=TRUE, linetype = 4)
    
    
## 120/60s graph 
    
    
## Power profile raw data table -----

    pp_df <- reactive({
      pp_Data()%>%
        filter(Date >= input$date45[1] & Date <= input$date45[2] & Athlete %in% input$athlete85)%>%
        arrange(Age)
      
    })
    
    output$ppRawData <- renderRHandsontable({
    rhandsontable(pp_df())
    })
    
    
## 120/60 raw data table -----
    
    onetwentysix_df <- reactive({
      onetwentysix()%>%
        filter(Date >= input$date46[1] & Date <= input$date46[2] & Athlete %in% input$athlete86)%>%
        arrange(Age)
      
    })
    
    output$onetwentyRawData <- renderRHandsontable({
      rhandsontable(onetwentysix_df())
    })
    
    
    
## Raw IP data table & data frame ----
    
    ip_raw_df <- reactive({
      ip_Data()%>%
        filter(Date >= input$date19[1] & Date <= input$date19[2] & Athlete %in% input$athlete99 &
               Effort.type %in% input$effort70 & Session %in% input$ipCompEventF2)%>%
        
        {if (input$trialRawFilterIp == TRUE) filter(., Trial %in% "Trial") else .}

    })
    
    output$rawData1 <- renderRHandsontable({
      rhandsontable(ip_raw_df())
    })
    
## Raw TP data table & data frame ----
    tp_raw_df <- reactive({
      tp_Data()%>%
        filter(Date >= input$date11[1] & Date <= input$date11[2] & Effort.type %in% input$effort99 &
               Session %in% input$tpCompEventF2 & Men.Women %in% input$tpGroup)%>%
      
      {if (input$trialRawFilterTp == TRUE) filter(., Trial %in% "Trial") else .}

    })
    
    output$rawData <- renderRHandsontable({
      rhandsontable(tp_raw_df())
    })

    
## Compare IP data frame ------

analyse_df <- reactive({   
  
    df1 <- ip_Data()%>%
       filter(Date == input$IpCompareDate & Effort.type %in% input$eventCompare & 
              Athlete %in% input$athleteCompareIp & Session %in% input$trainCompFilter)%>%
      
        {if (input$trainCompFilter %in% "Training") filter(.,Training.effort == input$TrainIpCompareEffort) else 
             filter(.,Comp.type %in% input$CompIpCompare) }
      
      
    df2 <- ip_Data()%>%
      filter(Date == input$IpCompareDate2 & Effort.type %in% input$eventCompare1 & 
               Athlete %in% input$athleteCompareIp2 & Session %in% input$trainCompFilter2)%>%
      
      {if (input$trainCompFilter2 %in% "Training") filter(.,Training.effort == input$TrainIpCompareEffort2) else 
        filter(.,Comp.type %in% input$CompIpCompare2) }
      
    
    
    df1new <- as.data.frame(df1)
    
    df2new <- as.data.frame(df2)
    
    df1new[is.na(df1new)] <- ""
    df2new[is.na(df2new)] <- ""
    
    Df <- df1new|>
      mutate(item = paste(Date, Athlete, Session ,Comp.type, Training.effort, sep = " "))
    
    Df2 <- df2new|>
      mutate(item = paste(Date, Athlete, Session ,Comp.type, Training.effort, sep = " "))
    
    new_df <- rbind(Df, Df2)

    
    new_df <- new_df %>%
      arrange(lap.splits)
    
    new_df$lap.splits <- as.vector(new_df$lap.splits)
    
    
    new_df
    
  })
    
## IP compare plot -----
    
    output$ipComparePlot <- renderPlotly({
      
    y <- analyse_df()%>%
      ggplot(aes(x = factor(lap.splits), y = Actual.Schedule, group = item, color = item))+
               geom_point()+
               geom_line()+
               theme_bw()+
               ylab("Time (Seconds)")+
               xlab("Lap Splits")+
               scale_y_reverse(breaks = seq(0,20,0.4))+
               theme(legend.position = "top",
                     legend.title = element_blank())
      
      ggplotly(y, tooltip = 'y')%>%
        layout(legend = list(orientation = "h", xanchor = "center",x = 0.5))
    
      
    })
    
    

## Compare IP analysis event list  ------
    
    ## create reactive event list
    
    ipEventList <- reactive({
     x<- ip_Data()|>
        select(Effort.type)
     
     x<- na.omit(x)
    })
    
    ## update select input 1
    
    observe({
      
      x <- ipEventList()
      
      updateSelectInput(session,
                        inputId = "eventCompare",
                        choices = x,
                        selected = head(x,1))
    })
    
   
    ## update select input 2
    
    observe({
      
      x <- ipEventList()
      
      updateSelectInput(session,
                        inputId = "eventCompare1",
                        choices = x,
                        selected = head(x,1))
    })

    
    
    
## Compare TP analysis event list ------
    
    ## create reactive event list
    
    tpEventList <- reactive({
      x<- tp_Data()|>
        select(Effort.type)
      
      x<- na.omit(x)
    })
    
    ## update select input 1 TP
    
    observe({
      
      x <- tpEventList()
      
      updateSelectInput(session,
                        inputId = "tpCompareEffortType",
                        choices = x,
                        selected = head(x,1))
    })
    
    
    ## update select input 2 TP
    
    observe({
      
      x <- tpEventList()
      
      updateSelectInput(session,
                        inputId = "tpCompareEffortType1",
                        choices = x,
                        selected = head(x,1))
    })
    
    
    
    
    
    
## lap splits and kilo times for IP compare analysis ----
    
    output$analysesTable <- function(){
      analyse_df()%>%
        select(lap.splits,Actual.Schedule, item)%>%
        pivot_wider(names_from = item, values_from = Actual.Schedule)%>%
        kbl(align = c('c',"c",'c'), linesep = "")%>%
        kable_styling(bootstrap_options = c('hover','striped','condensed'), font_size = 14, fixed_thead = T)%>%
        row_spec(0, bold = T, color = 'white', background = "#281794")
    }
    
## TP analysis data table -------
    
    output$tpanalysesTable <- function(){
      analyse_df_tp()%>%
        select(lap.splits,Actual.Schedule, item, Athlete)%>%
        pivot_wider(names_from = item, values_from = Actual.Schedule)%>%
        kbl(align = c('c',"c",'c'), linesep = "")%>%
        kable_styling(bootstrap_options = c('hover','striped','condensed'), font_size = 14, fixed_thead = T)%>%
        row_spec(0, bold = T, color = 'white', background = "#281794")
    }
    
## IP compare analysis Kilo times ----
    
    ipCompareKiloDf <- reactive({
      
      
    ## First df 
     filterDf <- ip_Data()%>%
        filter(Date == input$IpCompareDate & Effort.type %in% input$eventCompare & 
                 Athlete %in% input$athleteCompareIp & Session %in% input$trainCompFilter)%>%
        
        {if (input$trainCompFilter %in% "Training") filter(.,Training.effort == input$TrainIpCompareEffort) else 
          filter(.,Comp.type %in% input$CompIpCompare) }
      
      
      x_new <- data.frame(total.times.actual = rollapplyr(filterDf$Actual.Schedule, nrow(filterDf) ,sum, partial = TRUE))
      
      df <- tibble(Actual.Time = if (filterDf[1,8] == 0.75) { c(x_new[4,1] + (filterDf[4,2]/4), x_new[8,1] + (filterDf[8,2]/4), x_new[12,1] + (filterDf[12,2]/4), x_new[16,1] + (filterDf[16,2]/4))} 
                  else {c(x_new[5,1] + (filterDf[5,2]/4), x_new[9,1] + (filterDf[9,2]/4), x_new[13,1] + (filterDf[13,2]/4), x_new[17,1] + (filterDf[17,2]/4)) } )
      
      
      ## Acutal times first df
      one <- as.numeric(df$Actual.Time[1])
      two <- as.numeric(df$Actual.Time[2])
      three <- as.numeric(df$Actual.Time[3])
      four <- as.numeric(df$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              Actual.Times = c(mmss_format(one), mmss_format(two - one), 
                                                  mmss_format(three - two), mmss_format(four - three), 
                                                  mmss_format(total)))
      
      ## second df compare
      
      t1 <- ip_Data()%>%
        filter(Date == input$IpCompareDate2 & Effort.type %in% input$eventCompare1 & 
                 Athlete %in% input$athleteCompareIp2 & Session %in% input$trainCompFilter2)%>%
        
        {if (input$trainCompFilter2 %in% "Training") filter(.,Training.effort == input$TrainIpCompareEffort2) else 
          filter(.,Comp.type %in% input$CompIpCompare2) }
      
      
      x1 <- data.frame(total.times.actual = rollapplyr(t1$Actual.Schedule, nrow(t1) ,sum, partial = TRUE))
      
      df1 <- tibble(Actual.Time = if (t1[1,8] == 0.75) { c(x1[4,1] + (t1[4,2]/4), x1[8,1] + (t1[8,2]/4), x1[12,1] + (t1[12,2]/4), x1[16,1] + (t1[16,2]/4))} 
                   else { c(x1[5,1] + (t1[5,2]/4), x1[9,1] + (t1[9,2]/4), x1[13,1] + (t1[13,2]/4), x1[17,1] + (t1[17,2]/4)) } )
      
      
      
      ## Acutal times second df
      one1 <- as.numeric(df1$Actual.Time[1])
      two1 <- as.numeric(df1$Actual.Time[2])
      three1 <- as.numeric(df1$Actual.Time[3])
      four1 <- as.numeric(df1$Actual.Time[4])
      twok1 <- two1 - one1
      threek1 <- three1 - two1
      fourk1 <- four1-three1
      total1 <- sum(one1,twok1,threek1,fourk1,na.rm = TRUE)
      
      
      km_splits1 <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              Actual.Times = c(mmss_format(one1), mmss_format(two1 - one1), 
                                                 mmss_format(three1 - two1), mmss_format(four1 - three1), 
                                                mmss_format(total1)))
      
      ## get col names from other df 
     col <- analyse_df()%>%
        select(lap.splits,Actual.Schedule, item)%>%
        pivot_wider(names_from = item, values_from = Actual.Schedule)
      
      col1 <- colnames(col[2])
      col2 <- colnames(col[3])
        
      
      ##Join data frames
      
      ## convert back to seconds and create difference column
      
      difOne <- sec_format(km_splits$Actual.Times)
      difTwo <- sec_format(km_splits1$Actual.Times)
      dif <- difTwo - difOne
      
      
      
      new_df <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                          one = km_splits$Actual.Times,
                          two = km_splits1$Actual.Times,
                          Difference = dif)
      
      colnames(new_df) <- c("Distance",col1,col2, "Difference")
      
      new_df
      
   
    })
    
## TP compare analysis kilo times ------- 
    
    tpCompareKiloDf <- reactive({
      ## First df 
      filterDf <-tp_Data()%>%
        filter(Date == input$tpAnalyseDate & Effort.type %in% input$tpCompareEffortType & 
                 Men.Women %in% input$tpMenWomen & Session %in% input$TPtrainCompFilter)%>%
        
        {if (input$TPtrainCompFilter %in% "Training") filter(.,Training.effort == input$TrainTpCompareEffort) else 
          filter(.,Comp.type %in% input$CompTpCompare & Team %in% input$teamCompTP) }
      
      
      x_new <- data.frame(total.times.actual = rollapplyr(filterDf$Actual.Schedule, nrow(filterDf) ,sum, partial = TRUE))
      
      df <- tibble(Actual.Time = if (filterDf[1,9] == 0.75) { c(x_new[4,1] + (filterDf[4,2]/4), x_new[8,1] + (filterDf[8,2]/4), x_new[12,1] + (filterDf[12,2]/4), x_new[16,1] + (filterDf[16,2]/4))} 
                   else {c(x_new[5,1] + (filterDf[5,2]/4), x_new[9,1] + (filterDf[9,2]/4), x_new[13,1] + (filterDf[13,2]/4), x_new[17,1] + (filterDf[17,2]/4)) } )
      
      ## Acutal times first df
      one <- as.numeric(df$Actual.Time[1])
      two <- as.numeric(df$Actual.Time[2])
      three <- as.numeric(df$Actual.Time[3])
      four <- as.numeric(df$Actual.Time[4])
      twok <- two - one
      threek <- three - two
      fourk <- four-three
      total <- sum(one,twok,threek,fourk,na.rm = TRUE)
      
      km_splits <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                              Actual.Times = c(mmss_format(one), mmss_format(two - one), 
                                               mmss_format(three - two), mmss_format(four - three), 
                                               mmss_format(total)))
      
      ## second df compare
      
      t1 <- tp_Data()%>%
        filter(Date == input$tpAnalyseDate1 & Effort.type %in% input$tpCompareEffortType1 & 
                 Men.Women %in% input$tpMenWomen1 & Session %in% input$TPtrainCompFilter1)%>%
        
        {if (input$TPtrainCompFilter1 %in% "Training") filter(.,Training.effort == input$TrainTpCompareEffort1) else 
          filter(.,Comp.type %in% input$CompTpCompare1 & Team %in% input$teamCompTP1) }
      
      
      x1 <- data.frame(total.times.actual = rollapplyr(t1$Actual.Schedule, nrow(t1) ,sum, partial = TRUE))
      
      df1 <- tibble(Actual.Time = if (t1[1,9] == 0.75) { c(x1[4,1] + (t1[4,2]/4), x1[8,1] + (t1[8,2]/4), x1[12,1] + (t1[12,2]/4), x1[16,1] + (t1[16,2]/4))} 
                    else { c(x1[5,1] + (t1[5,2]/4), x1[9,1] + (t1[9,2]/4), x1[13,1] + (t1[13,2]/4), x1[17,1] + (t1[17,2]/4)) } )
      
      
      
      ## Acutal times second df
      one1 <- as.numeric(df1$Actual.Time[1])
      two1 <- as.numeric(df1$Actual.Time[2])
      three1 <- as.numeric(df1$Actual.Time[3])
      four1 <- as.numeric(df1$Actual.Time[4])
      twok1 <- two1 - one1
      threek1 <- three1 - two1
      fourk1 <- four1-three1
      total1 <- sum(one1,twok1,threek1,fourk1,na.rm = TRUE)
      
      
      km_splits1 <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                               Actual.Times = c(mmss_format(one1), mmss_format(two1 - one1), 
                                                mmss_format(three1 - two1), mmss_format(four1 - three1), 
                                                mmss_format(total1)))
      
      ## get col names from other df 
      col <- analyse_df_tp()%>%
        select(lap.splits,Actual.Schedule, item)%>%
        pivot_wider(names_from = item, values_from = Actual.Schedule)
      
      col1 <- colnames(col[2])
      col2 <- colnames(col[3])
      
      
      ##Join data frames
      
      ## convert back to seconds and create difference column
      
      difOne <- sec_format(km_splits$Actual.Times)
      difTwo <- sec_format(km_splits1$Actual.Times)
      dif <- difTwo - difOne
      
      
      
      new_df <- data.frame(Distance = c("1km", "2km", "3km", "4km", "Total"),
                           one = km_splits$Actual.Times,
                           two = km_splits1$Actual.Times,
                           Difference = dif)
      
      colnames(new_df) <- c("Distance",col1,col2, "Difference")
      
      new_df
    
      
    })
    
    
    
    
## IP compare kilo times plot table ----
    
    output$ipCompareKiloPlot <- function(){

       ipCompareKiloDf()%>%
        replace_na(list(Difference = 0))%>%
         mutate(Difference = cell_spec(round(Difference,2), background = ifelse(Difference <= 0.01, "green", "red"), color = 'white', bold = TRUE))%>%
         kbl(align = c('c',"c",'c'), escape = FALSE)%>%
         kable_styling(bootstrap_options = c('hover','striped','condensed'), font_size = 15)%>%
         row_spec(0, bold = T, color = 'white', background = "black")%>%
         column_spec(4, color = "white")
    }
    
## TP compare kilo times plot table -----
    
    output$tpCompareKiloPlot <- function(){
      
      tpCompareKiloDf()%>%
        replace_na(list(Difference = 0))%>%
        mutate(Difference = cell_spec(round(Difference,2), background = ifelse(Difference <= 0.01, "green", "red"), color = 'white', bold = TRUE))%>%
        kbl(align = c('c',"c",'c','c'), escape = FALSE)%>%
        kable_styling(bootstrap_options = c('hover','striped','condensed'), font_size = 15)%>%
        row_spec(0, bold = T, color = 'white', background = "black")
        
        
    }
    
    
## Power prediction model ----
    
    ## model df *********
    
    modelDf <- reactive({
      x <-pp_Data()%>%
        filter(Athlete %in% input$athletePred)%>%
        filter(Duration %in% c(5,60,180,240,600))%>%
        filter(Date >= as.Date(input$calDate[1]-200) & Date <= as.Date(input$calDate)[2])%>%
        group_by(Duration)%>%
        summarise(mPower = mean(Power))
      
      x<- as.data.frame(x)
      
    })
    
  ## Model
    
   model <- reactive({
     

     fit1 <- lm(log(mPower)~ log(Duration), data = modelDf())
     fit2 <- nls(mPower ~ a * Duration^b + c, data = modelDf(), start = list(a = exp(coef(fit1)[1]), b = coef(fit1)[2], c = 100))
     
     fit2
     
   })
   
  ## model fit 1
   
   fit1 <- reactive({
     lm(log(mPower)~ log(Duration), data = modelDf()) 
   })
   
  ## power output from model 
   
   predictedPower <- reactive({
     
     
     onek <- as.numeric(round(predict(model(), newdata = data.frame(Duration = pwrCalDf()$Time[1]))))
     twok <- as.numeric(round(predict(model(), newdata = data.frame(Duration = pwrCalDf()$Time[2]))))
     threek <- as.numeric(round(predict(model(), newdata = data.frame(Duration = pwrCalDf()$Time[3]))))
     fourk <- as.numeric(round(predict(model(), newdata = data.frame(Duration = pwrCalDf()$Time[4]))))
     
     x<- tibble(Athlete.power = c(onek,twok,threek,fourk))
     
   })
    
     
  ## Plot  
   output$modelPlot <- renderPlot({
     
     plot(mPower ~ Duration, data = modelDf())
     curve(predict(model(), newdata = data.frame(Duration = x)), col = "red", lty = c(1), add = TRUE)
     legend("topright",legend = paste("R2 is", format(summary(fit1())$r.squared)))
     
   })
   
## Power Calculator -----
   
   fDrag <- reactive({
     x <- 0.5*(input$cda * (Velocity()^2) * input$airDen)
   }) 
   
   Frr <- reactive ({
     x <- 9.8067*cos(tan(0/100))*(input$Fw*0.0025)
   })
   
   Fg <- reactive ({
     x <- 9.08067*sin(tan(0/100))*input$Fw
   })
   
   Velocity <- reactive ({
     x <- 250/input$lap
   })
   
   avgPower <- reactive({
     x <- ((1-(3/100))^-1 * (Fg()+Frr()+fDrag())  *  Velocity())
     
   })
   
   
   endFirst <- reactive({
     x <- (1417*(x1.0())^-1.044) * 0.983
   })
   
   x1.0 <- reactive({
     x <- (input$halfLapTar + (input$lap/2) * input$coef)
   })
   
   x0.25 <- reactive({
     x <- (x1.0() *0.396)
   })
   
   x0.75 <- reactive({
     x <- (((((x1.0() - input$halfLapTar)/2)+(input$halfLapTar - x0.25()))/2)+input$halfLapTar)*0.99
   })
   
   
   
## Power prediction df 
   
   pwrCalDf <- reactive({
     
     n <- (Velocity()*60*60)/1000
     
     xy <- 1 / (endFirst()/n)
     
     
     x1.25 <- x1.0() + (( (input$lap/2) /2) * xy)
     x1.75 <- x1.25 + (input$lap/2)
     x2.25 <- x1.75 + (input$lap/2)
     x2.75 <- x2.25 + (input$lap/2)
     x3.25 <- x2.75 + (input$lap/2)
     x3.75 <- x3.25 + (input$lap/2)
     x4.25 <- x3.75 + (input$lap/2)
     x4.75 <- x4.25 + (input$lap/2)
     x5.25 <- x4.75 + (input$lap/2)
     x5.75 <- x5.25 + (input$lap/2)
     x6.25 <- x5.75 + (input$lap/2)
     x6.75 <- x6.25 + (input$lap/2)
     x7.25 <- x6.75 + (input$lap/2)
     x7.75 <- x7.25 + (input$lap/2)
     x8.25 <- x7.75 + (input$lap/2)
     x8.75 <- x8.25 + (input$lap/2)
     x9.25 <- x8.75 + (input$lap/2)
     x9.75 <- x9.25 + (input$lap/2)
     x10.25 <- x9.75 + (input$lap/2)
     x10.75 <- x10.25 + (input$lap/2)
     x11.25 <- x10.75 + (input$lap/2)
     x11.75 <- x11.25 + (input$lap/2)
     x12.25 <- x11.75 + (input$lap/2)
     x12.75 <- x12.25 + (input$lap/2)
     x13.25 <- x12.75 + (input$lap/2)
     x13.75 <- x13.25 + (input$lap/2)
     x14.25 <- x13.75 + (input$lap/2)
     x14.75 <- x14.25 + (input$lap/2)
     x15.25 <- x14.75 + (input$lap/2)
     x15.75 <- x15.25 + (input$lap/2)
     
     ## Kilo times 
     kiloTime <- x3.75 + ((input$lap/2)/2)
     twokTime <- x7.25 + ((input$lap/2) * 1.5) 
     threekTime <- x11.25 + ((input$lap/2) * 1.5) 
     fourkTime <- x15.25 + ((input$lap/2) * 1.5) 
     
     ## power classifications for each lap 
     
     y1 <- ((twokTime - x7.75)/twokTime) * avgPower()
     
     p0.5 <- (input$halfLapTar / twokTime) *input$startPwr
     p1.0 <- ((x1.0() - input$halfLapTar)/twokTime)*input$startPwr
     p1.25 <- ((x1.25 - x1.0())/ twokTime) * avgPower()
     p1.75 <- ((x1.75 - x1.25)/twokTime) * avgPower()
     p2.25 <- ((x2.25 - x1.75)/twokTime) * avgPower()
     p2.75 <- ((x2.75 - x2.25)/twokTime) * avgPower()
     p3.25 <- ((x3.25 - x2.75)/twokTime) * avgPower()
     p3.75 <- ((x3.75 - x3.25)/twokTime) * avgPower()
     p4.25 <- ((x4.25 - x3.75)/twokTime) * avgPower()
     p4.75 <- ((x4.75 - x4.25)/twokTime) * avgPower()
     p5.25 <- ((x5.25 - x4.75)/twokTime) * avgPower()
     p5.75 <- ((x5.75 - x5.25)/twokTime) * avgPower()
     p6.25 <- ((x6.25 - x5.75)/twokTime) * avgPower()
     p6.75 <- ((x6.75 - x6.25)/twokTime) * avgPower()
     p7.25 <- ((x7.25 - x6.75)/twokTime) * avgPower()
     p7.75 <- ((x7.75 - x7.25)/twokTime) * avgPower()
     
     p10.75 <- ((x10.75 - x10.25)/threekTime) * avgPower()
     p11.75 <- ((x11.75 - x11.25)/threekTime) * avgPower()
     p15.75 <- ((x15.75 - x15.25)/fourkTime) * avgPower()
     
     k1pwr <- ((x2.75 - x2.25)/kiloTime) * avgPower()
     
     kiloPwr <- (k1pwr*6)+((x1.0()/kiloTime)* input$startPwr)
     
     twokPwr <- (p0.5+p1.0+p1.25+p1.75+p2.25+p2.75+p3.25+ 
                   p3.75+p4.25+p4.75+p5.25+p5.75+p6.25+p6.75+p7.25+p7.75+y1)
     
     threekPwr <- (p11.75*22)+((x1.0()/threekTime)* input$startPwr)
     
     fourkPwr <- (p15.75*30)+((x1.0()/fourkTime)* input$startPwr)
     
     ## compile to df 
     
     df <- tibble(Distance = c(1000,2000,3000,4000),
                  Time = c(kiloTime,twokTime,threekTime,fourkTime),
                  Power = c(kiloPwr,twokPwr,threekPwr,fourkPwr))
     
     df
  
})
   
# combine prediction times and calculated times for visual plot -----
   
 finalDf <- reactive({
   
   x<- bind_cols(pwrCalDf(),predictedPower())%>%
       mutate(Athlete.power = Athlete.power * input$crrF,
              'Difference (w)'= round(Athlete.power - Power))
   x$Power <- round(x$Power)

   x
   
 })
   


## Data table for predicted power and times  ------

   output$testdf <- renderReactable({
     formatDf()%>%
       reactable(defaultColDef = colDef(
         header = function(value) gsub(".", " ", value, fixed = TRUE),
         cell = function(value) format(value, nsmall = 1),
         align = "center",
         headerStyle = list(background = "black", color = 'white', fontweight = 'bold')),
         columns = list('Difference (w)'= colDef(
           style = function(value){
             if (value > 0) {
               color <- "#008000"
             } else if (value < 0) {
               color <- "#e00000"
             } else {
               color <- "#777"
             }
             list(color = color, fontWeight = "bold")
           }
          )),
         bordered = TRUE,
         highlight = TRUE,
         showSortIcon = FALSE,
         pagination = FALSE,
         compact = FALSE,
         fullWidth = TRUE)
   })

## Plot for power calculator ----- *********************************************************************

output$calcPlot <- renderPlotly({
  
  x <- data.frame(Time = finalDf()$Time,
                  Distance = finalDf()$Distance,
                  Power = finalDf()$Power,
                  Athlete.power = finalDf()$Athlete.power)
  
  
    plot_ly(x, x = ~Distance, type = "scatter", y = ~Power, name = "Estimated required power", mode = "lines+markers", line = list(shape = "spline"), 
            hoverinfo = 'text', text = ~paste('<b>Estimated required Power (w):</b>', Power), connectgaps = TRUE)%>%
      
    add_trace(y = ~Athlete.power, name = "Average best MMP", mode ="lines+markers", line = list(shape = "spline"), 
              hoverinfo = 'text', text = ~paste('<b>Athlete power (w):</b>', Athlete.power), connectgaps = TRUE)%>%
      
    layout(xaxis = list(title = "<b> Distance (m) </b>"), yaxis = list(title = '<b>Power (W)</b>'))
  
  
})

## formatted df for data table power calculator ----
   
   formatDf <- reactive({
     
     x<- finalDf()
     x$Time <- mmss_format(x$Time)
     x$Athlete.power <- round(x$Athlete.power)
     
     x <- as.data.frame(x)
     
     
   })
   
     
## energetics profile *** not in use -----
   
   energyDf <- reactive({

     
     power <- finalDf()%>% filter(Distance == input$calDura)%>%
               select(Predicted_power)
     
     duration <- finalDf()%>%filter(Distance == input$calDura)%>%
       select(Time)
     
     totalWk <- round((duration *power)/1000,2)
     wPrime_percentage <- round((cpDf()$`W'`/1000)/totalWk, 2)
     wPrime <- round(totalWk * wPrime_percentage)
     aerobic1 <- round(totalWk - wPrime)
     
     x <- data.frame(aerobic = aerobic1,
                     anaerobic = wPrime,
                     energy = '')
     
   })
   
   ## energetics plot *** not in use -------
   
   output$workDone <- renderPlotly({
     energyDf()%>%
       plot_ly(x = ~energy, y=~ aerobic,name = "aerobic", type = 'bar')%>%
       add_trace(y = ~anaerobic, name = "anaerobic" )%>%
       layout(yaxis = list(title = "Work (kj)"), barmode = 'stack', xaxis = list(title = ""))
     
   })
   
   
## CP model calculator -----
   
   ## CP data frame 
   
   cpDf <- reactive({
     
     x <- cpdata()
     
     CP <- critical_power(
       .data = x, 
       power_output_column = "PO", 
       time_to_exhaustion_column = "TTE", 
       method = c("3-hyp", "2-hyp", "linear", "1/time"),
       plot = FALSE, 
       all_combinations = FALSE,
       reverse_y_axis = FALSE)
     
     y <- filter(CP,method %in% input$cpModel)
     
     y
     
   })
 
   
   ## df for cp rhandsometable 
   
   cpdata <- reactive ({
     
   x <-pp_Data()%>%
     filter(Athlete %in% input$athletePred)%>%
     filter(Date >= as.Date(Sys.Date()-180) & Date <= as.Date(Sys.Date()))%>%
     filter(Duration %in% c(60,120,240,600,1200))%>%
     group_by(Duration)%>%
     summarise(Power = max(Power))%>%
     select(Power, Duration)
   
   x <- x%>%  
     rename(PO = Power, TTE = Duration)
   
   })
   
   
## Compare TP analysis data frame -----
  analyse_df_tp <- reactive({   
    
    df1 <- tp_Data()%>%
      filter(Date == input$tpAnalyseDate & Effort.type %in% input$tpCompareEffortType & 
               Men.Women %in% input$tpMenWomen & Session %in% input$TPtrainCompFilter)%>%
      
      {if (input$TPtrainCompFilter %in% "Training") filter(.,Training.effort == input$TrainTpCompareEffort) else 
        filter(.,Comp.type %in% input$CompTpCompare & Team %in% input$teamCompTP) }
    
    
    df2 <- tp_Data()%>%
      filter(Date == input$tpAnalyseDate1 & Effort.type %in% input$tpCompareEffortType1 & 
               Men.Women %in% input$tpMenWomen1 & Session %in% input$TPtrainCompFilter1)%>%
      
      {if (input$TPtrainCompFilter1 %in% "Training") filter(.,Training.effort == input$TrainTpCompareEffort1) else 
        filter(.,Comp.type %in% input$CompTpCompare1 & Team %in% input$teamCompTP1) }

    
    df1new <- as.data.frame(df1)
    
    df2new <- as.data.frame(df2)
    
    df1new[is.na(df1new)] <- ""
    df2new[is.na(df2new)] <- ""
    
    Df <- df1new%>%
      mutate(item = paste(Date, Session, Event ,Comp.type, Training.effort, Team ,sep = " "))
    
    Df2 <- df2new%>%
      mutate(item = paste(Date, Session, Event ,Comp.type, Training.effort, Team ,sep = " "))
    
    new_df <- rbind(Df, Df2)
    
    
    new_df <- new_df %>%
      arrange(lap.splits)
    
    
    new_df
    
  })
  
## Compare TP analyses plot ------
  
  output$tpPlotAnalyses <- renderPlotly({
    
    analyse_df_tp()%>%
      plot_ly(x = ~lap.splits, y=~Actual.Schedule, group = ~item, type = 'scatter', 
              color = ~item, mode = "lines+markers", line = list(shape = "spline"), text =~Athlete)%>%
      layout(yaxis = list(autorange = 'reversed', title = "Time (Seconds)"),
             xaxis = list(title = "Lap Splits", type = 'category'),
             legend = list(legend = 1, orientation = 'h', xanchor = 'center', x = 0.5, y = 1.0))
    
    
  })
  
## competition list TP ------
  
  observe({
    
    x <- tp_Data()|>
      filter(Session %in% "Competition")%>%
      mutate(eventList = paste(Event, year(Date),sep = " "))|>
      distinct(eventList)
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "tpCompEventFilter",
                      choices = x,
                      selected = head(x,1))
  })
  
## competition list IP ------
  
  observe({
    
    x <- ip_Data()|>
      filter(Session %in% "Competition")%>%
      mutate(EventList = paste(Event, year(Date) ,sep = " "))|>
      distinct(EventList)
    
    if(is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId = "ipCompEventFilter",
                      choices = x,
                      selected = head(x,1))
  })
  
  
  
  
  
  
## PDF report download TP Training ------
  
  output$tpTrainreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data_table_df = tp_data_new_t(),
                     notes = pasteNotesTpTrain)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
