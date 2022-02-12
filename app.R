#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

require(scales)
options(scipen=10000)


temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, read.csv)
allData <- do.call(rbind, allData2)

allData$rides <- as.numeric(gsub(",", "", as.character(allData$rides)))

# allData$rides <- as.numeric(as.character(allData$rides))



lubridateDate <- mdy(allData$date)
allData$date <- lubridateDate
allData$month <- month(lubridateDate)
allData$day <- day(lubridateDate)
allData$year <- year(lubridateDate)

allData$weekday <- weekdays(allData$date)

dfUICHalsted <- subset(allData, stationname == "UIC-Halsted")
dfOhare <- subset(allData, stationname == "O'Hare Airport")
dfDempster <- subset(allData, stationname == "Dempster")

yearList = c(2001:2021)
df1 <- data.frame(
  Year = yearList,
  Entries = c(0)
)
df2 <- data.frame(
  Year = yearList,
  Entries = c(0)
)
dk1 <- data.frame(
  Year = yearList,
  Entries = c(0)
)

m1 = 1
for(i in yearList) {
  dfUICHalstedSubset <- subset(dfUICHalsted, year == i)
  #sum(dfUICHalsted$rides)
  sumEntries <- sum(dfUICHalstedSubset$rides)
  df1[m1,2] = sumEntries
  #print(df1[m,2])
  m1=m1+1
}

pages <- c("Home","About Page")

m2 = 1
for(i in yearList) {
  dfOharesub <- subset(dfOhare, year == i)
  #sum(dfUICHalsted$rides)
  sumEntries <- sum(dfOharesub$rides)
  df2[m2,2] = sumEntries
  #print(df1[m,2])
  m2=m2+1
}

d1 = 1
for(i in yearList) {
  dfDempstersub <- subset(dfDempster, year == i)
  #sum(dfUICHalsted$rides)
  sumEntries <- sum(dfDempstersub$rides)
  dk1[d1,2] = sumEntries
  #print(df1[m,2])
  d1=d1+1
}

months <- month.abb
months_no <- c(1:12)
weekday_list <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")



ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 1"),
  # dashboardSidebar(),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   selectInput("page1", "Select the page", pages, selected = "Home")
                   #selectInput("Room7", "Select the room to visualize", listNamesGood, selected = "Machine Room")
                   
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.page1 == 'Home'",
      
    fluidRow(
    column(12,
           h1("CTA Entries comparison between Stations from 2001-2021"),
           )
    ),
    fluidRow(
      column(6,
             h2("Station 1"),
      ),
      column(6,
             h2("Station 2"),
      )
    ),
    fluidRow(
      column(2,
          selectInput("chart1", h3("Select Chart type"), 
                         choices = list("Bar chart" = 1, "Table" = 2), selected = 1)
      ),
      column(2,
             selectInput("Year1", h3("Select Year"), 
                         choices = list("2001" = 2001,
                                        "2002"= 2002,
                                        "2003" = 2003,
                                        "2004" = 2004,
                                        "2005" = 2005,
                                        "2006" = 2006,
                                        "2007" = 2007,
                                        "2008" = 2008,
                                        "2009"= 2009,
                                        "2010" = 2010,
                                        "2011" = 2011,
                                        "2012" = 2012,
                                        "2013"= 2013,
                                        "2014" = 2014,
                                        "2015" = 2015,
                                        "2016" = 2016,
                                        "2017" = 2017,
                                        "2018" = 2018,
                                        "2019" = 2019,
                                        "2020" = 2020,
                                        "2021" = 2021), selected = 2021)
             ),
      column(2,
             selectInput("select1", h3("Select Station 1"), 
                         choices = list("UIC-Halsted" = 1, "O'hare Airport" = 2,
                                        "Dempster" = 3), selected = 1)
      ),
      column(2,
             selectInput("chart2", h3("Select Chart type"), 
                         choices = list("Bar chart" = 1, "Table" = 2), selected = 1)
      ),
      column(2,
             selectInput("Year2", h3("Select Year"),
                         choices = list("2001" = 2001, 
                                        "2002"= 2002,
                                        "2003" = 2003,
                                        "2004" = 2004,
                                        "2005" = 2005,
                                        "2006" = 2006,
                                        "2007" = 2007,
                                        "2008" = 2008,
                                        "2009"= 2009,
                                        "2010" = 2010,
                                        "2011" = 2011,
                                        "2012" = 2012,
                                        "2013"= 2013,
                                        "2014" = 2014,
                                        "2015" = 2015,
                                        "2016" = 2016,
                                        "2017" = 2017,
                                        "2018" = 2018,
                                        "2019" = 2019,
                                        "2020" = 2020,
                                        "2021" = 2021), selected = 2021)
      ),
      column(2,
             selectInput("select2", h3("Select Station 2"),
                         choices = list("UIC-Halsted" = 1, "O'hare Airport" = 2,
                                        "Dempster" = 3), selected = 2)
      ),
    ),
    fluidRow(
      column(3,
             fluidRow(
               box(title = "Entries from 2001-2021 for Station 1", solidHeader = TRUE, status = "primary", width = 12,
                   conditionalPanel(
                     condition = "input.chart1 == '1'",
                     plotOutput("hist1", height=400)
               )
               , conditionalPanel(
                 condition = "input.chart1 == '2'",
                 DTOutput("tb1", height=400)
               )
              )
             ),
             fluidRow(
              box(title = "Entries for Months for Station 1", solidHeader = TRUE, status = "primary", width = 12,
                   conditionalPanel(
                     condition = "input.chart1 == '1'",
                     plotOutput("hist2", height = 400)
                   )
                   , conditionalPanel(
                     condition = "input.chart1 == '2'",
                     DTOutput("tb2", height = 400)
                   )
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "Entries for Weekdays for Station 1", solidHeader = TRUE, status = "primary", width = 12,
                   conditionalPanel(
                     condition = "input.chart1 == '1'",
                     plotOutput("hist3", height = 400)
                   )
                   , conditionalPanel(
                     condition = "input.chart1 == '2'",
                     DTOutput("tb3", height = 400)
                   )
               )
             ),
             fluidRow(
               box(title = "Entries throughout an Year for Station 1", solidHeader = TRUE, status = "primary", width = 12,
                   conditionalPanel(
                     condition = "input.chart1 == '1'",
                     plotOutput("hist4", height = 400)
                   )
                   , conditionalPanel(
                     condition = "input.chart1 == '2'",
                     DTOutput("tb4", height = 400)
                   )
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "Entries from 2001-2021 for Station 2", solidHeader = TRUE, status = "danger", width = 12,
                   conditionalPanel(
                     condition = "input.chart2 == '1'",
                     plotOutput("hist5", height=400)
                   )
                   , conditionalPanel(
                     condition = "input.chart2 == '2'",
                     DTOutput("tb5", height=400)
                   )
               )
             ),
             fluidRow(
               box(title = "Entries for Months for Station 2", solidHeader = TRUE, status = "danger", width = 12,
                   conditionalPanel(
                     condition = "input.chart2 == '1'",
                     plotOutput("hist6", height=400)
                   )
                   , conditionalPanel(
                     condition = "input.chart2 == '2'",
                     DTOutput("tb6", height=400)
                   )
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "Entries for Weekdays for Station 2", solidHeader = TRUE, status = "danger", width = 12,
                   conditionalPanel(
                     condition = "input.chart2 == '1'",
                     plotOutput("hist7", height=400)
                   )
                   , conditionalPanel(
                     condition = "input.chart2 == '2'",
                     DTOutput("tb7", height=400)
                   )
               )
             ),
             fluidRow(
               box(title = "Entries throughout an Year for Station 2", solidHeader = TRUE, status = "danger", width = 12,
                   conditionalPanel(
                     condition = "input.chart2 == '1'",
                     plotOutput("hist8", height=400)
                   )
                   , conditionalPanel(
                     condition = "input.chart2 == '2'",
                     DTOutput("tb8", height=400)
                   )
               )
             ),
      ),
    ),
  ),
    conditionalPanel(
      condition = "input.page1 == 'About Page'",
      column(12,
             h3("The data is from City of Chicago https://www.evl.uic.edu/aej/424/22Sproject1.html#:~:text=https%3A//data.cityofchicago.org/Transportation/CTA%2DRidership%2DL%2DStation%2DEntries%2DDaily%2DTotals/5neh%2D572f
                and the app is made by Soel Mullenkuzhiyil Sunny last updated on 02/12/2022 and was made to compare the CTA entries between O'hare Airport, UIC-Halsted and Dempster stops.")
      )
    ),
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  justOneYearReactive1 <- reactive({
    nameOfPlace = "UIC-Halsted"
    if(input$select1 == 1){
      nameOfPlace = "UIC-Halsted"
    } else if(input$select1 == 2) {
      nameOfPlace = "O'Hare Airport"
    } else {
      nameOfPlace = "Dempster"
    }
   # print(nameOfPlace)
    subset(allData, allData$year == input$Year1 & allData$stationname == nameOfPlace)
    })
  justOneYearReactive2 <- reactive({
    nameOfPlace = "O'Hare Airport"
    if(input$select2 == 2){
      nameOfPlace = "O'Hare Airport"
    } else if(input$select2 == 2) {
      nameOfPlace = "UIC-Halsted"
    } else {
      nameOfPlace = "Dempster"
    }
   # print(nameOfPlace)
    subset(allData, allData$year == input$Year2 & allData$stationname == nameOfPlace)
    })
  
  
  
    output$hist1 <- renderPlot({
      #newYears <-  justOneYearReactive()
      if(input$select1 == 1) {
          ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
      } else if(input$select1 == 2) {
          ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in O'Hare Airport from 2001-2021")+scale_y_continuous(labels=comma)
      } else {
        ggplot(dk1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in Dempster from 2001-2021")+scale_y_continuous(labels=comma)
        }
      })
    
    output$tb1 = renderDT(
      df1, options  = list(lengthMenu = c(7,7))
    )
    
    
    
    output$hist2 <- renderPlot({
      ny1 <- justOneYearReactive1()
      
      
      df3 <- data.frame(
        Months = months,
        Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      
      
        m3 = 1
        for(i in months_no) {
          ny1Subset <- subset(ny1, month == i)
          #print(ny1Subset)
          #sum(dfUICHalsted$rides)
          sumEntries <- sum(ny1Subset$rides)
          #print(sumEntries)
          df3[m3,3] = sumEntries
          #print(df1[m,2])
          m3=m3+1
        }
        titlePlot <- paste("Entries in Months for", ny1$stationname, "in", ny1$year, sep = " ")
        ggplot(df3, aes(x=Months, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Months", title=titlePlot)+scale_y_continuous(labels=comma)
        
        
    })
    
    output$tb2 = renderDT({
      ny1 <- justOneYearReactive1()
      
      
      df3 <- data.frame(
        Months = months,
        #Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      
      
      m3 = 1
      for(i in months_no) {
        ny1Subset <- subset(ny1, month == i)
        #print(ny1Subset)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(ny1Subset$rides)
        #print(sumEntries)
        df3[m3,2] = sumEntries
        #print(df1[m,2])
        m3=m3+1
      }
      datatable(df3,options  = list(lengthMenu = c(7,7)))
      #df3 options  = list(lengthMenu = c(6,6))
    })
    
    output$hist3 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive1()

      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }
      titlePlot <- paste("Entries in Weekdays for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(df4, aes(x=Weekday, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Weekdays", title=titlePlot)+scale_y_continuous(labels=comma)
      
      #ny1 <- justOneYearReactive1()
     # ggplot(ny1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb3 = renderDT({
      ny1 <- justOneYearReactive1()
      
      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }
      #df4 
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
    })
    
    output$hist4 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive1()
      
      
      titlePlot <- paste("Entries in throughout the year for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(ny1, aes(x=date, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Throughout Year", title=titlePlot)+scale_y_continuous(labels=comma)
    })
    
    output$tb4 = renderDT({
      ny1 <- justOneYearReactive1()
      
      df4 <- data.frame(
        Day = ny1$date,
        Entries = ny1$rides
      )
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
      
    })
    
    output$hist5 <- renderPlot({
      #newYears <-  justOneYearReactive()
      #ny2 <- justOneYearReactive2()
      if(input$select2 == 2) {
        ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Year", title="Entries in O'Hare Airport from 2001-2021")+scale_y_continuous(labels=comma)
      } else if(input$select2 == 1) {
        ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
      } else {
        ggplot(dk1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Year", title="Entries in Dempster from 2001-2021")+scale_y_continuous(labels=comma)
      }
      #ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb5 = renderDT(
      df2,  options  = list(lengthMenu = c(7,7))
    )
    
    
    output$hist6 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive2()
      
      
      df3 <- data.frame(
        Months = months,
        Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      m3 = 1
      for(i in months_no) {
        ny1Subset <- subset(ny1, month == i)
        #print(ny1Subset)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(ny1Subset$rides)
        #print(sumEntries)
        df3[m3,3] = sumEntries
        #print(df1[m,2])
        m3=m3+1
      }
      
      titlePlot <- paste("Entries in Months for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(df3, aes(x=Months, y=Entries))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Months", title=titlePlot)+scale_y_continuous(labels=comma)
        
      #ny2 <- justOneYearReactive2()
      #ggplot(ny2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb6 = renderDT({
      ny1 <- justOneYearReactive2()
      
      
      df3 <- data.frame(
        Months = months,
        #Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      
      
      m3 = 1
      for(i in months_no) {
        ny1Subset <- subset(ny1, month == i)
        #print(ny1Subset)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(ny1Subset$rides)
        #print(sumEntries)
        df3[m3,2] = sumEntries
        #print(df1[m,2])
        m3=m3+1
      }
      datatable(df3,options  = list(lengthMenu = c(7,7)))
      #df3 options  = list(lengthMenu = c(6,6))
    })

    output$hist7 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive2()
      
      
      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }

      titlePlot <- paste("Entries in Weekdays for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(df4, aes(x=Weekday, y=Entries))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Weekday", title=titlePlot)+scale_y_continuous(labels=comma)
        
      #ny2 <- justOneYearReactive2()
      #ggplot(ny2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb7 = renderDT({
      ny1 <- justOneYearReactive2()
      
      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }
      #df4 
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
    })

    output$hist8 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive2()
      
      titlePlot <- paste("Entries in throughout the year for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(ny1, aes(x=date, y=rides))+geom_bar(stat="identity", fill="#eb6363")+labs(y = "Total Entries", x="Throughout Year", title= titlePlot)+scale_y_continuous(labels=comma)
      
      #ny2 <- justOneYearReactive2()
      #ggplot(ny2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+ scale_y_continuous(limits=c(1100000,4553704),oob = rescale_none)
    })
    
    output$tb8 = renderDT({
      ny1 <- justOneYearReactive2()
      
      df4 <- data.frame(
        Day = ny1$date,
        Entries = ny1$rides
      )
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
