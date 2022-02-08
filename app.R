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

dfUICHalsted <- subset(allData, stationname == "UIC-Halsted")
dfOhare <- subset(allData, stationname == "O'Hare Airport")

yearList = c(2001:2021)
df1 <- data.frame(
  Year = yearList,
  Entries = c(0)
)
df2 <- data.frame(
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

m2 = 1
for(i in yearList) {
  dfOharesub <- subset(dfOhare, year == i)
  #sum(dfUICHalsted$rides)
  sumEntries <- sum(dfOharesub$rides)
  df2[m2,2] = sumEntries
  #print(df1[m,2])
  m2=m2+1
}


# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("hist1")
#         )
#     )
# )

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 1"),
  dashboardSidebar(),
  # dashboardSidebar(disable = FALSE, collapsed = FALSE,
  # 
  #                  sidebarMenu(
  #                    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
  #                    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
  #                    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
  #                    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
  #                    menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
  # 
  #                  selectInput("Year", "Select the year to visualize", years, selected = 2021),
  # ),
  # dashboardBody(),
  dashboardBody(
    fluidRow(
      column(3,
             selectInput("select", h3("Select box"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)
             ),
      column(3,
             selectInput("select", h3("Select box"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)
      ),
      column(3,
             selectInput("select", h3("Select box"),
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)
      ),
      column(3,
             selectInput("select", h3("Select box"),
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)
      ),
    ),
    fluidRow(
      column(3,
             fluidRow(
               box(title = "UIC-Halsted Entries from 2001-2021", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist1", height=400)
               )
             ),
             fluidRow(
               box(title = "UIC-Halsted each day for 2021 ", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist2", height=400)
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "UIC-Halsted for each month for 2021", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist3", height=400)
               )
             ),
             fluidRow(
               box(title = "UIC-Halsted for each day of the week for 2021", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist4", height=400)
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "O'hare Entries from 2001-2021", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist5", height=400)
               )
             ),
             fluidRow(
               box(title = "O'hare each day for 2021 ", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist6", height=400)
               )
             ),
      ),
      column(3,
             fluidRow(
               box(title = "O'hare for each month for 2021", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist7", height=400)
               )
             ),
             fluidRow(
               box(title = "O'hare each day of the week for 2021 ", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist8", height=400)
               )
             ),
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$hist1 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    
    output$hist2 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$hist3 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$hist4 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$hist5 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    
    output$hist6 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })

    output$hist7 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })

    output$hist8 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+ scale_y_continuous(limits=c(1100000,4553704),oob = rescale_none)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
