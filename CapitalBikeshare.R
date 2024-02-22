########## Capital Bikeshare Project #############

#### Preparation ########################
#load packages
library(readr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(DT)

#set a work directory
getwd()
setwd("/Volumes/GoogleDrive/Shared drives/LC/Advanced/Data")

#### Data importing ########################
#import the data
oct <- read.csv("202110-capitalbikeshare-tripdata.csv")
sep <- read.csv("202109-capitalbikeshare-tripdata.csv")
aug <- read.csv("202108-capitalbikeshare-tripdata.csv")
jul <- read.csv("202107-capitalbikeshare-tripdata.csv")
jun <- read.csv("202106-capitalbikeshare-tripdata.csv")
may <- read.csv("202105-capitalbikeshare-tripdata.csv")
apr <- read.csv("202104-capitalbikeshare-tripdata.csv")
mar <- read.csv("202103-capitalbikeshare-tripdata.csv")
feb <- read.csv("202102-capitalbikeshare-tripdata.csv")
jan <- read.csv("202101-capitalbikeshare-tripdata.csv")
dec <- read.csv("202012-capitalbikeshare-tripdata.csv")
nov <- read.csv("202011-capitalbikeshare-tripdata.csv")

#combine data into three months
novDecJan <- rbind(nov, dec, jan)
febMarApr <- rbind(feb, mar, apr)
mayJunJul <- rbind(may, jun, jul)
augSepOct <- rbind(aug, sep, oct)

#check if the number of row in combine data is the same as that of orginal monthly data
nrow(nov) + nrow(dec) + nrow(jan) #381334
nrow(novDecJan) #381334

nrow(feb) + nrow(mar) + nrow(apr) #465433
nrow(febMarApr) #465433

nrow(may) + nrow(jun) + nrow(jul) #841983
nrow(mayJunJul) #841983

nrow(aug) + nrow(sep) + nrow(oct) #909491
nrow(augSepOct) #909491

#### Obtain 12,000 samples  ########################
#take the sample; 3,000 for each three-month data (12,000 in total)
set.seed(1)
novDecJan.s <- sample_n(novDecJan, 3000)

set.seed(1)
febMarApr.s <- sample_n(febMarApr, 3000)

set.seed(1)
mayJunJul.s <- sample_n(mayJunJul, 3000)

set.seed(1)
augSepOct.s <- sample_n(augSepOct, 3000)

#combine sample data
bike.df <- rbind(novDecJan.s, febMarApr.s, mayJunJul.s, augSepOct.s)

#check the combined data
nrow(bike.df) #12000
summary(bike.df)
str(bike.df)

#### Generate some variables and adjust type/format of data  ########################
#change the membership variable to a factor
bike.df$member_casual <- factor(bike.df$member_casual, levels = c("member", "casual"))

#change the bike type variable to a factor
bike.df$rideable_type <- factor(bike.df$rideable_type, 
                                levels = c("classic_bike", "electric_bike", "docked_bike"))

#change the format of date/time(hr + min) variable
bike.df$started_at <- ymd_hms(bike.df$started_at)
bike.df$ended_at <- ymd_hms(bike.df$ended_at)

#create monthly variable
bike.df$month_start <- month(bike.df$started_at, label = TRUE, abbr = TRUE)

#change month variable to factor
bike.df$month_start <- factor(bike.df$month_start, 
                              levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                         "Sep", "Oct", "Nov", "Dec"))

#create the day of the week variables
bike.df$day_of_week_start <- weekdays(bike.df$started_at, abbreviate = FALSE)

#change the day of week variable to factor
bike.df$day_of_week_start <- factor(bike.df$day_of_week_start, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
                                               "Saturday", "Sunday"))

#create duration variable (sec)
bike.df$duration <- bike.df$ended_at - bike.df$started_at

#change the unit of from sec to mins
bike.df$duration <- (bike.df$ended_at - bike.df$started_at)/60

#change duration variable to numeric
bike.df$duration <- as.numeric(bike.df$duration)

summary(bike.df$duration)

#### Dashboard ########################

#create data set having the name of station (excluding rows whose station name is NA)
bike.df.station <- bike.df[bike.df$start_station_name != "",]

####UI
ui <- dashboardPage(skin = "red",
                    
                    # Application title
                    dashboardHeader(title = "The Capital Bikeshare"
                    ),
                    
                    # Sidebar
                    dashboardSidebar(
                        #Select date range
                        dateRangeInput("date_select", strong("Select date range:"), start = "2020-11-01", 
                                       end = "2021-10-31", min = "2020-11-01", max =  "2021-10-31"),
                        #Select membership type
                        checkboxGroupInput("member_select", "Select membership type:", 
                                           choices = unique(bike.df$member_casual), selected = c("member", "casual")),
                        #Select bike type
                        checkboxGroupInput("bike_select", "Select bike type:", 
                                           choices = unique(bike.df$rideable_type), 
                                           selected = c("classic_bike", "electric_bike", "docked_bike"))
                        #actionButton("click", "Show Result")
                    ),
                    
                    # Add tabs and plots in UI
                    dashboardBody(
                        tabsetPanel(
                            #Tab1: Duration
                            tabPanel("Duration",
                                     box(title = "The Duration of Rides",
                                         status = "danger", 
                                         solidHeader = TRUE,
                                         plotOutput("Plot1", height = 500))),
                            #Tab2: Rentals: three plots
                            tabPanel("Rentals",
                                     fluidRow(
                                         box(title = "The Number of Rentals",
                                             status = "danger", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             plotly::plotlyOutput("Plot2", height = 250)),
                                         box(title = "The Number of Rentals by Day of Week", 
                                             status = "danger", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             plotly::plotlyOutput("Plot3", height = 250)
                                         )),
                                     fluidRow(
                                         box(title = "The Number of Rentals by Month", 
                                             status = "danger", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             plotly::plotlyOutput("Plot4", height = 250)))),
                            #Tab3: Station (The ranking of the most frequently used stations)
                            tabPanel("Station", 
                                     fluidRow(
                                         box(
                                             title = "The Popular Station", 
                                             status = "danger", 
                                             solidHeader = TRUE,
                                             DT::DTOutput("Plot5"))))
                        )
                    )
)
####Server
server <- function(input, output) {
    
    #Filters (1) for plot 1 - 4: date range, membership type and bike type
    selected_trips <- reactive({
        
        #make sure that the choice of begin and end dates is logical 
        req(input$date_select)
        validate(need(!is.na(input$date_select[1]) & !is.na(input$date_select[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date_select[1] < input$date_select[2], "Error: Start date should be earlier than end date."))     
        
        #Select the right data to perform the plots
        df1=bike.df[(bike.df$started_at>=as.POSIXct(input$date_select[1])) & 
                        (bike.df$started_at<=as.POSIXct(input$date_select[2])) &
                        (bike.df$member_casual %in% input$member_select) &
                        (bike.df$rideable_type %in% input$bike_select),] 
    })
    
    
    #Filters (2) for plot 5: date range, membership type and bike type 
    #(bike.df contains many NAs in a station name variable, 
    #which leads to a blank space in the 1st place on the table (Plot 5) showing stations in descending order of frequency.
    #Hence we linked plot 5 with new data (bike.df.station) which does not include NAs in the station variable.)
    selected_trips_for_plot5 <- reactive({
        
        #make sure that the choice of begin and end dates is logical 
        req(input$date_select)
        validate(need(!is.na(input$date_select[1]) & !is.na(input$date_select[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date_select[1] < input$date_select[2], "Error: Start date should be earlier than end date."))
        
        #Select the right data to perform the plots
        df2=bike.df.station[(bike.df.station$started_at>=as.POSIXct(input$date_select[1])) & 
                                (bike.df.station$started_at<=as.POSIXct(input$date_select[2])) &
                                (bike.df.station$member_casual %in% input$member_select) &
                                (bike.df.station$rideable_type %in% input$bike_select),] 
    })
    
    #Plot1: The total duration of rides
    output$Plot1 <- renderPlot({
        mainplot1 <- ggplot(data = selected_trips(), aes(x=month_start, y=duration)) +
            geom_jitter(alpha = .5, color = "orange") +
            labs(x = "Month", y = "", color = "Type of Bikes") +
            facet_grid(rideable_type~ member_casual) +
            ylim(0,1500) +
            theme(axis.text.x = element_text(angle = 90))
        print(mainplot1)
    })
    #Plot2: The total bike rentals
    output$Plot2 <- plotly::renderPlotly({
        #set color
        palette <- c(member = "orange", casual = "seagreen3")
        #plot
        mainplot2 <- ggplot(data = selected_trips(), aes(member_casual, fill = member_casual)) +
            geom_bar(position = "dodge",width = 0.5) +
            labs(x= "Membership", y = "") +
            theme(legend.position = "none") +
            scale_fill_manual(values = palette)
        print(mainplot2)
    })
    #Plot3: The total bike rentals by day of week
    output$Plot3 <- plotly::renderPlotly({
        mainplot3 <- ggplot(data = selected_trips(), aes(day_of_week_start)) +
            geom_bar(fill = "seagreen3") +
            labs(x= "", y = "") +
            theme(legend.position = "none")+
            theme(axis.text.x = element_text(angle = 90))
        print(mainplot3) 
    })
    #Plot4: The total bike rentals by month
    output$Plot4 <- plotly::renderPlotly({
        #set color
        palette <- c(member = "orange", casual = "seagreen3")
        #plot
        mainplot4 <- ggplot(data = selected_trips(), aes(month_start, fill = member_casual)) +
            geom_bar(position = "dodge",width = 0.5) +
            scale_fill_manual(values = palette) +
            labs(x= "", y = "", fill = "Membership") +
            theme(axis.text.x = element_text(angle = 90))
        print(mainplot4) 
    })
    #Plot5: The ranking of the most frequently used stations
    output$Plot5 <- DT::renderDT({
        data = selected_trips_for_plot5() %>%
            count(start_station_name, name = "n_station") %>%
            arrange(desc(n_station))
    })
}

#Run the application 
shinyApp(ui = ui, server = server)