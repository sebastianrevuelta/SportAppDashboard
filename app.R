# 1. Load libraries ----
library("shinydashboard")
library("shinyjs")
library("shinyalert")

library("plyr")
library("dplyr")

library("lubridate")
library("forcats")
library("readr")
library("stringr")

library("methods")
library("DT")

library("ggplot2")

source('AlgorithmTrainings.R')
source('Constants.R')

dfTrainings <- read_rds("dfTrainings.rds")

#filter 1999 year
dfTrainings <- dfTrainings %>%
    dplyr::filter(dfTrainings$Year > 2000) 

sport_list <- as.list(sort(unique(dfTrainings$`Sport`)))
sport_list <- c(as.list("All sports"),sport_list)

period_list <- as.list(c("year","quarter","month","week"))


# 4. Shiny App Header ----
header <- dashboardHeader(title = "My Trainings",dropdownMenuOutput("alertMenu")) ##TODO ADD FROM ... TO...


# 5. Shiny App Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
     menuItem("Home", tabName = "Home"),
     menuItem("Evolution", tabName = "Evolution")
  )
)

# 6. Shiny App Body ----
body <- dashboardBody(
    useShinyalert(),
    fluidRow(
      shinydashboard::box(dateRangeInput('dateRange',label = 'Trainings Since', max = Sys.Date(), format = "dd/mm/yyyy"),width=3,
                          column(12, actionButton("Default_Dates", label = "All Dates", width = "100%")),
                          column(12, div(style="height:20px;")) #space <br>
                          ),
      shinydashboard::box(selectInput("sport_selection", "Sport:", sport_list), width=2),
      shinydashboard::box(selectInput("period_selection", "Period:", period_list), width=2)
    ),
   
  
    tabItems(
      tabItem(tabName = "Home",
              fluidRow(
                valueBoxOutput("NumberOfTrainings",width = 2),
                valueBoxOutput("NumberOfKms",width = 2),
                valueBoxOutput("TotalTime",width = 2),
                valueBoxOutput("AverageDistance",width = 2),
                valueBoxOutput("AverageTime",width = 2),
                valueBoxOutput("AveragePace",width = 2)),
              fluidRow(
                valueBoxOutput("Best10Km",width = 4),
                valueBoxOutput("BestHalfMarathon",width = 4),
                valueBoxOutput("BestMarathon",width = 4)),
                fluidRow(
                 h4(paste("My trainings")),
                 shinydashboard::box(DT::dataTableOutput('training_table'),width = 12),
                 shinydashboard::box(plotOutput("time_by_sport"),width = 12))
      ),
      tabItem(tabName = "Evolution",
              fluidRow(
                  shinydashboard::box(plotOutput("distance"),width = 12),
                  shinydashboard::box(plotOutput("time"),width = 12),
                  shinydashboard::box(plotOutput("pace"),width = 12),
                  shinydashboard::box(plotOutput("pace_weekday"),width = 6),
                  shinydashboard::box(plotOutput("distance_by_hours"),width = 6),
                  shinydashboard::box(plotOutput("trainings_by_hours"),width = 12),
                  shinydashboard::box(plotOutput("trainings_by_day"),width = 12)
              )
      )
  )
)

ui <- dashboardPage(header, sidebar, body)

# 7. Shiny App dynamic content ----
server <- function(input, output, session) {

    observeEvent(input$'Default_Dates', {
        updateDateRangeInput(session,'dateRange',label = 'Training Calendar',start=NA, end=NA)
    })

    observe({
      if(!is.na(input$dateRange[1]) && !is.na(input$dateRange[2])){
        if(input$dateRange[1] > input$dateRange[2]){
          shinyalert(
            title = "Bad date range",
            text = "The End date must be greater or equal than the Start date.",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
        }
       }

    # 1.1 Number of Trainings ----
    output$NumberOfTrainings <- renderValueBox({

        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }

        valueBox(
            nrow(dfTrainings), "Trainings", color = "blue"
        )
    })    

    # 1.2 NumberOfKms ----
    output$NumberOfKms <- renderValueBox({

        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        valueBox(
            sum(dfTrainings$Distance), "Total Distance (km)", color = "blue"
        )
    }) 
    
    # 1.3 Total Time ----
    output$TotalTime <- renderValueBox({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        s <- str_sub(as.character(seconds_to_period(sum(dfTrainings$Time))),0,10)
        
        valueBox(
            s, "Total Time", color = "blue"
        )
    }) 
    
    # 1.4 Average Kms ----
    output$AverageDistance <- renderValueBox({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
    
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        valueBox(
            round(mean(dfTrainings$Distance),digits = 2), "Average Km per training",
            color = "green"
        )
    })    
    
    # 1.5 AverageTime ----
    output$AverageTime <- renderValueBox({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        valueBox(
            seconds_to_period(round(mean(dfTrainings$Time),digits=0)),"Average time per training",
            color = "green"
        )
    })         
    
    # 1.6 Average Pace ----
    output$AveragePace <- renderValueBox({
        
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        valueBox(
            round(getTrainingPace(sum(dfTrainings$Time/60),sum(dfTrainings$Distance)),digits=2),"Average Pace per training",
            color = "green"
        )
    })    
    
    # 1.6.1 Best10Km ----
    output$Best10Km <- renderValueBox({
        
        df <- dfTrainings %>%
            filter(Distance >= 10 & Distance < 10.2) 
        
        time <- seconds_to_period(min(df$Time))
        
        if (is.null(time)) {
            time <- "--"
        }
        
        valueBox(
            time,"Best 10km time",
            color = "red"
        )
    }) 
    
    # 1.6.2 BestHalfMarathon ----
    output$BestHalfMarathon <- renderValueBox({
        
        df <- dfTrainings %>%
            filter(Distance >= 21 & Distance < 21.3) 
        
        time <- seconds_to_period(min(df$Time))
        
        if (is.null(time)) {
            time <- "--"
        }
        
        valueBox(
            time,"Best Half Marathon time",
            color = "red"
        )
    })
    
    # 1.6.3 BestMarathon ----
    output$BestMarathon <- renderValueBox({
        
        df <- dfTrainings %>%
            filter(Distance >= 42 & Distance < 42.4) 
        
        time <- seconds_to_period(min(df$Time))
        
        if (is.null(time)) {
            time <- "--"
        }
        
        valueBox(
            time,"Best marathon time",
            color = "red"
        )
    }) 
    
    # 1.7 Training Table ----
    output$training_table <- DT::renderDataTable({ 
    
    date1 <- input$dateRange[1]
    date2 <- input$dateRange[2]
    sport <- input$sport_selection
    
    if (sport != "All sports") {
        dfTrainings <- dfTrainings %>%
            filter(Sport == sport)
    }
    
    if(!is.na(date1) && !is.na(date2)){
        dfTrainings <- dfTrainings %>% 
            filter(Date >= date1 & Date <= date2)
    }
    
    dfTrainings <- dfTrainings[order(dfTrainings$"Date",decreasing = TRUE),]
    df <- dfTrainings %>% 
        mutate(Date = str_sub(Date,1,10)) %>%
        select("Sport","Date","Time","Distance","Elevation") %>%
        mutate(Time = round(Time/60,digits=2)) %>%
        rename("Time (minutes)" = "Time") %>%
        rename("Distance (km)" = "Distance")
    brks <- c(0,30,60)
    clrs <- c(white,yellow,green,red)
    datatable(df,rownames=FALSE,caption = "My trainings") %>% 
        formatStyle("Time (minutes)", backgroundColor = styleInterval(brks, clrs))
    
  })
    
    # 1.8 Time by sport ----
    output$time_by_sport <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        period <- input$period_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        df <- dfTrainings %>%
            select(Date, Time, Sport) %>%
            mutate(Date = Date %>% floor_date(unit = period) %>% ymd()) %>%
            group_by(Sport, Date) %>%
            summarise(total = sum(Time/60)) %>%
            ungroup() %>%
            mutate(Sport = Sport %>% fct_reorder2(Date,total))
                
        df %>% 
            ggplot(aes(x = Date, y = total, color = Sport)) +     
            geom_point() + 
            geom_line(size=1.5) + 
            #facet_wrap(~ Sport) +
            theme_light() +
            #scale_color_tq() +
            scale_y_continuous() +
            labs(
                title = "",
                subtitle = "",
                x = "Date",
                y = "Time (minutes)"
            )
    })

    # 2.1 Distance by quarter ----
    output$distance <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        period <- input$period_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        df <- dfTrainings %>%
            mutate(Period = floor_date(Date, unit = period)) %>%
            group_by(Period) %>%
            summarise(Distance = sum(Distance)) %>%
            ungroup()
        
        plot_col_evolution_no_flip(df,"Period","Distance","km",15)

    })
    
    # 2.2 Time by month year ----
    output$time <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        period <- input$period_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        df <- dfTrainings %>%
            mutate(Period = floor_date(Date, unit = period)) %>%
            group_by(Period) %>%
            summarise(Time = round(sum(Time/60),digits=2)) %>%
            ungroup()
        
        plot_col_evolution_no_flip(df,"Period","Time","minutes",7)
        
    })
    
    # 2.3 Pace by month year ----
    output$pace <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        period <- input$period_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        df <- dfTrainings %>%
            mutate(Period = floor_date(Date, unit = period)) %>%
            group_by(Period) %>%
            summarise(Pace = getTrainingPace(sum(Time/60),sum(Distance))) %>%
            ungroup()
        
        plot_col_evolution_no_flip(df,"Period","Pace","min/km",3)
        
    })
    
    # 2.4 Pace by weekday ----
    output$pace_weekday <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        df <- dfTrainings %>%
            group_by(Weekday) %>%
            summarise(Pace = getTrainingPace(sum(Time/60),sum(Distance))) %>%
            ungroup() %>%
            mutate(Weekday = Weekday %>% as.factor() %>% fct_reorder(desc(Pace)))
        
        plot_col_evolution(df,"Weekday","Pace","min/km",2)
        
    })
    
    # 2.5 Distance by hours ----
    output$distance_by_hours <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        df <- dfTrainings %>%
            group_by(Hour) %>%
            summarise(Distance = sum(Distance)) %>%
            ungroup() %>%
            mutate(Hour = paste0(Hour,":00")) %>%
            mutate(Hour = Hour %>% as.factor() %>% fct_reorder(Distance)) 
            
        
        plot_col_evolution(df,"Hour","Distance","km",17)
        
    })
    
    # 2.6 Trainings by hours ----
    output$trainings_by_hours <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        dfTrainings$Count <- 1
        df <- dfTrainings %>%
            group_by(Hour) %>%
            summarise("Number of trainings" = sum(Count)) %>%
            ungroup() %>%
            mutate(Hour = paste0(Hour,":00"))
        
        plot_col_evolution_no_flip(df,"Hour","Number of trainings","times",8)

    })
    
    # 2.7 Trainings by weekday ----
    output$trainings_by_day <- renderPlot({
        
        date1 <- input$dateRange[1]
        date2 <- input$dateRange[2]
        sport <- input$sport_selection
        
        if (sport != "All sports") {
            dfTrainings <- dfTrainings %>%
                filter(Sport == sport)
        }
        
        if(!is.na(date1) && !is.na(date2)){
            dfTrainings <- dfTrainings %>% 
                filter(Date >= date1 & Date <= date2)
        }
        
        dfTrainings$Count <- 1
        df <- dfTrainings %>%
            group_by(Weekday) %>%
            summarise("Number of trainings" = sum(Count)) %>%
            ungroup() 
        
        plot_col_evolution_no_flip(df,"Weekday","Number of trainings","times",5)

    })
    
})
}
shinyApp(ui, server)

