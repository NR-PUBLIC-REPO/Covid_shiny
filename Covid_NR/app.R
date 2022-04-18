## Nayia RAVEL
## Covid App using FindCovidTrack


library(DT)
library(dplyr)
library(ggplot2)

#library(googleVis)
library(lubridate)
library(magrittr)

library('rsconnect')


library(shiny)
library(shinythemes)

library(tidyverse)
# library(vroom)


### 1. Get the latest data
df <- read.csv("https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/processed/data_all.csv",
               sep = ",")

## Adding variables to get monthly and quarterly results
df_country <- df %>% 
  filter(set=="country") %>% 
  mutate(month_year=format(as.Date(time), "%Y-%m"),
         month = format(as.Date(time), "%m"),
         year = format(as.Date(time), "%Y"),
         qtr_year = quarter(as.Date(time), with_year = T))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme=shinytheme('superhero'),
  # Application title
  titlePanel("Covid Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # selectInput("countries","Countries",
      #             choices=c(as.character(sort(unique(df_country$name))))),
      selectizeInput("countries", "Countries (max 6)",
                     choices=c(as.character(sort(unique(df_country$name)))), selected=NULL, options = list(maxItems = 6)),
      
      selectInput("metric", "First tab/Third tab : Days with new cases/tests/deaths data available (in days)",
                  choices=c("new_cases_orig","new_tests_orig","new_deaths_orig"), 
                  selected = NULL),
      selectInput("average", "Second tab/Third tab : Average cases/tests/deaths per capita (per 1000 people)",
                  choices=c("cap_new_cases","cap_new_tests","cap_new_deaths"), 
                  selected = NULL)
    )
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Days with new cases/tests/deaths data", DTOutput("monthlyNewTable"),DTOutput("quarterlyNewTable")),
                  tabPanel("Average cases/tests/deaths per capita", DTOutput("monthlyAverageTable"),DTOutput("quarterlyAverageTable")),
                  tabPanel("Boxplots", plotOutput("boxMonth"),plotOutput("boxQuarter"),plotOutput("boxMonthAverage"),plotOutput("boxQuarterAverage"))
      )
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ####   Fist tab : New cases/tests/deaths data available (in days)
  ### Monthly data
  
  data_month <- reactive({
    df_month <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,month_year,.data[[input$metric]]) %>%
      group_by(name,month_year) %>%
      summarise(number_days = n()) %>%
      pivot_wider(names_from = name, values_from = number_days)
    
    
    
  })
  
  ### Quarterly data
  data_qtr <- reactive({
    df_quarter <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,qtr_year,.data[[input$metric]]) %>%
      group_by(name,qtr_year) %>%
      summarise(number_days = n()) %>%
      pivot_wider(names_from = name, values_from = number_days)
    
  })
  
  
  output$monthlyNewTable <- DT::renderDT({
    datatable(data_month())
  })
  
  output$quarterlyNewTable <- DT::renderDT({
    datatable(data_qtr())
  })
  
  
  ####   Second tab : Average cases/tests/deaths per 1000 people
  ### Monthly data
  monthly_average <- reactive({
    df_month <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,month_year,.data[[input$average]]) %>%
      group_by(name,month_year) %>%
      summarise(average = mean(.data[[input$average]],na.rm=T)) %>%
      pivot_wider(names_from = name, values_from = average)
    
    
    
  })
  
  ### Quarterly data
  qtr_average <- reactive({
    df_quarter <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,qtr_year,.data[[input$average]]) %>%
      group_by(name,qtr_year) %>%
      summarise(average = mean(.data[[input$average]],na.rm=T)) %>%
      pivot_wider(names_from = name, values_from = average)
    
  })
  
  
  output$monthlyAverageTable <- DT::renderDT({
    datatable(monthly_average())
  })
  
  output$quarterlyAverageTable <- DT::renderDT({
    datatable(qtr_average())
  })
  
  
  
  #### Third tab : Average cases/tests/deaths per 1000 people
  data_boxMonth <- reactive({
    df_month <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,month_year,.data[[input$metric]]) %>%
      group_by(name,month_year) %>%
      summarise(number_days = n())
  })
  
  
  data_boxQuarter <- reactive({
    df_month <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,qtr_year,.data[[input$metric]]) %>%
      group_by(name,qtr_year) %>%
      summarise(number_days = n())
  })
  
  
  output$boxMonth <- renderPlot({
    #dataToPlot <- data_month()
    ggplot(data_boxMonth(), aes(x=name, y=number_days)) + 
      geom_boxplot()+
      xlab("Countries")+
      ylab("Numbers (monthly)") +
      ggtitle("Days with new cases/tests/deaths data")
    # guides(colour = guide_legend(override.aes = list(size=5)))+
    # ylab(input$yax)+xlab("Year")+labs(size=input$bub)
  })
  
  output$boxQuarter <- renderPlot({
    #dataToPlot <- data_month()
    ggplot(data_boxQuarter(), aes(x=name, y=number_days)) + 
      geom_boxplot()+
      xlab("Countries")+
      ylab("Numbers (quarterly)")+
      ggtitle("Days with new cases/tests/deaths data")
    # guides(colour = guide_legend(override.aes = list(size=5)))+
    # ylab(input$yax)+xlab("Year")+labs(size=input$bub)
  })
  
  ### Average
  data_monthAverage <- reactive({
    df_month <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,month_year,.data[[input$average]]) %>%
      group_by(name,month_year) %>%
      summarise(average = mean(.data[[input$average]],na.rm=T)) 
    
  })
  
  ### Quarterly data
  data_qrtAverage <- reactive({
    df_quarter <- df_country %>%
      filter(name %in% input$countries) %>%
      select(name,unit,qtr_year,.data[[input$average]]) %>%
      group_by(name,qtr_year) %>%
      summarise(average = mean(.data[[input$average]],na.rm=T)) 
    
  })
  
  output$boxMonthAverage <- renderPlot({
    #dataToPlot <- data_month()
    ggplot(data_monthAverage(), aes(x=name, y=average)) + 
      geom_boxplot()+
      xlab("Countries")+
      ylab("Numbers (monthly)") +
      ggtitle("Average cases/tests/deaths per capita (per 1000 people)")
    # guides(colour = guide_legend(override.aes = list(size=5)))+
    # ylab(input$yax)+xlab("Year")+labs(size=input$bub)
  })
  
  output$boxQuarterAverage <- renderPlot({
    #dataToPlot <- data_month()
    ggplot(data_qrtAverage(), aes(x=name, y=average)) + 
      geom_boxplot()+
      xlab("Countries")+
      ylab("Numbers (quarterly)")+
      ggtitle("Average cases/tests/deaths per capita (per 1000 people)")
    # guides(colour = guide_legend(override.aes = list(size=5)))+
    # ylab(input$yax)+xlab("Year")+labs(size=input$bub)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


