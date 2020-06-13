library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(rgdal)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)

source('CovidAnalysisGET.R')

# Define UI for application that draws a Map
shinyUI(dashboardPage(
    skin = 'black',
    # Application title
    dashboardHeader(title = "Spread of a disease: Coveying exponential data", titleWidth = '90vw'),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        sliderInput("Date",
                    "Select Date:",
                    min = cases$dateRep[6],
                    max = cases$dateRep[nrow(cases)],
                    value = cases$dateRep[6],
                    step = 1,
                    timeFormat = '%d-%m' 
                    #animate = animationOptions(interval = 5000, F),
                    ),
        submitButton('Submit')
), 

        # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(box(
            h4(strong('Info:')),
            h5(strong('Doubling Time:')),
            p("Diseases spread exponentially. This basically means that higher the number of 
            infected people present in a society, more people are going to get infected. 
              Most of us can't intuatively make sense of this kind of spread which leads to
              the severity of the situation being under-estimated. Doubling time offers a more
              intuative explaination to this. It is the time it would take for the number of cases to double.
              A lower number indicates a faster spread of the disease, even when the total number of cases are low.",
              a('A good video explaining this by the Barcelona Institute for Global Health.', href = 'https://youtu.be/qri7BjbcVtg')),
            h5(strong('Interpretation:')),
            p("This doubling time has been calculated for each day and hence should not be extrapolated.
              However, it does a reasonably good job at describing the situation on that day. A good analogy would be 
              the 'slope of the graph, on that day'."),
            p("Another important fact is that the doubling time is capped at 2 years as this method of calculating the doubling time
              cannot be extrapolated for long periods. So a country with a doubling time of 730 days either has a very slow spread of the disease
              or the disease is being eradicated effectively."),
            h5(strong('Visualizing the data:')),
            p("Select the date you'd like to plot on the map by using the slider in the side panel.
              After that click on 'Submit' to plot the data for that day. You could also focus on countries of your interest by 
              selecting them and visualizing its doubling time over days since it reached 150 cases.
              You'd need to click submit again to update the graph"),
            width = 12)),
        fluidRow(box(
            leafletOutput("CovidPlot"),
            textOutput("Selection"),
            width = 12)),
        fluidRow(box(
            selectInput('CountriesSel', NULL, choices = c('Please select countries to plot' = '',
                                                          sort(str_replace_all(names(cases[,-1]),
                                                                               '_', ' '))), multiple = TRUE),
            plotlyOutput('DoubleTimePlt'),
            width = 12)),
        fluidRow( box(
            h4(strong('Sources:')),
            p(strong(em('Number of cases of corona virus: ')),
              a('European Centre for Disease Prevention and Control',
                href = 'https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide')),
            p(strong(em(' Shape file of countries: ')),
              a('ArcGIS Hub',
                href = 'https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0')),
            h4(strong('Author:')),
            p(em('Valdrich J Fernandes')),
            p(em('valdrichfernandes@gmail.com')),
            width = 12))

    )
))
