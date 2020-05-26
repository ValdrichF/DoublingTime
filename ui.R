library(leaflet)
library(shiny)
library(rgdal)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(shinydashboard)

covid19 =  read.csv('Covid19.csv')%>%
    mutate(dateRep = dmy(dateRep))
cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)
cases[is.na(cases)] = 0
cases = cases[order(cases$dateRep),]

# Define UI for application that draws a Map
shinyUI(dashboardPage(
    skin = 'black',
    # Application title
    dashboardHeader(title = "Doubling time of COVID-19 by country", titleWidth = '90vw'),

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
        submitButton('Submit')), 

        # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(box(
            h4(strftime(Sys.Date(), format = "%d-%m-%Y")),
            width = 12)),
        fluidRow(box(
            leafletOutput("CovidPlot"),
            textOutput("Selection"),
            width = 12)),
        fluidRow( box(
            h4('Sources:'),
            p(strong(em('Number of cases of corona virus: ')),
              a('European Centre for Disease Prevention and Control',
                href = 'https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide')),
            p(strong(em(' Shape file of countries: ')),
              a('ArcGIS Hub',
                href = 'https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0')),
            h4('Author: Valdrich J Fernandes'),
            p('valdrichfernandes@gmail.com'),
            width = 12)) 
    )
))
