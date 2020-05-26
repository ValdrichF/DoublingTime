library(leaflet)
library(shiny)
library(rgdal)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(shinydashboard)
# download.file('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv', 'Covid19.csv')
covid19 =  read.csv('Covid19.csv')
cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)%>%
    mutate(dateRep = dmy(dateRep))
cases[is.na(cases)] = 0
cases = cases[order(cases$dateRep),]
totCases = cbind(cases[,1],cumsum(cases[,-1]))

ma = function(x, n = 5){
    stats::filter(x,rep(1 / n, n), sides = 1)
}

totCasesMA = apply(totCases[,-1], 2, ma)%>%
    as.data.frame%>%
    +1
totCasesMA = cbind(cases[,1],totCasesMA)
CalcB = function(x, timelag = 1){
    1+diff(x,timelag)/x[(timelag+1):length(x)]
}

B = apply(totCasesMA[,-1], 2, CalcB)
DoubleTime = log10(2)/log10(B)%>%
    as.data.frame
DoubleTime[which((DoubleTime>365.25*2)|(DoubleTime<0), arr.ind = T)] = 365.25*2

DoubleTime = cbind(cases[-1,1], DoubleTime)
DoubleTime = DoubleTime%>%
    filter(!is.na(Afghanistan))%>%
    mutate(dateRep = as.character(dateRep))
DoubleTime[,-1] = log10(DoubleTime[,-1])
countriesShp = readOGR('TM_WORLD_BORDERS-0.3/World_Countries__Generalized_.shp', stringsAsFactors = F)
names(DoubleTime)[c(26, 38, 49, 52, 54, 55, 68,
                    87, 90, 150, 161, 191, 196,
                    201, 202, 203)] = str_replace_all(c("Bonaire","Cabo Verde", "CÃ´te d'Ivoire",
                                                        "Curacao", "Czech Republic", "Congo DRC", "Falkland Islands",
                                                        "Guinea-Bissau", "Vatican City", "Palestinian Territory",
                                                        "Russian Federation", "Timor-Leste",  "Turks and Caicos Islands",
                                                        "Tanzania", "United States", "US Virgin Islands"), ' ', '_')
countriesShp = subset(countriesShp, is.element(countriesShp$COUNTRY,
                                               str_replace_all(names(DoubleTime), '_', ' ')))
DoubleTime = DoubleTime[,c(TRUE,str_replace_all(names(DoubleTime)[-1], '_', ' ')%in%countriesShp$COUNTRY)]

pal = colorNumeric('RdYlGn', domain = range(DoubleTime[,-1]))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # Subset the data based on the slider output
    DataPlot = reactive({
        a = data.frame(t(DoubleTime[which(DoubleTime$dateRep==input$Date),-1]))
        a$CountryName = str_replace_all(row.names(a), '_', ' ')
        names(a)[1] = 'TwoTime'
        row.names(a) = NULL
        a
    })
    # Order
    DataPlotOrdered = reactive({
        DataPlot()[order(match(DataPlot()$CountryName, countriesShp$COUNTRY)),]
    })
    Labels = reactive({
        paste('<p>',DataPlotOrdered()$CountryName, '</p>',
              '<p>', 'Doubling time: ', round(10^DataPlotOrdered()$TwoTime, 0), ' days','</p>', sep = '')%>%
        lapply(htmltools::HTML)
    })

    output$CovidPlot = renderLeaflet({
        # Creating a continuous palette function
        
        # draw the map
        countriesShp%>%
            leaflet()%>%
            addTiles()%>%
            addPolygons(weight = 1, 
                        smoothFactor = 0.5,
                       color = "white",
                       opacity = 0.7,
                        fillColor = pal(DataPlotOrdered()$TwoTime),
                        fillOpacity = 0.8,
                        # highlightOptions = highlightOptions(
                        #     weight = 5,
                        #     color = "#666",
                        #     dashArray = "",
                        #     fillOpacity = 0.7,
                        #     bringToFront = TRUE),
                        label = Labels(), 
                       labelOptions = labelOptions(style = list('front-wright'='normal',
                                                                padding = "3px 8px"),
                                                   textsize = '15px'))%>%
            addLegend("topright",pal, DataPlotOrdered()$TwoTime, 
                      labFormat = labelFormat(transform = function(x) round(10^x), suffix = ' days'),
                      title = 'Doubling Time')
    })
    output$Selection = renderText({
        paste0("Doubling time (in days) of the number of COVID-19 cases on: ",
               strftime(DoubleTime[which(DoubleTime$dateRep==input$Date),1], format = "%d-%m-%Y"))
    })

})
