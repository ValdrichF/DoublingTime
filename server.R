# library(leaflet)
# library(shiny)
# library(rgdal)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(lubridate)
# library(shinydashboard)
# library(ggplot2)
# library(plotly)

##  defining a pallet
pal = colorNumeric('RdYlGn', domain = range(DoubleTime[,-1]))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## Subset the data based on the slider output
    DataPlot = reactive({
        a = data.frame(t(DoubleTime[which(DoubleTime$dateRep==input$Date),-1]))
        a$CountryName = str_replace_all(row.names(a), '_', ' ')
        names(a)[1] = 'TwoTime'
        row.names(a) = NULL
        a
    })
    ## Confirming the order of the country names match
    DataPlotOrdered = reactive({
        DataPlot()[order(match(DataPlot()$CountryName, countriesShp$COUNTRY)),]
    })
    ## defining the labels for each country in the map
    Labels = reactive({
        paste('<p>',DataPlotOrdered()$CountryName, '</p>',
              '<p>', 'Doubling time: ', round(10^DataPlotOrdered()$TwoTime, 0), ' days','</p>', sep = '')%>%
        lapply(htmltools::HTML)
    })
    # draw the map
    output$CovidPlot = renderLeaflet({
        countriesShp%>%
            leaflet()%>%
            addTiles()%>%
            addPolygons(weight = 1, 
                        smoothFactor = 0.5,
                        color = "white",
                        opacity = 0.7,
                        fillColor = pal(DataPlotOrdered()$TwoTime),
                        fillOpacity = 0.8,
                        label = Labels(),
                        labelOptions = labelOptions(style = list('front-wright'='normal',
                                                                 padding = "3px 8px"),
                                                    textsize = '15px'))%>%
            addLegend("topright",pal, DataPlotOrdered()$TwoTime,
                      labFormat = labelFormat(transform = function(x) round(10^x), suffix = ' days'),
                      title = 'Doubling Time')
    })
    # A line referring to the date plotted in the map (slider check)
    output$Selection = renderText({
        paste0("Doubling time (in days) of the number of COVID-19 cases on: ",
               strftime(DoubleTime[which(DoubleTime$dateRep==input$Date),1], format = "%d-%m-%Y"))
    })
    CountryCorrect = reactive({
        if(identical(input$CountriesSel, NULL)) return(NULL)
        str_replace_all(input$CountriesSel, pattern = ' ', replacement = '_')
    })
    output$DoubleTimePlt = renderPlotly({
        if(identical(input$CountriesSel, NULL)) return(NULL)
        p2 = ggplot(filter(DoubleTimeGather,Country %in% CountryCorrect()), aes(x= days, y = 10^DoublingTime, colour = Country))+
            geom_line(size = 1)+
            theme(legend.position = 'bottom')+ 
            labs(x = 'Days (since 150 cases)', y = 'Doubling Time (days)')+
            theme_bw()+
            scale_y_log10()
        ggplotly(p2)%>%
            toWebGL()
    })

})
