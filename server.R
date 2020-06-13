##  defining a pallet
pal = colorNumeric('RdYlGn', domain = log10(c(1,365.25*2)))
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    totCasesMA = reactive({
        invalidateLater(1*24*60*60*1000, session)
        # Calculating the doubling time from the number of new cases
        library(httr)
        library(readr)
        covid19 = content(GET('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'), type = 'text/csv',
                          col_types = cols(dateRep = col_character(), 
                                           day = col_double(), 
                                           month = col_double(),
                                           year = col_double(),
                                           cases = col_double(),
                                           deaths = col_double(),
                                           countriesAndTerritories = col_character(),
                                           geoId = col_character(),
                                           countryterritoryCode = col_character(),
                                           popData2018 = col_double(), 
                                           continentExp = col_character()
                                           ))
        ## Wider format, Date vs Countries
        cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)%>%
            mutate(dateRep = dmy(dateRep))
        cases[is.na(cases)] = 0
        cases = cases[order(cases$dateRep),]
        
        totCases = cbind(cases[,1],cumsum(cases[,-1]))
        ToCasesMA = apply(totCases[,-1], 2, ma)%>%
            as.data.frame%>%
            +1 # +1 to help with calculating the Log10
        ToCasesMA = cbind(cases[,1],ToCasesMA)
        patterns = c('Bona', 'Ver', 'Ivo', 'Cur', 'Cze', '^D.*Congo', 'Falkland', 'Guinea_B', 'Holy', 'pale',
                     'Russia', 'Timor', 'Turks', 'Tanzania', 'America', 's_Virgin')
        indexes = mapply(grep, pattern = patterns, MoreArgs = list(x = names(ToCasesMA), ignore.case = TRUE))
        names(ToCasesMA)[indexes] = str_replace_all(c("Bonaire","Cabo Verde", countriesShp$COUNTRY[grep('Ivoi', countriesShp$COUNTRY)],
                                                       "Curacao", "Czech Republic", "Congo DRC", "Falkland Islands",
                                                       "Guinea-Bissau", "Vatican City", "Palestinian Territory",
                                                       "Russian Federation", "Timor-Leste",  "Turks and Caicos Islands",
                                                       "Tanzania", "United States", "US Virgin Islands"), ' ', '_')
        ToCasesMA
    })
    DoubleTime = reactive({
        invalidateLater(1*24*60*60*1000, session)
        B = apply(totCasesMA()[,-1], 2, CalcB)
        
        ## Doubling time from R~0~
        doubleTime = log10(2)/log10(B)%>%
            as.data.frame
        
        ## Limiting the range to between 0 days and 2 years.
        ## Large numbers mean a very flat curve and 
        ## negative numbers mean the R0 is less than 1 so it'll never double
        doubleTime[which((doubleTime>365.25*2)|(doubleTime<0), arr.ind = T)] = 365.25*2
        doubleTime = cbind(cases[-1,1], doubleTime)
        
        ## Remove the first four days (Lost due to Moving average of 5 days)
        doubleTime = doubleTime%>%
            filter(!is.na(Afghanistan))
        doubleTime[,-1] = log10(doubleTime[,-1]) # Plot on a log scale as smaller values are of interest
        
        ## Subsetting the shapefile to the countries in doubleTime
        countriesShp = subset(countriesShp, is.element(countriesShp$COUNTRY, str_replace_all(names(doubleTime), '_', ' ')))
        
        ## matching the order of the countries to match the shapefile
        indexes = sapply(countriesShp$COUNTRY, function(x) which(x==str_replace_all(names(doubleTime), '_', ' ')))
        doubleTime[,c(1,indexes)]
    })
    DoubleTimeGather = reactive({
        invalidateLater(1*24*60*60*1000, session)
        gathering(DoubleTime(), 'DoublingTime', totCasesMA())
    })
    
    ## Subset the data based on the slider output
    DataPlot = reactive({
        a = data.frame(t(DoubleTime()[which(DoubleTime()$dateRep==input$Date),-1]))
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
              '<p>', 'Doubling time: ', round(10^DataPlotOrdered()$TwoTime, 1), ' days','</p>', sep = '')%>%
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
               strftime(DoubleTime()[which(DoubleTime()$dateRep==input$Date),1], format = "%d-%m-%Y"))
    })
    # Replace _ with a space in the name of the countries selected to match the data
    CountryCorrect = reactive({
        if(identical(input$CountriesSel, NULL)) return(NULL)
        str_replace_all(input$CountriesSel, pattern = ' ', replacement = '_')
    })
    # Plot the Doubling time
    output$DoubleTimePlt = renderPlotly({
        if(identical(input$CountriesSel, NULL)) return(NULL)
        p2 = ggplot(filter(DoubleTimeGather(),Country %in% CountryCorrect()))+
            geom_line(aes(x= days, y = 10^DoublingTime, colour = Country), size = 1)+
            theme(legend.position = 'bottom')+ 
            labs(x = 'Days (since 150 cases)', y = 'Doubling Time (days)')+
            theme_bw()+
            scale_y_log10()
        height = session$clientData$output_p_height
        width = session$clientData$output_p_width
        ggplotly(p2, height = height, width = width)%>%
            toWebGL() # faster rendering in browsers
    })

})
