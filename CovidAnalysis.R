library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(rgdal)

download.file('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv', 'Covid19.csv')
covid19 =  read.csv('Covid19.csv')
geoID = unique(covid19[,c("geoId","countriesAndTerritories")])
# pop = NULL
# for (i in unique(covid19$countriesAndTerritories)){
#     pop = with(covid19,rbind(pop,c(i,mean(popData2018[countriesAndTerritories == i]))))
# }
# pop = as.data.frame(pop)
# names(pop) = c('countriesAndTerritories', 'popData2018')

cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)%>%
    mutate(dateRep = dmy(dateRep))
cases[is.na(cases)] = 0
cases = cases[order(cases$dateRep),]

# deaths = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = deaths)%>%
#     mutate(dateRep = dmy(dateRep))
# deaths[is.na(deaths)] = 0
# deaths = deaths[order(deaths$dateRep),]

totCases = cbind(cases[,1],cumsum(cases[,-1]))

ma = function(x, n = 5){
    stats::filter(x,rep(1 / n, n), sides = 1)
}

totCasesMA = apply(totCases[,-1], 2, ma)%>%
    as.data.frame%>%
    +1
totCasesMA = cbind(cases[,1],totCasesMA)
# a = pivot_longer(totCasesMA,-dateRep, 'CountriesAndTerritories', values_to = 'TotalCases')
# 
# ggplot(a, aes(dateRep, TotalCases, color= CountriesAndTerritories))+
#     geom_line(show.legend = F)+
#     scale_y_log10()

CalcB = function(x, timelag = 1){
    1+diff(x,timelag)/x[(timelag+1):length(x)]
}

B = apply(totCasesMA[,-1], 2, CalcB)
DoubleTime = log10(2)/log10(B)%>%
    as.data.frame
DoubleTime[which((DoubleTime>365.25*2)|(DoubleTime<0), arr.ind = T)] = 365.25*2

DoubleTime = cbind(cases[2:143,1], DoubleTime)

# a = pivot_longer(DoubleTime, -dateRep, 'CountriesAndTerritories', values_to = 'DoubleTime')
# 
# ggplot(a, aes(dateRep, DoubleTime, color= CountriesAndTerritories))+
#     geom_line(show.legend = F)+
#     scale_y_log10()

CountryCoord = read.csv('countries.csv')
CountryCoord = inner_join(geoID, CountryCoord, by = c('geoId' = 'country'))

DoubleTime = DoubleTime%>%
    filter(!is.na(Afghanistan))
DoubleTime[,-1] = log10(DoubleTime[,-1])

write.csv(CountryCoord, 'CountryCoord.csv', row.names = F)

# download.file('https://datahub.io/core/geo-countries/r/countries.geojson',
             # 'countries.geojson')
countries = readOGR('countries.geojson')

DoubleTimeNames = substr(str_replace(names(DoubleTime), '_', ' '), 1, 6)
# indexes = mapply(grep, pattern = paste('^', DoubleTimeNames, sep = ''), MoreArgs = list(x = countries$ADMIN, ignore.case = T, perl = T))
# a = countries[!(countries$ADMIN %in%str_replace_all(names(DoubleTime), '_', ' ')),]
# View(names(DoubleTime)[!(str_replace_all(names(DoubleTime), '_', ' ')%in%countries$ADMIN)])
countries$ADMIN[grep('The Bahamas', countries$ADMIN)] = 'Bahamas'
countries$ADMIN[grep('Brunei' , countries$ADMIN)] = 'Brunei Darussalam'
countries$ADMIN[grep('Ivory Coast', countries$ADMIN)] = 'Cote dIvoire'
countries$ADMIN[grep('Czech Republic', countries$ADMIN)] = 'Czechia'
countries$ADMIN[grep('Falkland Islands', countries$ADMIN)] = 'Falkland IslandsMalvinas'
countries$ADMIN[grep('Vatican', countries$ADMIN)] = 'Holy See'
countries$ADMIN[grep('Republic of Serbia', countries$ADMIN)] = 'Serbia'
countries$ADMIN[grep('Timor', countries$ADMIN)] = 'Timor Leste'
countries$ADMIN[grep('Swazi', countries$ADMIN)] = 'Eswatini'
countries = countries[countries$ADMIN %in%str_replace_all(names(DoubleTime), '_', ' '),]

# DoubleTime = DoubleTime[,c(1,which(str_replace_all(names(DoubleTime), '_', ' ')%in%countries$ADMIN))]
indexes = sapply(countries$ADMIN, function(x) which(x==str_replace_all(names(DoubleTime), '_', ' ')))
DoubleTime = DoubleTime[,c(1,indexes)]
write.csv(DoubleTime, 'DoubleTime.csv', row.names = F)

# Working with a Shapefile
# download.file('https://opendata.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0.zip', 'TM_WORLD_BORDERS-0.3.zip')
countriesShp = readOGR('./TM_WORLD_BORDERS-0.3/World_Countries__Generalized_.shp')
names(DoubleTime)[c(26, 38, 49, 52, 54, 55, 68,
                    87, 90, 150, 161, 191, 196, 201, 202, 203)] = str_replace_all(c("Bonaire","Cabo Verde", "CÃ´te d'Ivoire",
                                                                 "Curacao", "Czech Republic", "Congo DRC", "Falkland Islands",
                                                                 "Guinea-Bissau", "Vatican City", "Palestinian Territory",
                                                                 "Russian Federation", "Timor-Leste",  "Turks and Caicos Islands",
                                                                 "Tanzania", "United States", "US Virgin Islands"), ' ', '_')
countriesShp = subset(countriesShp, is.element(countriesShp$COUNTRY, str_replace_all(names(DoubleTime), '_', ' ')))
indexes = sapply(countriesShp$COUNTRY, function(x) which(x==str_replace_all(names(DoubleTime), '_', ' ')))
DoubleTime = DoubleTime[,c(1,indexes)]
write.csv(DoubleTime, './CovidMap/DoubleTime.csv', row.names = F)

pal = colorNumeric('Purples', domain = c(0,7),
                   reverse = TRUE, alpha = 1)

DataPlot = t(DoubleTime[91,-1])

Labels = paste('<p>',countriesShp$COUNTRY, '</p>',
          '<p>', 'Doubling time: ', DataPlot, ' days','</p>', sep = '')%>%
        lapply(htmltools::HTML)

countriesShp%>%
    leaflet()%>%
    addTiles()%>%
    addPolygons(weight = 1, stroke = FALSE, fillColor = ~pal(DataPlot), fillOpacity = 0.7,
                color = 'white', opacity = 0.7,
                highlightOptions = highlightOptions(weight = 5, color = '#666666',
                                                    dashArray = '',fillOpacity = 0.7,
                                                    bringToFront = T),
                label = Labels, labelOptions = labelOptions(style = list('front-wright'='normal',
                                                                           padding = "3px 8px"),
                                                              textsize = '15px'))
ggplot(countriesShp, aes(long, lat), colour = 'black' , fill = NA) + 
    theme_void()
a = st_read('./TM_WORLD_BORDERS-0.3/World_Countries__Generalized_.shp')
b = st_transform(a, crs="+init=epsg:4326")
map_df = ms_simplify(a)
plot(countriesShp)
