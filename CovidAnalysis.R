library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(rgdal)

# Calculating the doubling time from the number of new cases
if(!file.exists('Covid19.csv')) {
    download.file('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv',
                  'Covid19.csv')
}

covid19 =  read.csv('Covid19.csv')

## Wider format, Date vs Countries
cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)%>%
    mutate(dateRep = dmy(dateRep))
cases[is.na(cases)] = 0
cases = cases[order(cases$dateRep),]

## function to calculate the Moving average of the previous 5 days
ma = function(x, n = 5) stats::filter(x,rep(1 / n, n), sides = 1)

totCases = cbind(cases[,1],cumsum(cases[,-1]))
totCasesMA = apply(totCases[,-1], 2, ma)%>%
    as.data.frame%>%
    +1 # +1 to help with calculating the Log10
totCasesMA = cbind(cases[,1],totCasesMA)

## Plot the total cases for all the countries. Slow due to 200+ countries
## Run with caution
# a = pivot_longer(totCasesMA,-dateRep, 'CountriesAndTerritories', values_to = 'TotalCases')
# 
# ggplot(a, aes(dateRep, TotalCases, color= CountriesAndTerritories))+
#     geom_line(show.legend = F)+
#     scale_y_log10()

## Caluclate 'B' or R~0~ R-naught on each day
CalcB = function(x, timelag = 1) 1+diff(x,timelag)/x[(timelag+1):length(x)]

B = apply(totCasesMA[,-1], 2, CalcB)

## Doubling time from R~0~
DoubleTime = log10(2)/log10(B)%>%
    as.data.frame

## Limiting the range to between 0 days and 2 years.
## Large numbers mean a very flat curve and 
## negative numbers mean the R0 is less than 1 so it'll never double
DoubleTime[which((DoubleTime>365.25*2)|(DoubleTime<0), arr.ind = T)] = 365.25*2

## to understand the negative numbers 
x = seq(0.001, 2, length.out = 100)
plot(x, log10(2)/log10(x), type = 'l', ylim = c(-10,10))

DoubleTime = cbind(cases[-1,1], DoubleTime)

## Plot the Doubling time, very slow again
# a = pivot_longer(DoubleTime, -dateRep, 'CountriesAndTerritories', values_to = 'DoubleTime')
# 
# ggplot(a, aes(dateRep, DoubleTime, color= CountriesAndTerritories))+
#     geom_line(show.legend = F)+
#     scale_y_log10()

## Remove the first four days (Lost due to Moving average of 5 days)
DoubleTime = DoubleTime%>%
    filter(!is.na(Afghanistan))
DoubleTime[,-1] = log10(DoubleTime[,-1]) # Plot on a log scale as smaller values are of interest

# PreProcessing DoubleTime and shapefile
## Matching the country names in the shape file with the DoubleTime
if(!dir.exists('TM_WORLD_BORDERS-0.3')) {
    download.file('https://opendata.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0.zip',
                                                      'TM_WORLD_BORDERS-0.3.zip')
    unzip('TM_WORLD_BORDERS-0.3.zip', exdir = './TM_WORLD_BORDERS-0.3')
}
countriesShp = readOGR('./TM_WORLD_BORDERS-0.3/World_Countries__Generalized_.shp')
names(DoubleTime)[c(26, 38, 49, 52, 54, 55, 68,
                    87, 90, 150, 161, 191, 196,
                    201, 202, 203)] = str_replace_all(c("Bonaire","Cabo Verde", countriesShp$COUNTRY[grep('Ivoi', countriesShp$COUNTRY)],
                                                        "Curacao", "Czech Republic", "Congo DRC", "Falkland Islands",
                                                        "Guinea-Bissau", "Vatican City", "Palestinian Territory",
                                                        "Russian Federation", "Timor-Leste",  "Turks and Caicos Islands",
                                                        "Tanzania", "United States", "US Virgin Islands"), ' ', '_')

names(totCasesMA)[c(26, 38, 49, 52, 54, 55, 68,
                    87, 90, 150, 161, 191, 196,
                    201, 202, 203)] = str_replace_all(c("Bonaire","Cabo Verde", countriesShp$COUNTRY[grep('Ivoi', countriesShp$COUNTRY)],
                                                        "Curacao", "Czech Republic", "Congo DRC", "Falkland Islands",
                                                        "Guinea-Bissau", "Vatican City", "Palestinian Territory",
                                                        "Russian Federation", "Timor-Leste",  "Turks and Caicos Islands",
                                                        "Tanzania", "United States", "US Virgin Islands"), ' ', '_')

## Subsetting the shapefile to the countries in DoubleTime
countriesShp = subset(countriesShp, is.element(countriesShp$COUNTRY, str_replace_all(names(DoubleTime), '_', ' ')))

## matching the order of the countries to match the shapefile
indexes = sapply(countriesShp$COUNTRY, function(x) which(x==str_replace_all(names(DoubleTime), '_', ' ')))
DoubleTime = DoubleTime[,c(1,indexes)]

gathering = function(dataframe, parameter = 'parameter'){
    Gathered = NULL
    for (i in names(dataframe)[-1]){
        ind = which(totCasesMA[-(1:5),i]>150)
        if(length(ind)<2) {
            next
        }
        Gathered = rbind(Gathered, data.frame(days = 1:length(ind),
                                              para = dataframe[,i][ind],
                                              Country = rep(i, times = length(ind))))
    }
    if(!identical(Gathered, NULL))    names(Gathered)[2] = parameter
    Gathered
}
DoubleTimeGather = gathering(DoubleTime, 'DoublingTime')
