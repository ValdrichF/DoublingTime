library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(rgdal)

covid19 =  read.csv('Covid19.csv')

## Wider format, Date vs Countries
cases = pivot_wider(covid19, dateRep, names_from = countriesAndTerritories, values_from = cases)%>%
    mutate(dateRep = dmy(dateRep))
cases[is.na(cases)] = 0
cases = cases[order(cases$dateRep),]

## function to calculate the Moving average of the previous 5 days
ma = function(x, n = 5) stats::filter(x,rep(1 / n, n), sides = 1)

## Caluclate 'B' or R~0~ R-naught on each day
CalcB = function(x, timelag = 1) 1+diff(x,timelag)/x[(timelag+1):length(x)]

countriesShp = readOGR('./TM_WORLD_BORDERS-0.3/World_Countries__Generalized_.shp')

gathering = function(dataframe, parameter = 'parameter', totCasesMA = totCasesMA){
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
