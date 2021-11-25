#CLI for USA construction using FRED data

#Loading needed libraries
library(tsbox)
library(forecast)
library(tsbox)
library(dataseries)
library(ggplot2)  
library(forecast)
library(gridExtra)
library(xts)

#Reset
rm(list=ls())

#Loading Prof. Kaufamann packages
source("UserPackages.R")

#To save files + graphs
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "/ResultsApp3")


#Loading GDP growth from FRED + transformation to a times series
GDP = ts_fred('A191RL1Q225SBEA')
GDP = xts(GDP[,3], order.by=as.Date(GDP[,2]))
print(paste("End of GDP", ts_summary(GDP)$end, sep = ": "))

#Plotting GDP growth
ts_ggplot(GDP)
