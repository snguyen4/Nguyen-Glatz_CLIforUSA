#CLI for USA construction using FRED data

#Loading needed libraries
library(tsbox)
library(quantmod)
library(forecast)
library(tsbox)
library(dataseries)
library(ggplot2)  
library(forecast)
library(gridExtra)
library(xts)
library(CADFtest) 

#Reset
rm(list=ls())

#Loading Prof. Kaufamann packages
source("UserPackages.R")

#To save files + graphs
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "/ResultsCLIforUSA")


#Loading GDP growth from FRED + transformation to a times series
GDP = ts_fred("A191RL1Q225SBEA")
GDP = xts(GDP[,3], order.by = as.Date(GDP[,2]))
print(paste("End of GDP", ts_summary(GDP)$end, sep = ": "))

#Plotting GDP growth
ts_ggplot(GDP)

#Possible indicators
#Industrial production
IP = ts_fred('IPTB56300S')
IP = xts(IP[, 3], order.by = as.Date(IP[, 2]))
print(paste("End of IP", ts_summary(IP)$end, sep = ": "))

#Plotting Industrial production. We can see that it is not stationary
ts_ggplot(log(IP))

#Plotting month on month growth rates. Roughly similar to first log-differences.
ts_ggplot(ts_pc(IP))

#Augmented Dickey-FUller test. First one clearly non-stationary. Log differences stationary.
uRootIP = CADFtest(log(IP), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootIP)

uRootIPd = CADFtest(ts_diff(log(IP)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIPd)

#Making IP stationary
dIP = ts_diff(log(IP))

#Examine lead-lag with CCF. Coincident indicator.
dIPq = ts_frequency(dIP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p <- plotCCF(ts_ts(dIPq), ts_ts(GDP), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data
ModelGDP <- auto.arima(GDP, max.p = 5, max.q = 5, ic = c("bic"))
ModeldIP  <- auto.arima(dIPq, max.p = 5, max.q = 5, ic = c("bic"))
p <- plotCCF(ts_ts(resid(ModeldIP)), ts_ts(resid(ModelGDP)), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

