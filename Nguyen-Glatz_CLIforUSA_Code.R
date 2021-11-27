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

#Industrial production: Non-durable consumer goods
IPNCCONGD = ts_fred('IPNCONGD')
IPNCCONGD = xts(IPNCCONGD[, 3], order.by = as.Date(IPNCCONGD[, 2]))
print(paste("End of IPNCCONGD", ts_summary(IPNCCONGD)$end, sep = ": "))

#Personal Consumption Expenditures
PCE = ts_fred('PCE')
PCE = xts(PCE[, 3], order.by = as.Date(PCE[, 2]))
print(paste("End of PCE", ts_summary(IP)$end, sep = ": "))


#Plotting Industrial production. We can see that it is not stationary
ts_ggplot(log(IP))

#Plotting IPNCCONGD
ts_ggplot(log(IPNCCONGD))

#Plotting PCE
ts_ggplot(log(PCE))

#Plotting month on month growth rates. Roughly similar to first log-differences.
ts_ggplot(ts_pc(IP))
ts_ggplot(ts_pc(IPNCCONGD))
ts_ggplot(ts_pc(PCE))

# Shorten all series to start in 1980-01-01
myStart = "1980-01-01"

IP  = ts_span(IP, start = myStart)
GDP  = ts_span(GDP, start = myStart)
IPNCCONGD = ts_span(IPNCCONGD, start = myStart)
PCE = ts_span(PCE, start = myStart)

#Augmented Dickey-FUller test. First one clearly non-stationary. Log differences stationary.
uRootIP = CADFtest(log(IP), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootIP)

uRootIPd = CADFtest(ts_diff(log(IP)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIPd)

uRootIPNCCONGD = CADFtest(log(IPNCCONGD), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootIPNCCONGD)

uRootIPNCCONGDd = CADFtest(ts_diff(log(IPNCCONGD)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIPNCCONGDd)

uRootPCE = CADFtest(log(PCE), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootPCE)

uRootPCEd = CADFtest(ts_diff(log(PCE)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootPCEd)

#Making series stationary
dIP = ts_diff(log(IP))
ts_ggplot(dIP)

dIPNCCONGD = ts_diff(log(IPNCCONGD))
ts_ggplot(dIPNCCONGD)

dPCE = ts_diff(log(PCE))
ts_ggplot(dPCE)

#Examine lead-lag with CCF. Coincident indicator. IP
dIPq = ts_frequency(dIP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p <- plotCCF(ts_ts(dIPq), ts_ts(GDP), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter.
ModelGDP <- auto.arima(GDP, max.p = 5, max.q = 5, ic = c("bic"))
ModeldIP  <- auto.arima(dIPq, max.p = 5, max.q = 5, ic = c("bic"))
p <- plotCCF(ts_ts(resid(ModeldIP)), ts_ts(resid(ModelGDP)), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p


#Examine lead-lag with CCF. Coincident indicator. IP non-durable goods
dIPNCCONGDq = ts_frequency(dIPNCCONGD, to = "quarter", aggregate= "mean", na.rm = TRUE)
p <- plotCCF(ts_ts(dIPNCCONGDq), ts_ts(GDP), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production: non-durable goods and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter. Lagging one quarter
ModeldIPNCONGD  <- auto.arima(dIPNCCONGDq, max.p = 5, max.q = 5, ic = c("bic"))
p <- plotCCF(ts_ts(resid(ModeldIPNCONGD)), ts_ts(resid(ModelGDP)), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between industrial production: non-durable goods and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. Coincident indicator. PCE
dPCEq = ts_frequency(dPCE, to = "quarter", aggregate= "mean", na.rm = TRUE)
p <- plotCCF(ts_ts(dPCEq), ts_ts(GDP), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between PCE and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter. Leading one quarter.
ModeldPCE  <- auto.arima(dPCEq, max.p = 5, max.q = 5, ic = c("bic"))
p <- plotCCF(ts_ts(resid(ModeldPCE)), ts_ts(resid(ModelGDP)), lag.max = 15)
p <- ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between PCE and GDP growth", subtitle = "Quarterly")
p


