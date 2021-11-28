#CLI for USA construction using FRED data

#1) Setting up -----------------------------------------------------------------
#-------------------------------------------------------------------------------

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
library(seasonal)

#Reset
rm(list=ls())

#Loading Prof. Kaufamann packages
source("UserPackages.R")

#To save files + graphs
mainDir = getwd()
outDir = makeOutDir(mainDir, "/ResultsCLIforUSA")


#2) Loading data/Data transformation -------------------------------------------
#-------------------------------------------------------------------------------
  
#Loading GDP growth from FRED + transformation to a times series
GDP = ts_fred("A191RL1Q225SBEA")
GDP = xts(GDP[,3], order.by = as.Date(GDP[,2]))

#Plotting GDP growth
ts_ggplot(GDP)

#Possible indicators
#Industrial production
IP = ts_fred("IPTB56300S")
IP = xts(IP[, 3], order.by = as.Date(IP[, 2]))

#Personal Consumption Expenditures
PCE = ts_fred("PCE")
PCE = xts(PCE[, 3], order.by = as.Date(PCE[, 2]))

#Early Estimate of Quarterly ULC Indicators: Total Labor Productivity for the United States
TLP = ts_fred("ULQELP01USQ657S")
TLP = xts(TLP[, 3], order.by = as.Date(TLP[, 2]))

#Manufacturers' New Orders: Total Manufacturing
TM = ts_fred("AMTMNO")
TM = xts(TM[, 3], order.by = as.Date(TM[, 2]))

#Leading Indicators OECD: Component series: Share prices: Normalised for the United States
SP = ts_fred("USALOCOSPNOSTSAM")
SP = xts(SP[, 3], order.by = as.Date(SP[, 2]))

# #Industrial production: Non-durable consumer goods
# IPNCCONGD = ts_fred("IPNCONGD")
# IPNCCONGD = xts(IPNCCONGD[, 3], order.by = as.Date(IPNCCONGD[, 2]))

# #Consumer Opinion Surveys: Confidence Indicators: Composite Indicators: OECD Indicator for the United States
# COS = ts_fred("CSCICP03USM665S")
# COS = xts(COS[, 3], order.by = as.Date(COS[, 2]))
# print(paste("End of COS", ts_summary(TM)$end, sep = ": "))

# #Business Tendency Surveys for Manufacturing: Confidence Indicators: Composite Indicators: OECD Indicator for the United States
# BTS = ts_fred("BSCICP03USM665S")
# BTS = xts(BTS[, 3], order.by = as.Date(BTS[, 2]))
# print(paste("End of BTS", ts_summary(BTS)$end, sep = ": "))

# #Business Tendency Surveys for Manufacturing: Export Order Books or Demand: Level: European Commission Indicator for the United States
# OB = ts_fred("BSXRLV02USM086S")
# OB = xts(OB[, 3], order.by = as.array.default(OB[, 2]))
# print(paste("End of OB", ts_summary(TLP)$end, sep = ": "))

# #Total construction spending
# TCS = ts_fred("TTLCONS")
# TCS = xts(TCS[, 3], order.by = as.Date(TCS[, 2]))
# print(paste("End of TLP", ts_summary(TLP)$end, sep = ": "))

#Initial Plotting
#Plotting Industrial production. We can see that it is not stationary
ts_ggplot(log(IP))

#Plotting PCE
ts_ggplot(log(PCE))

#Plotting TLP
ts_ggplot(TLP)

#Plotting TM
ts_ggplot(log(TM))

#Plotting SP
ts_ggplot(SP)

# #Plotting COS
# ts_ggplot(COS)

# #Plotting BTS
# ts_ggplot(BTS)

# #Plotting OB
# ts_ggplot(OB)

# #Plotting TCS
# ts_ggplot(TCS)

# #Plotting IPNCCONGD
# ts_ggplot(log(IPNCCONGD))

#Plotting month on month growth rates if it has a trend. Roughly similar to first log-differences.
ts_ggplot(ts_pc(IP))
ts_ggplot(ts_pc(PCE))
ts_ggplot(ts_pc(TM))
# ts_ggplot(ts_pc(TCS))
# ts_ggplot(ts_pc(IPNCCONGD))

# Shorten all series to start in xxxx-xx-xx
myStart = "1992-01-01"

IP  = ts_span(IP, start = myStart)
GDP  = ts_span(GDP, start = myStart)
PCE = ts_span(PCE, start = myStart)
TLP = ts_span(TLP, start = myStart)
TM = ts_span(TM, start = myStart)
SP = ts_span(TS, start = myStart)
# COS = ts_span(COS, start = myStart)
# BTS = ts_span(BTS, start = myStart)
# OB = ts_span(OB, start = myStart)
# TCS = ts_span(TCS, start = myStart)
# IPNCCONGD = ts_span(IPNCCONGD, start = myStart)

#3) Making series with a trend stationary --------------------------------------
#-------------------------------------------------------------------------------

#Augmented Dickey-FUller test. First one clearly non-stationary. Log differences stationary.
uRootIP = CADFtest(log(IP), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootIP)

uRootIPd = CADFtest(ts_diff(log(IP)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIPd)

uRootPCE = CADFtest(log(PCE), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootPCE)

uRootPCEd = CADFtest(ts_diff(log(PCE)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootPCEd)

uRootTM = CADFtest(log(TM), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootTM)

uRootTMd = CADFtest(ts_diff(log(TM)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootTMd)

# uRootIPNCCONGD = CADFtest(log(IPNCCONGD), max.lag.y = 10, type = "trend", criterion = "BIC")
# summary(uRootIPNCCONGD)
# 
# uRootIPNCCONGDd = CADFtest(ts_diff(log(IPNCCONGD)), max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(uRootIPNCCONGDd)

# uRootCOS = CADFtest(COS, max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(uRootCOS)

# uRootTCS = CADFtest(log(TCS), max.lag.y = 10, type = "trend", criterion = "BIC")
# summary(uRootTCS)
# 
# uRootTCSd = CADFtest(ts_diff(log(TCS)), max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(uRootTCSd)

#Making series stationary
dIP = ts_diff(log(IP))
ts_ggplot(dIP)

dPCE = ts_diff(log(PCE))
ts_ggplot(dPCE)

dTM = ts_diff(log(TM))
ts_ggplot(dTM)

# dIPNCCONGD = ts_diff(log(IPNCCONGD))
# ts_ggplot(dIPNCCONGD)

# dTCS = ts_diff(log(TCS))
# ts_ggplot(dTCS)

#4) CCF and pre-whitening ------------------------------------------------------
#-------------------------------------------------------------------------------

#Examine lead-lag with CCF. IP
dIPq = ts_frequency(dIP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(dIPq), ts_ts(GDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter.
ModelGDP = auto.arima(GDP, max.p = 5, max.q = 5, ic = c("bic"))
ModeldIP  = auto.arima(dIPq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModeldIP)), ts_ts(resid(ModelGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF.PCE
dPCEq = ts_frequency(dPCE, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(dPCEq), ts_ts(GDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between PCE and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter. Leading one quarter.
ModeldPCE  = auto.arima(dPCEq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModeldPCE)), ts_ts(resid(ModelGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between PCE and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF.TLP
TLPq = ts_frequency(TLP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(TLPq), ts_ts(GDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between TLP and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter.
ModelTLP  = auto.arima(TLPq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelTLP)), ts_ts(resid(ModelGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between TLP and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF.TM
dTMq = ts_frequency(dTM, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(dTMq), ts_ts(GDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter.
ModeldTM  = auto.arima(dTMq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModeldTM)), ts_ts(resid(ModelGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between TM and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. SP
SPq = ts_frequency(SP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(SPq), ts_ts(GDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between SP and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Leading 2 quarters
ModelSP  = auto.arima(SPq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelSP)), ts_ts(resid(ModelGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between SP and GDP growth", subtitle = "Quarterly")
p

# #Examine lead-lag with CCF. Coincident indicator. TLP
# COSq = ts_frequency(COS, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(COSq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between COS and GDP growth", subtitle = "Quarterly")
# p
# 
# #Pre-whitening data. Lagging 4 quarters.
# ModelCOS  = auto.arima(COSq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelCOS)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between COS and GDP growth", subtitle = "Quarterly")
# p


# #Examine lead-lag with CCF. Coincident indicator. BTS
# BTSq = ts_frequency(BTS, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(BTSq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between BTS and GDP growth", subtitle = "Quarterly")
# p
# 
# #Pre-whitening data. No lead
# ModelBTS  = auto.arima(BTSq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelBTS)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between BTS and GDP growth", subtitle = "Quarterly")
# p


# #Examine lead-lag with CCF. Coincident indicator. OB
# OBq = ts_frequency(OB, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(OBq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between OB and GDP growth", subtitle = "Quarterly")
# p
# 
# #Pre-whitening data. Lag of one quarter. No lead.
# ModelOB  = auto.arima(OBq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelOB)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between OB and GDP growth", subtitle = "Quarterly")
# p


# #Examine lead-lag with CCF. Coincident indicator. TCS
# dTCSq = ts_frequency(dTCS, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(dTCSq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between TCS and GDP growth", subtitle = "Quarterly")
# p
# 
# #Pre-whitening data.No correlation.
# dModelTCS  = auto.arima(dTCSq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(dModelTCS)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between TCS and GDP growth", subtitle = "Quarterly")
# p


# #Examine lead-lag with CCF. IP non-durable goods
# dIPNCCONGDq = ts_frequency(dIPNCCONGD, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(dIPNCCONGDq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between industrial production: non-durable goods and GDP growth", subtitle = "Quarterly")
# p
# 
# #Pre-whitening data. Lead of one quarter. Lagging one quarter
# ModeldIPNCONGD  = auto.arima(dIPNCCONGDq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModeldIPNCONGD)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between industrial production: non-durable goods and GDP growth", subtitle = "Quarterly")
# p

