
# #Early Estimate of Quarterly ULC Indicators: Total Labor Productivity for the United States. Quarterly bad?
# TLP = ts_fred("ULQELP01USQ657S")
# TLP = xts(TLP[, 3], order.by = as.Date(TLP[, 2]))

# #Manufacturers' New Orders: Total Manufacturing. Starts in 1992. Should I remove it or make all series start in 1992?
# TM = ts_fred("AMTMNO")
# TM = xts(TM[, 3], order.by = as.Date(TM[, 2]))

# #Industrial production: Non-durable consumer goods
# IPNCCONGD = ts_fred("IPNCONGD")
# IPNCCONGD = xts(IPNCCONGD[, 3], order.by = as.Date(IPNCCONGD[, 2]))

# #Business Tendency Surveys for Manufacturing: Confidence Indicators: Composite Indicators: OECD Indicator for the United States
# BTS = ts_fred("BSCICP03USM665S")
# BTS = xts(BTS[, 3], order.by = as.Date(BTS[, 2]))
# print(paste("End of BTS", ts_summary(BTS)$end, sep = ": "))

# #Business Tendency Surveys for Manufacturing: Export Order Books or Demand: Level: European Commission Indicator for the United States
# OB = ts_fred("BSXRLV02USM086S")
# OB = xts(OB[, 3], order.by = as.array.default(OB[, 2]))
# print(paste("End of OB", ts_summary(TLP)$end, sep = ": "))

# #ICE BofA US High Yield Index Option-Adjusted Spread
# OAS = ts_fred("BAMLH0A0HYM2")
# OAS = xts(OAS[, 3], order.by =  as.Date(OAS[, 2]))

# #St. Louis Fed Financial Stress Index
# STFLI2 = ts_fred("STLFSI2")
# STFLI2 = xts(STFLI2[, 3], order.by =  as.Date(STFLI2[, 2]))

#-------------------------------------------------------------------------------

# #Plotting TCS
# ts_ggplot(TCS) +
#   labs(title = "Total construction spending")

# #Plotting OAS
# ts_ggplot(OAS) +
#   labs(title = "ICE BofA US High Yield Index Option-Adjusted Spread")

# #Plotting STFLI2
# ts_ggplot(STFLI2) +
#   labs(title = "St. Louis Fed Financial Stress Index")

# #Plotting TLP
# ts_ggplot(TLP)

# #Plotting TM
# ts_ggplot(log(TM))

# #Plotting OB
# ts_ggplot(OB)

# #Plotting IPNCCONGD
# ts_ggplot(log(IPNCCONGD))

# ts_ggplot(ts_pc(TCS))
# ts_ggplot(ts_pc(TM))
# ts_ggplot(ts_pc(IPNCCONGD))

#-------------------------------------------------------------------------------

# #ICE BofA US High Yield Index Option-Adjusted Spread
# OAS = ts_frequency(OAS, to = "month", aggregate = "mean", na.rm = TRUE)
# OAS = seas(ts_ts(OAS))
# OAS = trend(OAS)
# OAS = as.xts(OAS)
# OAS = ts_frequency(OAS, to = "month", aggregate = "mean", na.rm = TRUE) #Just for the date format
#
# ts_ggplot(OAS) +
#   labs(title = "ICE BofA US High Yield Index Option-Adjusted Spread", subtitle = "Seasonally adjusted")

# #St. Louis Fed Financial Stress Index
# STFLI2 = ts_frequency(STFLI2, to = "month", aggregate = "mean", na.rm = TRUE)
# STFLI2 = seas(ts_ts(STFLI2))
# STFLI2 = trend(STFLI2)
# STFLI2 = as.xts(STFLI2)
# STFLI2 = ts_frequency(STFLI2, to = "month", aggregate = "mean", na.rm = TRUE) #Just for the date format
#
# ts_ggplot(STFLI2) +
#   labs(title = "St. Louis Fed Financial Stress Index", subtitle = "Seasonally adjusted")

#-------------------------------------------------------------------------------

# TLP = ts_span(TLP, start = myStart)
# TM = ts_span(TM, start = myStart)
# OB = ts_span(OB, start = myStart)
# IPNCCONGD = ts_span(IPNCCONGD, start = myStart)
# OAS = ts_span(OAS, start = myStart)
# STFLI2 = ts_span(STFLI2, start = myStart)
# TCS = ts_span(TCS, start = myStart)

# preTLP = ts_span(TLP, end = preCovid)
# prePCE = ts_span(PCE, end = preCovid)
# preIP = ts_span(IP, end = preCovid)
# preOAS = ts_span(OAS, end = preCovid)
# preSTFLI2 = ts_span(STFLI2, end = preCovid)
# preTCS = ts_span(TCS, end = preCovid)

#-------------------------------------------------------------------------------

# uRootTM = CADFtest(log(TM), max.lag.y = 10, type = "trend", criterion = "BIC")
# summary(uRootTM)

# uRootTMd = CADFtest(ts_diff(log(TM)), max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(uRootTMd)

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


# dTM = ts_diff(log(TM))
# ts_ggplot(dTM)

# dIPNCCONGD = ts_diff(log(IPNCCONGD))
# ts_ggplot(dIPNCCONGD)

# dTCS = ts_diff(log(TCS))
# ts_ggplot(dTCS) +
#   labs(title = "Total construction spending", subtitle = "Log differences")

#-------------------------------------------------------------------------------

# #Examine lead-lag with CCF.TLP
# preTLPq = ts_frequency(preTLP, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(preTLPq), ts_ts(preGDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between TLP and GDP growth", subtitle = "Quarterly")
# p

# #Pre-whitening data. Lead of one quarter. Doesn't change after pre-whitening. Not useful to pre-whiten data? No correlation pre covid?
# ModelpreTLP  = auto.arima(preTLPq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelpreTLP)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between TLP and GDP growth", subtitle = "Quarterly")
# p
#

# #Examine lead-lag with CCF.TM
# dTMq = ts_frequency(dTM, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(dTMq), ts_ts(GDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between TM and GDP growth", subtitle = "Quarterly")
# p
#
# #Pre-whitening data. Lead of one quarter.
# ModeldTM  = auto.arima(dTMq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModeldTM)), ts_ts(resid(ModelGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between TM and GDP growth", subtitle = "Quarterly")
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

# #Examine lead-lag with CCF. OAS
# preOASq = ts_frequency(preOAS, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(preOASq), ts_ts(preGDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between ICE BofA US High Yield Index Option-Adjusted Spread and GDP growth", subtitle = "Quarterly")
# p
#
# #Pre-whitening data. Leading 1 quarter.
# ModelpreOAS  = auto.arima(preOASq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelpreOAS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between ICE BofA US High Yield Index Option-Adjusted Spread and GDP growth", subtitle = "Quarterly")
# p

# #Examine lead-lag with CCF. STFLI2
# preSTFLI2q = ts_frequency(preSTFLI2, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(preSTFLI2q), ts_ts(preGDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between St. Louis Fed Financial Stress Index and GDP growth", subtitle = "Quarterly")
# p
#
# #Pre-whitening data. Lagging 2 quarters.
# ModelpreSTFLI2  = auto.arima(preSTFLI2q, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(ModelpreSTFLI2)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between St. Louis Fed Financial Stress Index and GDP growth", subtitle = "Quarterly")
# p

# #Examine lead-lag with CCF. Coincident indicator. TCS
# predTCSq = ts_frequency(predTCS, to = "quarter", aggregate= "mean", na.rm = TRUE)
# p = plotCCF(ts_ts(predTCSq), ts_ts(preGDP), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Cross-correlation between total construction spending and GDP growth", subtitle = "Quarterly")
# p
#
# #Pre-whitening data.No correlation if date starts in 1980. Leading of 9 quarters if date starts in 1997.
# predModelTCS  = auto.arima(predTCSq, max.p = 5, max.q = 5, ic = c("bic"))
# p = plotCCF(ts_ts(resid(predModelTCS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
# p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
#   labs(title = "Pre-whitened Cross-correlation between total construction spending and GDP growth", subtitle = "Quarterly")
# p

