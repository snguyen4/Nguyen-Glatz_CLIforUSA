
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


# #Plotting TLP
# ts_ggplot(TLP)

# #Plotting TM
# ts_ggplot(log(TM))

# #Plotting OB
# ts_ggplot(OB)

# #Plotting IPNCCONGD
# ts_ggplot(log(IPNCCONGD))

# ts_ggplot(ts_pc(TM))
# ts_ggplot(ts_pc(IPNCCONGD))


# TLP = ts_span(TLP, start = myStart)
# TM = ts_span(TM, start = myStart)
# OB = ts_span(OB, start = myStart)
# IPNCCONGD = ts_span(IPNCCONGD, start = myStart)

# preTLP = ts_span(TLP, end = preCovid)
# prePCE = ts_span(PCE, end = preCovid)
# preIP = ts_span(IP, end = preCovid)



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


# dTM = ts_diff(log(TM))
# ts_ggplot(dTM)

# dIPNCCONGD = ts_diff(log(IPNCCONGD))
# ts_ggplot(dIPNCCONGD)


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



