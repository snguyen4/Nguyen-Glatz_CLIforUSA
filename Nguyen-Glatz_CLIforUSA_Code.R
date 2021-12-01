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
library(missMDA)

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

#Leading Indicators OECD: Component series: Share prices: Normalised for the United States
SP = ts_fred("USALOCOSPNOSTSAM")
SP = xts(SP[, 3], order.by = as.Date(SP[, 2]))

#Business Tendency Surveys for Manufacturing: Confidence Indicators: Composite Indicators: 
#European Commission and National Indicators for the United States
BTS = ts_fred("BSCICP02USM460S")
BTS = xts(BTS[, 3], order.by = as.Date(BTS[, 2]))

#Total construction spending
TCS = ts_fred("TTLCONS")
TCS = xts(TCS[, 3], order.by = as.Date(TCS[, 2]))

#Consumer Opinion Surveys: Confidence Indicators: Composite Indicators: OECD Indicator for the United States
COS = ts_fred("CSCICP03USM665S")
COS = xts(COS[, 3], order.by = as.Date(COS[, 2]))

#Economic Policy Uncertainty Index for United States
EPUI = ts_fred("USEPUINDXD")
EPUI = xts(EPUI[, 3], order.by =  as.Date(EPUI[, 2]))

#CBOE Volatility Index: VIX
VIX = ts_fred("VIXCLS")
VIX = xts(VIX[, 3], order.by =  as.Date(VIX[, 2]))

#ICE BofA US High Yield Index Option-Adjusted Spread
OAS = ts_fred("BAMLH0A0HYM2")
OAS = xts(OAS[, 3], order.by =  as.Date(OAS[, 2]))

#St. Louis Fed Financial Stress Index
STFLI2 = ts_fred("STLFSI2")
STFLI2 = xts(STFLI2[, 3], order.by =  as.Date(STFLI2[, 2]))

# From http://www.nber.org/cycles.html
NBERREC = read.table(textConnection(
  "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2021-02-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)
NBERREC = subset(NBERREC, Peak >= as.Date("1980-01-01") )

#Initial Plotting
#Plotting Industrial Production. We can see that it is not stationary
ts_ggplot(log(IP))

#Plotting Personal Consumption Expenditures
ts_ggplot(log(PCE))

#Plotting SP
ts_ggplot(SP)

#Plotting BTS
ts_ggplot(BTS)

#Plotting TCS
ts_ggplot(TCS)

#Plotting COS
ts_ggplot(COS)

#Plotting month on month growth rates if it has a trend. Roughly similar to first log-differences.
ts_ggplot(ts_pc(IP))
ts_ggplot(ts_pc(PCE))
ts_ggplot(ts_pc(TCS))

# Shorten all series to start in xxxx-xx-xx
myStart = "1980-01-01"

IP  = ts_span(IP, start = myStart)
GDP  = ts_span(GDP, start = myStart)
PCE = ts_span(PCE, start = myStart)
SP = ts_span(SP, start = myStart)
BTS = ts_span(BTS, start = myStart)
TCS = ts_span(TCS, start = myStart)
COS = ts_span(COS, start = myStart)

#Pre-COVID
preCovid = "2019-12-01"
preGDP = ts_span(GDP, end = preCovid)
preSP = ts_span(SP, end = preCovid)
preBTS = ts_span(BTS, end = preCovid)
preTCS = ts_span(TCS, end = preCovid)
preCOS = ts_span(COS, end = preCovid)

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

uRootTCS = CADFtest(log(TCS), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootTCS)

uRootTCSd = CADFtest(ts_diff(log(TCS)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootTCSd)

#Making series stationary
dIP = ts_diff(log(IP))
ts_ggplot(dIP)

dPCE = ts_diff(log(PCE))
ts_ggplot(dPCE)

dTCS = ts_diff(log(TCS))
ts_ggplot(dTCS)

#PreCovid
predIP = ts_span(dIP, end = preCovid)
predPCE = ts_span(dPCE, end = preCovid)
predTCS = ts_span(dTCS, end = preCovid)

#4) CCF and pre-whitening ------------------------------------------------------
#-------------------------------------------------------------------------------

#Examine lead-lag with CCF. IP
predIPq = ts_frequency(predIP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(predIPq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter. Changes if start date changes. Lagging if 1992.
ModelpreGDP = auto.arima(preGDP, max.p = 5, max.q = 5, ic = c("bic"))
ModelpredIP  = auto.arima(predIPq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelpredIP)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between industrial production and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF.PCE
predPCEq = ts_frequency(predPCE, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(predPCEq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between personal consumption rxpenditures and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Lead of one quarter. Leading one quarter. Doesn't change after pre-whitening.
ModelpredPCE  = auto.arima(predPCEq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelpredPCE)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between personal consumption expenditures and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. SP
preSPq = ts_frequency(preSP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preSPq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between share prices and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Leading 1 quarter.
ModelpreSP  = auto.arima(preSPq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelpreSP)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between share prices and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. BTS
preBTSq = ts_frequency(preBTS, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preBTSq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between business tendency surveys for manufacturing and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Leading 1 quarter.
ModelpreBTS  = auto.arima(preBTSq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelpreBTS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between business tendency surveys for manufacturing and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. Coincident indicator. TCS
predTCSq = ts_frequency(predTCS, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(predTCSq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between total construction spending and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data.No correlation.
predModelTCS  = auto.arima(predTCSq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(predModelTCS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between total construction spending and GDP growth", subtitle = "Quarterly")
p

#Examine lead-lag with CCF. COS.
preCOSq = ts_frequency(preCOS, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preCOSq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between consumer opinion surveys and GDP growth", subtitle = "Quarterly")
p

#Pre-whitening data. Leading one quarter
ModelpreCOS  = auto.arima(preCOSq, max.p = 5, max.q = 5, ic = c("bic"))
p = plotCCF(ts_ts(resid(ModelpreCOS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Pre-whitened Cross-correlation between consumer opinion surveys and GDP growth", subtitle = "Quarterly")
p

#5) Transformation for CLI------------------------------------------------------
#-------------------------------------------------------------------------------

# Summary. Not really sure for publication-
# Indicator     Lead/lag      Cyclicality   Publication     Notes
# Industrial P. Lead 1Q       Pro           Immediate       None
# PCE           Lead 1Q       Pro           Immediate       None
# Share Prices  Lead 1Q       Pro           Immediate       None
# B.T survey    Lead 1Q       Pro           Immediate       None
# Cons. Os. S.  Lead 1Q       Pro           Immediate       None

#Transforming to monthly
IPm = lag(ts_frequency(dIP, to = "month", aggregate= "mean", na.rm = T), 0)    
PCEm = lag(ts_frequency(dPCE, to = "month", aggregate= "mean", na.rm = T), 0)
SPm = lag(ts_frequency(SP, to = "month", aggregate= "mean", na.rm = T), 0)
BTSm = lag(ts_frequency(BTS, to = "month", aggregate= "mean", na.rm = T), 0)
COSm = lag(ts_frequency(COS, to = "month", aggregate = "mean", na.rm = T), 0)

#Normalize everything
IPm = normalize(IPm)
PCEm = normalize(PCEm)
BTSm = normalize(BTSm)
SPm = normalize(SPm)
COSm = normalize(COSm)

p = ts_ggplot(
  "Industrial Production" = IPm,
  "Personal Consumption Expenditure" = PCEm,
  "Share Prices" = SPm,
  "Business Tendency Surveys: Manufacturing" = BTSm,
  "Consumer Opinion Surveys" = COSm,
  title = "Selection of indicators",
  subtitle = "monthly, normalized"
)
p = ggLayout(p)
p

#Computing equally weighted indicator
CLI = rowSums(cbind(IPm, PCEm, BTSm, SPm, COSm), na.rm = T)/5
Dates = seq(as.Date("1980-01-01"), length = length(CLI), by = "months")
CLI = xts(CLI, order.by = Dates)

g = ggplot(CLI) + geom_line(aes(x = index(CLI), y = CLI)) + theme_minimal()
g = g + geom_rect(data=NBERREC, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill ="grey", alpha = 0.5)
g = g + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
g = g + xlab("") +  ggtitle("CLI simple average and NBER recessions")
g = ggLayout(g)
g

#6) Principal component analysis and comparison with indicator from App 3 -----
#-------------------------------------------------------------------------------

#Principal component analysis
X = cbind(IPm, PCEm, BTSm, SPm, COSm)
Ximp = imputePCA(as.matrix(X), ncp = 1)
PCX = prcomp(Ximp$completeObs)
CLIf = xts(PCX$x[,"PC1"], order.by = as.Date(index(X)))

g = ggplot(CLIf) + geom_line(aes(x = index(CLIf), y = CLIf))
g = g + geom_rect(data = NBERREC, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill = "grey", alpha = 0.5)
g = g + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
g = g + xlab("") +  ggtitle("CLI factor model and NBER recessions")
g = ggLayout(g)
g

#Computing correlation between CLI and CLIf. High correlation normal?
cor(CLI, CLIf)

# Compare indicator with App3's indicator
#Loading CLI from App3
source("CLIApp3.R")

#Plotting and comparing CLIs
g1 = ts_ggplot(
  "CLI average" =  normalize(CLI),
  "CLI factor" =  normalize(CLIf), 
  "CLIApp3 average" = normalize(CLIApp3),
  "CLIApp3 factor" = normalize(CLIfApp3),
  title = "Comparison between leading indicators",
  subtitle = "Normalized"
)
g1 = ggLayout(g1)
g1

#In-Sample evaluation

#Regressing GDP growth on indicator
CLIq = ts_frequency(CLI, to = "quarter", aggregate= "mean", na.rm = T)
CLIq = ts_span(CLIq, end = "2021-07-01")
lmCLI = lm(GDP ~ CLIq) 
summary(lmCLI) 

#Regressing GDP growth on App3 indicator
CLIApp3q = ts_frequency(CLIApp3, to = "quarter", aggregate= "mean", na.rm = T)
CLIApp3q = ts_span(CLIApp3q, end = "2021-07-01")
lmCLIApp3 = lm(GDP ~ CLIApp3q) 
summary(lmCLIApp3) 

