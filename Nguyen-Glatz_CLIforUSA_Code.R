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
library(ggpubr)

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
ts_ggplot(GDP) +
  labs(title = "GDP growth")

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

#Consumer Opinion Surveys: Confidence Indicators: Composite Indicators: OECD Indicator for the United States
COS = ts_fred("CSCICP03USM665S")
COS = xts(COS[, 3], order.by = as.Date(COS[, 2]))

#Economic Policy Uncertainty Index for United States
EPUI = ts_fred("USEPUINDXD")
EPUI = xts(EPUI[, 3], order.by =  as.Date(EPUI[, 2]))

#CBOE Volatility Index: VIX
VIX = ts_fred("VIXCLS")
VIX = xts(VIX[, 3], order.by =  as.Date(VIX[, 2]))

#Initial Plotting
#Plotting Industrial Production. We can see that it is not stationary
ts_ggplot(log(IP)) +
  labs(title = "Industrial Production")
 
#Plotting Personal Consumption Expenditures
ts_ggplot(log(PCE)) +
  labs(title = "Personal Consumption Ependiture")

#Plotting SP
ts_ggplot(SP) +
  labs(title = "Share Prices")

#Plotting BTS
ts_ggplot(BTS) +
  labs(title = "Business Tendency Surveys: Manufacturing")

#Plotting COS
ts_ggplot(COS) +
  labs(title = "Consumer Opinion Surveys")

#Plotting EPUI. Not seasonally adjusted.
ts_ggplot(EPUI) +
  labs(title = "Economic Policy Uncertainty Index for United States")

#Plotting VIX
ts_ggplot(VIX) +
  labs(title = "CBOE Volatility Index: VIX")

#Plotting month on month growth rates if it has a trend. Roughly similar to first log-differences.
ts_ggplot(ts_pc(IP))
ts_ggplot(ts_pc(PCE))

#Removing seasonality

#Seasonal period is too large. In order to use SEATS, the frequency had to be changed to monthly 
#Economic Policy Uncertainty Index for United States
EPUI = ts_frequency(EPUI, to = "month", aggregate = "mean", na.rm = TRUE)
EPUI = seas(ts_ts(EPUI))
EPUI = trend(EPUI)
EPUI = as.xts(EPUI)
EPUI = ts_frequency(EPUI, to = "month", aggregate = "mean", na.rm = TRUE) #Just for the date format

ts_ggplot(EPUI) +
  labs(title = "Economic Policy Uncertainty Index for United States", subtitle = "Seasonally adjusted")

#CBOE Volatility Index: VIX
VIX = ts_frequency(VIX, to = "month", aggregate = "mean", na.rm = TRUE)
VIX = seas(ts_ts(VIX))
VIX = trend(VIX)
VIX = as.xts(VIX)
VIX = ts_frequency(VIX, to = "month", aggregate = "mean", na.rm = TRUE) #Just for the date format

ts_ggplot(VIX) +
  labs(title = "CBOE Volatility Index: VIX", subtitle = "Seasonally adjusted")

#Shortening all series to start in xxxx-xx-xx
myStart = "1990-01-01"

IP  = ts_span(IP, start = myStart)
GDP  = ts_span(GDP, start = myStart)
PCE = ts_span(PCE, start = myStart)
SP = ts_span(SP, start = myStart)
BTS = ts_span(BTS, start = myStart)
COS = ts_span(COS, start = myStart)
EPUI = ts_span(EPUI, start = myStart)
VIX = ts_span(VIX, start = myStart)

#Pre-COVID
preCovid = "2019-12-01"
preGDP = ts_span(GDP, end = preCovid)
preSP = ts_span(SP, end = preCovid)
preBTS = ts_span(BTS, end = preCovid)
preCOS = ts_span(COS, end = preCovid)
preEPUI = ts_span(EPUI, end = preCovid)
preVIX = ts_span(VIX, end = preCovid)

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
NBERREC = subset(NBERREC, Peak >= as.Date(myStart) )

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

#Making series stationary
dIP = ts_diff(log(IP))
ts_ggplot(dIP) +
  labs(title = "Industrial production", subtitle = "Log differences")

dPCE = ts_diff(log(PCE))
ts_ggplot(dPCE) +
  labs(title = "Personal consumption expenditures", subtitle = "Log differences")

#PreCovid
predIP = ts_span(dIP, end = preCovid)
predPCE = ts_span(dPCE, end = preCovid)

#4) CCF and pre-whitening ------------------------------------------------------
#-------------------------------------------------------------------------------

#Examine lead-lag with CCF. Industrial production.
predIPq = ts_frequency(predIP, to = "quarter", aggregate = "mean", na.rm = TRUE)
p = plotCCF(ts_ts(predIPq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Industrial Production and GDP Growth", subtitle = "Quarterly")
p

#Coincident or leading. not very clear because of autocorrelation

#Pre-whitening data. 
ModelpreGDP = auto.arima(preGDP, max.p = 5, max.q = 5, ic = c("bic"))
ModelpredIP  = auto.arima(predIPq, max.p = 5, max.q = 5, ic = c("bic"))
p1 = plotCCF(ts_ts(resid(ModelpredIP)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p1 = ggLayout(p1)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Industrial Production", subtitle = "Quarterly")
p1

#Concurrent and leading by one quarter. Pro-cyclical indicator.

#Examine lead-lag with CCF.Personal consumption expenditures.
predPCEq = ts_frequency(predPCE, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(predPCEq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Personal Consumption Expenditures and GDP Growth", subtitle = "Quarterly")
p

#Coincident indicator

#Pre-whitening data. 
ModelpredPCE  = auto.arima(predPCEq, max.p = 5, max.q = 5, ic = c("bic"))
p2 = plotCCF(ts_ts(resid(ModelpredPCE)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p2 = ggLayout(p2)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Personal Consumption Expenditures", subtitle = "Quarterly")
p2

#Coincident and lead of one quarter. Pro-cyclical indicator.

#Examine lead-lag with CCF. SP
preSPq = ts_frequency(preSP, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preSPq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Share Prices and GDP Growth", subtitle = "Quarterly")
p

#coincident indicator. Could be leading or lagging.

#Pre-whitening data.
ModelpreSP  = auto.arima(preSPq, max.p = 5, max.q = 5, ic = c("bic"))
p3 = plotCCF(ts_ts(resid(ModelpreSP)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p3 = ggLayout(p3)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Share Prices", subtitle = "Quarterly")
p3

#Coincident and lead of one quarter. S = -1 is higher, so we will lag it by 3 months. Pro-cyclical indicator.

#Examine lead-lag with CCF. BTS
preBTSq = ts_frequency(preBTS, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preBTSq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Business Tendency Surveys for Manufacturing and GDP Growth", subtitle = "Quarterly")
p

#Coincident, maybe lagging.

#Pre-whitening data. 
ModelpreBTS  = auto.arima(preBTSq, max.p = 5, max.q = 5, ic = c("bic"))
p4 = plotCCF(ts_ts(resid(ModelpreBTS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p4 = ggLayout(p4)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Business Tendency Surveys", subtitle = "Quarterly")
p4

#It looks coincident, and slightly lagging, but barely. So we'll treat it as a coincident pro-cyclical indicator.

#Examine lead-lag with CCF. COS.
preCOSq = ts_frequency(preCOS, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preCOSq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Consumer Opinion Surveys and GDP Growth", subtitle = "Quarterly")
p

#Coincident or maybe lagging.

#Pre-whitening data. 
ModelpreCOS  = auto.arima(preCOSq, max.p = 5, max.q = 5, ic = c("bic"))
p5 = plotCCF(ts_ts(resid(ModelpreCOS)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p5 = ggLayout(p5)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Consumer Opinion Surveys", subtitle = "Quarterly")
p5

#Coincident indicator, pro-cyclical.

#Examine lead-lag with CCF. EPUI
preEPUIq = ts_frequency(preEPUI, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preEPUIq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between Economic Policy Uncertainty Index for United States and GDP Growth", subtitle = "Quarterly")
p

#Counter-cyclical, maybe lagging.

#Pre-whitening data. 
ModelpreEPUI  = auto.arima(preEPUIq, max.p = 5, max.q = 5, ic = c("bic"))
p6 = plotCCF(ts_ts(resid(ModelpreEPUI)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p6 = ggLayout(p6)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Economic Policy Uncertainty Index", subtitle = "Quarterly")
p6

#Counter-cyclical coincident indicator, barely lagging.

#Examine lead-lag with CCF. VIX
preVIXq = ts_frequency(preVIX, to = "quarter", aggregate= "mean", na.rm = TRUE)
p = plotCCF(ts_ts(preVIXq), ts_ts(preGDP), lag.max = 15)
p = ggLayout(p)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "Cross-correlation between CBOE Volatility Index: VIX and GDP Growth", subtitle = "Quarterly")
p

#Counter-cyclical, coincident, not clear.

#Pre-whitening data. lagging 2 quarters.
ModelpreVIX  = auto.arima(preVIXq, max.p = 5, max.q = 5, ic = c("bic"))
p7 = plotCCF(ts_ts(resid(ModelpreVIX)), ts_ts(resid(ModelpreGDP)), lag.max = 15)
p7 = ggLayout(p7)+ ylab("Cross correlation X(t+s), GDP(t)") +
  labs(title = "CBOE Volatility Index: VIX", subtitle = "Quarterly")
p7

#Coincident, counter cyclical with a lag of 2 quarters. Will be treated as a coincident counter-cyclical indicator.

#Grouping plots
ggarrange(p1, p2, p3, p4, p5, p6, p7, 
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 2, nrow = 4)


#5) Transformation for CLI------------------------------------------------------
#-------------------------------------------------------------------------------

# Summary. Not really sure for publication-
# Indicator     Lead/lag      Cyclicality   Publication     Notes
# Industrial P. Coincident    Pro           -               None
# PCE           Coincident    Pro           -               None
# Share Prices  Lead 1Q       Pro           -               Lag 1Q to match GDP growth
# B.T survey    Coincident    Pro           -               None
# Cons. Op. S.  Coincident    Pro           -               None
# EPUI          Coincident    Counter       -               Multiply by -1
# VIX           Coincident    COunter       -               Multiply by -1

#Transforming to monthly
IPm = lag(ts_frequency(dIP, to = "month", aggregate= "mean", na.rm = T), 0)    
PCEm = lag(ts_frequency(dPCE, to = "month", aggregate= "mean", na.rm = T), 0)
SPm = lag(ts_frequency(SP, to = "month", aggregate= "mean", na.rm = T), 3)
BTSm = lag(ts_frequency(BTS, to = "month", aggregate= "mean", na.rm = T), 0)
COSm = lag(ts_frequency(COS, to = "month", aggregate = "mean", na.rm = T), 0)
EPUIm = -1 * lag(ts_frequency(EPUI, to = "month", aggregate = "mean", na.rm = T), 0)
VIXm = -1 * lag(ts_frequency(VIX, to = "month", aggregate = "mean", na.rm = T), 0)

#Normalize everything
IPm = normalize(IPm)
PCEm = normalize(PCEm)
BTSm = normalize(BTSm)
SPm = normalize(SPm)
COSm = normalize(COSm)
EPUIm = normalize(EPUIm)
VIXm = normalize(VIXm)

p = ts_ggplot(
  "Industrial Production" = IPm,
  "Personal Consumption Expenditure" = PCEm,
  "Share Prices" = SPm,
  "Business Tendency Surveys: Manufacturing" = BTSm,
  "Consumer Opinion Surveys" = COSm,
  "Economic Policy Uncertainty Index for United States" = EPUIm,
  "CBOE Votality Index: VIX" = VIXm,
  title = "Selection of indicators",
  subtitle = "monthly, normalized"
)
p = ggLayout(p)
p

#Computing equally weighted indicator
CLI = rowSums(cbind(IPm, PCEm, BTSm, SPm, COSm, EPUIm, VIXm), na.rm = T)/7
Dates = seq(as.Date("1990-01-01"), length = length(CLI), by = "months")
CLI = xts(CLI, order.by = Dates)

g = ggplot(CLI) + geom_line(aes(x = index(CLI), y = CLI)) + theme_minimal()
g = g + geom_rect(data=NBERREC, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill ="grey", alpha = 0.5)
g = g + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
g = g + xlab("") +  ggtitle("CLI Simple Average and NBER Recessions")
g = ggLayout(g)
g

#6) Principal component analysis and comparison with indicator from App 3 -----
#-------------------------------------------------------------------------------

#Principal component analysis
X = cbind(IPm, PCEm, BTSm, SPm, COSm, EPUIm, VIXm)
Ximp = imputePCA(as.matrix(X), ncp = 1)
PCX = prcomp(Ximp$completeObs)
CLIf = xts(PCX$x[,"PC1"], order.by = as.Date(index(X)))

g = ggplot(CLIf) + geom_line(aes(x = index(CLIf), y = CLIf))
g = g + geom_rect(data = NBERREC, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill = "grey", alpha = 0.5)
g = g + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
g = g + xlab("") +  ggtitle("CLI Factor Model and NBER Recessions")
g = ggLayout(g)
g

#Computing correlation between CLI and CLIf.
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
  title = "Comparison between Leading Indicators",
  subtitle = "Normalized"
)
g1 = ggLayout(g1)
g1

#Comparing CLIs. Simple average
g2 = ts_ggplot(
  "CLI average" =  normalize(CLI),
  "CLIApp3 average" = normalize(CLIApp3),
  title = "Comparison between Leading Indicators: Simple Average",
  subtitle = "Normalized"
)
g2 = ggLayout(g2)
g2

#Comparing CLIs. Factor model
g3 = ts_ggplot(
  "CLI factor" =  normalize(CLIf), 
  "CLIApp3 factor" = normalize(CLIfApp3),
  title = "Comparison between Leading Indicators: Factor Model",
  subtitle = "Normalized"
)
g3 = ggLayout(g3)
g3

#7) In-Sample evaluation -------------------------------------------------------
#-------------------------------------------------------------------------------

#Linear regression and comparing r^2--------------------------------------------

#Regressing GDP growth on indicator. Simple average.
CLIq = ts_frequency(CLI, to = "quarter", aggregate= "mean", na.rm = T)
CLIq = ts_span(CLIq, end = "2021-07-01")
lmCLI = lm(GDP ~ CLIq) 
summary(lmCLI) 

#Regressing GDP growth on App3 indicator. Simple average.
CLIApp3q = ts_frequency(CLIApp3, to = "quarter", aggregate= "mean", na.rm = T)
CLIApp3q = ts_span(CLIApp3q, start = "1990-01-01", end = "2021-07-01")
lmCLIApp3 = lm(GDP ~ CLIApp3q) 
summary(lmCLIApp3) 

#Regressing GDP growth on indicator. Factor model.
CLIfq = ts_frequency(CLIf, to = "quarter", aggregate= "mean", na.rm = T)
CLIfq = ts_span(CLIfq, end = "2021-07-01")
lmCLIf = lm(GDP ~ CLIfq) 
summary(lmCLIf) 

#Regressing GDP growth on App3 indicator. Factor model.
CLIfApp3q = ts_frequency(CLIfApp3, to = "quarter", aggregate= "mean", na.rm = T)
CLIfApp3q = ts_span(CLIfApp3q, start = "1990-01-01", end = "2021-07-01")
lmCLIfApp3 = lm(GDP ~ CLIfApp3q) 
summary(lmCLIfApp3) 

#Cross-correlation test---------------------------------------------------------

#Simple average
ModelCLIq  = auto.arima(CLIq, max.p = 5, max.q = 5, ic = c("bic"))
ModelCLIApp3q  = auto.arima(CLIApp3q, max.p = 5, max.q = 5, ic = c("bic"))

p8 = plotCCF(ts_ts(resid(ModelCLIq)), ts_ts(resid(ModelCLIApp3q)), lag.max = 15)
p8 = ggLayout(p8)+ ylab("Cross correlation X(t+s), CLIApp3(t)") +
  labs(title = "CLI: Simple Average", subtitle = "Quarterly")
p8

#Factor model
ModelCLIfq  = auto.arima(CLIfq, max.p = 5, max.q = 5, ic = c("bic"))
ModelCLIfApp3q  = auto.arima(CLIfApp3q, max.p = 5, max.q = 5, ic = c("bic"))

p9 = plotCCF(ts_ts(resid(ModelCLIfq)), ts_ts(resid(ModelCLIfApp3q)), lag.max = 15)
p9 = ggLayout(p9)+ ylab("Cross correlation X(t+s), CLIfApp3(t)") +
  labs(title = "CLI: Factor Model", subtitle = "Quarterly")
p9

ggarrange(p8, p9, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


