#CLI App 3----------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Term Spread
TS = ts_fred('T10Y2Y')
TS = xts(TS[, 3], order.by = TS[, 2])

# Initial claims (initial unemployment insurance claims, weekly)
IC = ts_fred('ICSA')
IC = xts(IC[, 3], order.by = IC[, 2])

#Business tendency survey manufacturing
BT = ts_fred('BSPRTE02USM460S')
BT = xts(BT[, 3], order.by = BT[, 2])

#Consumer sentiment survey
CS = ts_fred('UMCSENT')
CS = (xts(CS[, 3], order.by = CS[, 2]))

#Series with all data from 1980 to current date for calculating the indicator
myStartApp3 = "1980-01-01"
TS  = ts_span(TS, start = myStartApp3)
IC  = ts_span(IC, start = myStartApp3)
BT  = ts_span(BT, start = myStartApp3)
CS  = ts_span(CS, start = myStartApp3)

#Transforming to monthly frequency
TSm = lag(ts_frequency(TS, to = "month", aggregate= "mean", na.rm = TRUE), 0)    
ICm = -1*lag(ts_frequency(IC, to = "month", aggregate= "mean", na.rm = TRUE), 0)
BTm = lag(ts_frequency(BT, to = "month", aggregate= "mean", na.rm = TRUE), 0)
CSm = lag(ts_frequency(CS, to = "month", aggregate= "mean", na.rm = TRUE), 3)

# Normalize and set the same start date for all
TSm = normalize(TSm)
ICm = normalize(ICm)
BTm = normalize(BTm)
CSm = normalize(CSm)

# Compute equally weighted indicator (where we ignore that some are missing)
CLIApp3 = rowSums(cbind(TSm, ICm, BTm, CSm), na.rm=TRUE)/4
Dates <- seq(as.Date("1980-01-01"), length = length(CLIApp3), by = "months")
CLIApp3 = xts(CLIApp3, order.by = Dates)

#PCA
X = cbind(TSm, ICm, BTm, CSm)
Ximp = imputePCA(as.matrix(X), ncp=1)
PCX = prcomp(Ximp$completeObs)
CLIfApp3 = -xts(PCX$x[,"PC1"], order.by=as.Date(index(X)))

