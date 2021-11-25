

# Correct X13 Path if wrong format
X13Path <- Sys.getenv("X13_PATH")
Sys.setenv(X13_PATH = gsub("\\\\\\\\", "//", X13Path))
#checkX13()


# Make output directory forder
makeOutDir <- function(mainDir, outDir){
  
  if (file.exists(outDir)){
    setwd(file.path(mainDir, outDir))
  } else {
    dir.create(file.path(mainDir, outDir))
  }
  return(paste(mainDir, outDir , sep="")) # combines the stringe mainDir and outDir with seperation "" (i.e. w/o any separation)
}


ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}

normalize <- function(x){
  x_norm = (x-mean(x, na.rm =TRUE))/sd(x, na.rm =TRUE)
  return(x_norm)
}

getForecastVariance <- function(fcst){
  # Function to extract forecast error variance from a forecast object
  # CI lower = y(t+h|t)-1.96*sig(h)
  # Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
  # Get exact percentile (1.96 yields basically the same)
  
  z957 = qnorm(0.975, 0, 1)
  sigh2 = ((fcst$lower[,"95%"]-fcst$mean)/(-z957))^2
  return(sigh2)
}

nameMin <- function(Matx){
  # This is a useful function that returns the column and row names of a matrix
  # for the minimum value of the matrix
  
  ind <- which(Matx == min(Matx), arr.ind = TRUE)
  cname <- colnames(Matx)[[ind[2]]]
  rname <- rownames(Matx)[[ind[1]]]
  return(c(rname, cname))
  
}

ts_cumsum <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(cumsum)(x))
}
ts_rowMeans <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(rowMeans)(x))
}
plotACF <- function(x,lag.max){
  
  x <- na.omit(x)
  
  # Do CCF (see Neusser ch. 12.1)
  CrossC       <- acf(as.numeric(x), lag.max = lag.max, plot = FALSE)
  CrossC$ciu   <- CrossC$acf
  CrossC$cil   <- CrossC$acf
  CrossC$ciu[] <- 1.96*CrossC$n.used^(-1/2)
  CrossC$cil[] <- -1.96*CrossC$n.used^(-1/2)
  
  Data <- data.frame(CrossC$lag, CrossC$acf, CrossC$cil, CrossC$ciu)
  colnames(Data) <- c("s", "acf", "lower", "upper")
  DataCI <- Data[,c(1, 3, 4)]
  p <- ggplot(Data, aes(x = s, y = acf)) + geom_bar(stat='identity' , width=0.2)
  p <- p + geom_line(data=DataCI, aes(x=s, y=lower), colour="blue", linetype = "dashed")
  p <- p + geom_line(data=DataCI, aes(x=s, y=upper), colour="blue", linetype = "dashed")
  return(p)
}

plotCCF <- function(x, y, lag.max){
  
  temp <- ts_c(x, y)
  temp <- na.omit(temp)
  x <- temp[, 1]
  y <- temp[, 2]
  
  # Do CCF (see Neusser ch. 12.1)
  CrossC       <- ccf(as.numeric(x), as.numeric(y), lag.max = lag.max, plot = FALSE)
  CrossC$ciu   <- CrossC$acf
  CrossC$cil   <- CrossC$acf
  CrossC$ciu[] <- 1.96*CrossC$n.used^(-1/2)
  CrossC$cil[] <- -1.96*CrossC$n.used^(-1/2)
  
  Data <- data.frame(CrossC$lag, CrossC$acf, CrossC$cil, CrossC$ciu)
  colnames(Data) <- c("s", "ccf", "lower", "upper")
  DataCI <- Data[,c(1, 3, 4)]
  p <- ggplot(Data, aes(x = s, y = ccf)) + geom_bar(stat='identity' , width=0.2)
  p <- p + geom_line(data=DataCI, aes(x=s, y=lower), colour="blue", linetype = "dashed")
  p <- p + geom_line(data=DataCI, aes(x=s, y=upper), colour="blue", linetype = "dashed")
  return(p)
}


ggLayout <- function(p) {
  p <- p + theme_minimal()+ylab("")+xlab("")+
    ggplot2::scale_color_brewer(palette = "Dark2")+
    theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(fill=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
    theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+
    theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank())+
    theme(plot.title = element_text(face = "bold", size = 10), plot.subtitle = element_text(size = 9), text=element_text(size=9))
  return(p)
}

ggColor2 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=.5)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5))
  return(p)
}


ggColor3 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=.5)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("tomato1", "firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5, 1))
  return(p)
}

ggColorMany <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=.5) + scale_color_grey(start = 0.8, end = 0.2) +
    theme(legend.position = "none")
  return(p)
}

addLines <- function(p, myLines, myLabels, yInter, hor){
  if(missing(hor)) {
    hor = "vert" 
  }
  
  myLines <- as.Date(myLines)
  p <- p + geom_vline(xintercept=myLines , colour="black", size = .5, alpha = 0.5) 
  for(i in 1:length(myLines)){
    
    if(hor == "hor"){
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  alpha = 0.7, hjust = -.3)
    }
    else{
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  angle=90, alpha = 0.7, vjust = -1, hjust = 0)
    }
    
    
  }
  return(p)
}