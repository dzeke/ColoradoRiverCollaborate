##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


Func_EnsembleTimeSeries <- function(ScenarioList, ScenarioName="ISM_Full"){
  
  scenario <- eval(parse(text = ScenarioList[1]))
  # scenario <- AR1
  
  
  ntraces <- ncol(scenario)-1    
  nyrs <- nrow(scenario)
  yr1 <- min(scenario[,1])
  yr2 <- max(scenario[,1])
  
  #### estimating quartiles and ranges of the ensemble ===========================================
  
  Range <- data.frame("Yr"=scenario[,1], "Max"=NA, "Q10"=NA, "Q25"=NA, "Median"=NA, "Q75"=NA, "Q90"=NA, "Min"=NA)
  
  library("matrixStats")
  Range$Max <- rowMaxs(as.matrix(scenario[,-1]))
  Range$Q10 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.1)
  Range$Q25 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.25)
  Range$Median <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.5)
  Range$Q75 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.75)
  Range$Q90 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.9)
  Range$Min <- rowMins(as.matrix(scenario[,-1]))
  
  # Range$Q5 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.05)
  # Range$Q95 <- rowQuantiles(as.matrix(scenario[,-1]), probs=0.95)
  
  #### Plot =======================================================================================
  
  ### create plot area --------------------------------------
  
  par(mar=c(7, 5, 4, 2) + 0.2 , mgp=c(2.5, 1, 0) )
  
  plot(1,  
       # main=paste0("\n Time Series of Simulated Annual Streamflow for Colorado River at Lees Ferry \n Ensemble: ", ScenarioList[2], ",    Number of Realizations: ", ntraces),
       # cex.main=1.1,
       xlim=c(min(scenario[,1]), max(scenario[,1])), 
       ylim=c(0, max(Range$Max)*1.2),
       xaxt="n",
       yaxt="n",
       xlab="Year",
       ylab="Streamflow (maf/yr)", 
       las=1,
       cex.lab=1.1,
       col="white")
  
  title(main="Simulated Annual Natural Flow for the Colorado River at Lees Ferry, Arizona", adj=0.5, line=1.8, cex.main=1)  ##  ## Time Series of Simulated Annual Streamflow for Colorado River at Lees Ferry
  text(paste0("Ensemble: ", ScenarioList[2], ",    Number of Realizations: ", ntraces),
       x=(par("usr")[1] + par("usr")[2])/2,
       y=(par("usr")[4]),
       xpd=T,
       adj=c(0.5,-1.2),
       cex=0.9)
  
  axis(1, at=seq(yr1, yr2, 2), las=1, cex.axis=1)
  axis(1, at=seq(yr1, yr2, 1),  labels=NA, tck=0.015)
  
  axis(2, at=seq(0, max(Range$Max)*1.2, 5), las=2, cex.axis=1.1, cex=2)
  axis(2, at=seq(0, max(Range$Max)*1.2, 1), labels=NA, tck=0.015)
  
  axis(4, at=seq(0, max(Range$Max)*1.2, 5), labels=NA)
  axis(4, at=seq(0, max(Range$Max)*1.2, 1), labels=NA, tck=0.015)
  
  ### Plot quartiles and ranges --------------------------------
  
  # col0 <- rgb(r=184/255, g=222/255, b=234/255, alpha=0.5)
  col1 <- rgb(r=184/255, g=222/255, b=234/255, alpha=1)
  col2 <- rgb(r=122/255, g=194/255, b=216/255, alpha=1)
  # col3 <- rgb(r=50/255, g=116/255, b=176/255, alpha=1)
  col3 <- "navy"
  col4 <- "red"
  
  # ## add 5% to 95% quartiles 
  # polygon( x=c(scenario[,1] , rev(scenario[,1] )) , y=c(Range$Q5, rev(Range$Q95)) , col=col0 , border=NA)
  # 
  
  ## add 10% to 90% quartiles 
  polygon( x=c(scenario[,1] , rev(scenario[,1] )) , y=c(Range$Q10, rev(Range$Q90)) , col=col1 , border=NA)
  
  ## add max to min ranges
  for(y in 1:nyrs){
    arrows(x0=scenario[y,1], y0=Range$Min[y], x1=scenario[y,1], y1=Range$Max[y], code=3, angle=90, length=0.02, col=col2, lwd=1.5, lty="solid")
  }
  
  ## add 25% to 75% quartiles
  polygon( x=c(scenario[,1] , rev(scenario[,1] )) , y=c(Range$Q25, rev(Range$Q75)) , col=col2 , border=NA)
  
  ## add median line
  lines(x=scenario[,1], y=Range$Median, lwd=3, col=col3)
  
  
  ## add a sample trace
  lines(x=scenario[,1], y=scenario[,2], col=col4, lwd=1)
  
  ### add legend -----------------------------------------------
  
  legend("top", cex=0.8, inset=c(0,0),  ## inset=c(0,-0.1)
         legend= c( "10%-90%","25%-75%  ", "Max/Min", "Median", "Sample trace" ),
         col=c(col1, col2, col2, col3, col4),
         # pt.bg=c(col1, col2, NA, NA), 
         pch=c(15, 15, NA, NA, NA),  pt.cex=c(1.5, 1.5, NA, NA, NA),  
         lwd=c(NA, NA, 1.5, 3, 1), lty=c(0,0,1,1,1),
         horiz = T, bty="n", x.intersp =0.5, xpd=T)

  
  
  #### Trend Analysis ===================================================
  
  ### if ISM, (if ISM character exists in ScenarioName) ------------
  IsISM <- grepl("ISM", ScenarioName, fixed = TRUE)
  
  if (IsISM==FALSE){   ### ISM scenarios have no trends. They have a horizontal median, which makes mann-kendall trend test results false (there are lots of ties in the test)
    
    TimeSeries <- subset(Range, select=c("Yr", "Median"))
    
    Reg <- lm(TimeSeries[,2] ~ TimeSeries[,1])
    Equ <-  paste("y =",round(coef(Reg)[2], digits=7),".x +", round(coef(Reg)[1], digits=4))
    delta <- coef(Reg)[2] * (yr2 - yr1 +1)
    slope <- Reg$coefficients[2]
    
    ## average of median line -----
    LTave <- mean(TimeSeries[,2])
    
    ## Mann-Kendall Test --------
    library("Kendall")
    MK_SF1 <- MannKendall(TimeSeries[,2])
    # Kendall(TimeSeries[,2], TimeSeries[,2])
    
    ## draw trend line
    x <- as.vector(TimeSeries[,1])
    y <- as.vector(TimeSeries[,2])
    # abline(lm( y ~ x ),  col="green", lty="dashed")
    
    ## Add text in plot to show trend analysis results
    if (MK_SF1$sl > 0.05){
      mtext(side=1, line=5.5, adj=0, cex=1.1, 
            text=paste0("Mann-Kendall Trend Test:  Tau = ", round(MK_SF1$tau, digits=2),
                        ",  P-Value = ", round(MK_SF1$sl, digits=4), "\n", 
                        "Trend = ", round(slope, digits = 4), " maf/yr,  Not Statistically Significant")   )
    }else{
      mtext(side=1, line=5.5, adj=0, cex=1.1, 
            text=paste0("Mann-Kendall Trend Test:  Tau = ", round(MK_SF1$tau, digits=2),
                        ",  P-Value = ", round(MK_SF1$sl, digits=4), "\n", 
                        "Trend = ", round(slope, digits = 4), " maf/yr,  Statistically Significant")   )
    }
    
    
  }  ## End of: if (IsISM==FALSE)
  
  
  if (IsISM==TRUE){
    mtext(side=1, line=4.5, adj=0, cex=1.1, text="No Trend")
  }
  
  
  
  # ##### add the Range dataframe output to Global Environment ##################################################################################
  # 
  # assign("HM_TimeSeriesRange", Range, .GlobalEnv)


}


