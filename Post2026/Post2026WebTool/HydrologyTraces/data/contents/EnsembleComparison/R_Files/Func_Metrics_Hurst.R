##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################



Func_Hurst <- function(ScenarioList, RoverSPlot=F, HurstBoxplot=F) {
  
  
  # function to calculate H =====================================================
  
  Func_Range=function(Q)
  {
    X=cumsum(Q-mean(Q))
    R=max(X)-min(X)
    R
  }
  
  
  HurstFunc=function(Qt, RoverSPlot, add=T, plottitle="Hurst plot")
  {
    N=length(Qt)
    #  Computing range, rescaled range and Hurst exponent
    # Intervals
    i=0
    # Use values that are a series of powers of 2 starting at 8 up to the length of the given series (subsets of the data)
    nvals=c(2^(3:trunc(log(N)/log(2))),N)
    for(n in nvals) {
      ns=ceiling(N/n)  #  ns is the number of subsets of data for a duration n
      for(ii in 1:ns){   #  ii is a counter of the subset intervals
        if(ii == 1){
          i=i+1    #  increment the index identifying subset
          istart=1   #   start of a subset interval
          iend=istart+n-1   # end of a subset interval
          
        }else
        {
          i=i+1
          iend=N-(ns-ii)*n
          istart=iend-n+1
        }
      }
    }
    #  First pass through just counts number of sets
    nset=i
    
    #  Second pass through, recording the interval information
    istart=rep(0,nset)
    iend=rep(0,nset)
    ni=rep(0,nset)
    i=0
    for(n in nvals)  
    {
      ns=ceiling(N/n) #  ns is the number of subsets of data for a duration n
      for(ii in 1:ns){
        if(ii == 1){
          i=i+1  # i is an index identifying the set of data
          ni[i]=n  #  keep track of the length for each subset
          istart[i]=1   #   start of an interval
          iend[i]=istart[i]+n-1   # end of an interval 
        }else
        {
          i=i+1  # i is an index identifying the set of data
          ni[i]=n   #  keep track of the length for each subset
          iend[i]=N-(ns-ii)*n
          istart[i]=iend[i]-n+1
        }
      }
    }
    
    # 3rd pass through computing range
    
    rnge=rep(0,nset)
    SS=rep(0,nset)
    RoverS=rep(0,nset)
    
    for(i in 1:nset){
      Q=Qt[istart[i]:iend[i]]
      rnge[i]=Func_Range(Q)
      SS[i]=sd(Q)
      RoverS[i]=rnge[i]/SS[i]
    }
    
    
    if(RoverSPlot==T){  ## do you want also the plot or just the Hurst value?
      
      if(!add) {
        plot(ni,RoverS,log="xy", xlab="Duration (yrs)", ylab="Rescaled Range", las=1, cex.lab=1.3, cex.axis=1.3, ylim=c(1, 20))
        axis(2, at=seq(1,100,1), labels=NA, tck=0.01)
        axis(1, at=seq(1,10000,1), labels=NA, tck=0.01)
        title(plottitle)
        legend("topleft", legend="Points for all traces and durations, line for each trace, \n slope of line gives Hurst exponent (H)",
               inset=c(0.03,0.05), cex=1.2, bty="n")
      }else{
        points(ni,RoverS)
      }
      
    }
    
    
    logreg=lsfit(log(ni),log(RoverS))$coefficients
    H=logreg[2]
    
    if(RoverSPlot==T){
      lines(ni,exp(logreg[1])*ni^H,  col="gray40")
    }
    
    H
  } 
  

  
  ## Hurst boxplot and R over S plot for scenarios =====================================================
  
  # par(mar=c(5,5,3,3))  ### margins for plot
  par(mar=c(6, 4, 3, 2) + 0.1 , mgp=c(2.7, 1, 0) )
  
  nScenarios <- nrow(ScenarioList)
  hlist=vector("list",nScenarios)
  
  for (s in 1:nScenarios){
    
    scenario <- eval(parse(text = ScenarioList[s,1]))
    nyrs <- nrow(scenario)
    ntrace=ncol(scenario)-1
    hscen=rep(NA,ntrace)
    for(t in 1:ntrace){
      hscen[t]=HurstFunc(scenario[,t+1], RoverSPlot, add=(t != 1), 
                         plottitle=paste0("Ensemble: ", ScenarioList[s,2]) )
    }

    hlist[[s]]=hscen
  }
  
  
  #### Add lines for the historical Hurst ========================
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
  # HistInfo <- data.frame("ExcelSheet"="Hist", "Name_On_Plots"="Historical", "StartYr"=1906, "EndYr"=2020)
  # source("./R_Files/Func_read_required_hydrology_scenarios_from_xlsx.R")
  # Func_read_required_hydrology_scenarios_from_xlsx(HistInfo)
  
  HHist=HurstFunc(Hist[,2], RoverSPlot=F, add=(t != 1), plottitle=HistInfo[1,2] )
  
  
  if (HurstBoxplot==T){
    ### boxplot
    boxplot(hlist, names=NA, las=1, cex.axis=1.2)
    abline(h=seq(0, 2, 0.1), col="grey50", lty="dotted")
    boxplot(hlist, names=NA, las=1, cex.axis=1.2, add=T)
    
    axis(2, at=seq(0,2, 0.1) , labels=NA, tck=0.015)
    axis(4, labels=NA, tck=-0.02)
    axis(4, at=seq(0,2, 0.1) , labels=NA, tck=0.015)
    title("Hurst coefficient")
    text(x=c(1:nScenarios)+0.5, y=(par("usr")[3]-0.04), labels=ScenarioList[,2], cex=0.9, srt=30, pos=2, xpd=T)
    
    
    abline(h=HHist, col="red", lwd=1.5)  ## 1906-2021: 14.65 maf/yr
    
    legend("bottomleft",
           legend=c("Historical Hurst  " ),
           cex=0.9,
           col="red",
           lwd=1.5, lty=1,
           inset=c(0.02, 0.03),
           bty = "o",
           bg = "white",
           box.col = "grey",
           y.intersp=1.5,  ## vertical distance between two symbols
           x.intersp=1)
    
  }
  
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  
  ### Store the values needed for the final heatmap 
  for (s in 1:nScenarios){
    HeatmapValues$Min[s] <- min(hlist[[s]])
    HeatmapValues$Q10[s] <- quantile(hlist[[s]], probs=0.10)
    HeatmapValues$Q25[s] <- quantile(hlist[[s]], probs=0.25)
    HeatmapValues$Median[s] <- median(hlist[[s]])
    HeatmapValues$Q75[s] <- quantile(hlist[[s]], probs=0.75)
    HeatmapValues$Q90[s] <- quantile(hlist[[s]], probs=0.90)
    HeatmapValues$Max[s] <- max(hlist[[s]])
  }
  
  HeatmapValues$Hist <- HHist
  
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_Hurst", HeatmapValues, .GlobalEnv)
  
}  ### end of Func_Hurst 





