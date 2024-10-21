##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################

##### Lag-1 Normalized Mutual Information ###################################################

Func_MutualInfo <- function (ScenarioList){
  
  #### packages #####
  ## dplyr, entropy
  # install.packages("dplyr")  ## to apply the lead and lag functions 
  # install.packages("entropy")
  
  ### Calculate MI using "entropy" package ##############################################################################
  ### https://cran.r-project.org/web/packages/entropy/entropy.pdf
  
  
  ### Function to calculate NMI between X and Y ------------------------------------------------

  
  library("entropy")
  func_NMI <- function (TimeSeries, X=X, Y=Y, nbins=10, Plot=F){
    
    ## binning method 1 (from entropy package):
    DiscXY2d  <-  discretize2d(X, Y, numBins1=nbins, numBins2=nbins)
    
    # ## binning method 2 (nbins can be non-integer so that the bin width will be equal to this formula --> 3.49 * sd(TimeSeries) * n^(-1/3) ):
    # bin_width <- ( max(TimeSeries)-min(TimeSeries) ) / nbins
    # X_c <- cut(X, breaks = seq(min(X), max(X) + bin_width, by = bin_width), right = FALSE)
    # Y_c <- cut(Y, breaks = seq(min(Y), max(Y) + bin_width, by = bin_width), right = FALSE)
    # DiscXY2d <- table(X_c, Y_c )
    
    ## joint entropy
    H12  <-  entropy(DiscXY2d )
    ## marginal entropies
    H1  <-  entropy(rowSums(DiscXY2d))
    H2 <-  entropy(colSums(DiscXY2d))
    ## mutual information
    MI <- H1+H2-H12
    ### normalized mutual information (NMI). It is better to normalize MI (NMI). MI(x,x)=H(x) ==> normalized MI = 1
    NMI <- 2*(H1+H2-H12)/(H1+H2) ### see Taormine et al 2016. 
    
    if (Plot==T){
      print(paste0("H1, H2, H12 = ", round(H1,digits=2),"  ", round(H2,digits=2), "  ", round(H12,digits=2)))
      print(paste0("MI, NMI = ", round(MI,digits=2),"  ", round(NMI,digits=2)))
      plot(X, Y, las=1, cex.lab=1.4, cex.axis=1.4 , cex.main=1.6,
           main=paste0("\n MI = ",round(MI,digits=2) ,",  NMI = ",round(NMI,digits=2),",   Cor = ", round(cor(X,Y), digits=2)  ))
    }
    
    return(NMI)
  }
  
  

  ### Calculate NMI values for all the traces of each scenario ------------------------------------------
  
  # nbins=20
  
  NMIlist <- vector("list",nScenarios)
  NBinslist <- vector("list",nScenarios)
  nScenarios <- nrow(ScenarioList)
  
  for (s in 1:nScenarios){
    
    Scenario <- eval(parse(text = ScenarioList[s,1]))
    
    nTraces <- ncol(Scenario)-1
    NMIscen=rep(NA,nTraces)
    NBinsEns=rep(NA,nTraces)
    
    for (t in 1:nTraces){
      
      ## data of the first trace
      TimeSeries <- Scenario[,(t+1)]
      
      n <- length(TimeSeries)
      X <- TimeSeries[-n]
      Y <- dplyr::lead(TimeSeries, n=1)[-n]  ## lag the time series
      
      ## specify nbins >>>>>>>>>>>>
      BinWidth <- 3.49 * sd(TimeSeries) * n^(-1/3)  ### Look at Gong et al., 2014
      nbins <-  (max(TimeSeries)-min(TimeSeries)) / BinWidth
      nbins <- round(nbins)
      
      NMIscen[t] <- func_NMI(TimeSeries, X, Y, nbins=nbins, Plot=F)
      NBinsEns[t] <- nbins
    }
    
    NMIlist[[s]] <- NMIscen
    NBinslist[[s]] <- NBinsEns
  }
  
  ### Historical NMI ----------------------------------------------------------------------------------
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  
  TimeSeries <- Hist[,2]
  n <- nrow(Hist)
  X <- Hist[-n,2]
  Y <- dplyr::lead(Hist[,2], n=1)[-n]  ## lag the time series
  
  BinWidth_hist <- 3.49 * sd(Hist[,2]) * n^(-1/3)  ### Look at Gong et al., 2014
  nbins_hist <-  (max(Hist[,2])-min(Hist[,2])) / BinWidth_hist
  nbins_hist <- round(nbins_hist)
  
  NMI_Hist <- func_NMI(TimeSeries, X, Y, nbins=nbins_hist, Plot=F)
  
  # 
  # bin_width=3.26
  # X_c <- cut(X, breaks = seq(min(X), max(X) + bin_width, by = bin_width), right = FALSE)
  # Y_c <- cut(Y, breaks = seq(min(Y), max(Y) + bin_width, by = bin_width), right = FALSE)
  # z <- table(X_c, Y_c )
  # 
  # library(plot3D)
  # z=discretize2d(X, Y, numBins1=nbins, numBins2=nbins)
  # hist3D( z=z )
  # image2D(z=z, border="black")
  
  
  ##### Store required heatmap values ==================================================================
  
  ####  Create an empty dataframe to get the final values that we need for heatmap 
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  ### Store the values needed for the final heatmap 
  for (s in 1:nScenarios){
    HeatmapValues$Min[s] <- min(NMIlist[[s]])
    HeatmapValues$Q10[s] <- quantile(NMIlist[[s]], probs=0.10)
    HeatmapValues$Q25[s] <- quantile(NMIlist[[s]], probs=0.25)
    HeatmapValues$Median[s] <- median(NMIlist[[s]])
    HeatmapValues$Q75[s] <- quantile(NMIlist[[s]], probs=0.75)
    HeatmapValues$Q90[s] <- quantile(NMIlist[[s]], probs=0.90)
    HeatmapValues$Max[s] <- max(NMIlist[[s]])
  }
  
  HeatmapValues$Hist <- NMI_Hist
  
  ### boxplot =========================================================================================
  ### Each boxplot represents a scenario comprising multiple traces
  ### Each boxplot shows the ranges of NMI calculated for all the traces whithin a scenario
  
  ylim1=0
  ylim2=1
  dy=0.05
  
  par(mar=c(6,4.5,3,2))  ### margins for plot
  boxplot(NMIlist, names=NA, las=1, cex.axis=1.2, ylim=c(ylim1,ylim2), cex=0.4, ylab="MI", cex.lab=1.3)
  abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
  boxplot(NMIlist, names=NA, las=1, cex.axis=1.2, ylim=c(ylim1,ylim2), cex=0.4, ylab="MI", cex.lab=1.3, add=T)
  abline(h=NMI_Hist, col="red")
  axis(2, at=seq(ylim1, ylim2, dy) , labels=NA, tck=0.015)
  axis(4, labels=NA, tck=-0.02)
  axis(4, at=seq(ylim1, ylim2, dy) , labels=NA, tck=0.015)
  title("Lag-1 Normalized Mutual Information ")
  text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.3), labels=ScenarioList[,2], cex=0.9, srt=30, xpd=T)
  
  legend("topleft", legend="Historical", cex=1, col="red", lty=1, inset=c(0.03, 0.03), bg = "white")
  
  
  
  # ### boxplot of nbins ---------------------------
  # 
  # ylim1=1
  # ylim2=10
  # dy=1
  # 
  # par(mar=c(6,4.5,3,2))  ### margins for plot
  # boxplot(NBinslist, names=NA, las=1, cex.axis=1.2, ylim=c(ylim1,ylim2), cex=0.4, ylab="nbins", cex.lab=1.3)
  # abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
  # boxplot(NBinslist, names=NA, las=1, cex.axis=1.2, ylim=c(ylim1,ylim2), cex=0.4, ylab="nbins", cex.lab=1.3, add=T)
  # abline(h=nbins_hist, col="red")
  # axis(2, at=seq(ylim1, ylim2, dy) , labels=NA, tck=0.015)
  # axis(4, labels=NA, tck=-0.02)
  # axis(4, at=seq(ylim1, ylim2, dy) , labels=NA, tck=0.015)
  # title(paste0("nbins used in NMI"))
  # text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.3), labels=ScenarioList[,2], cex=0.9, srt=30, xpd=T)
  # 
  # legend("topleft", legend="Historical", cex=1, col="red", lty=1, inset=c(0.03, 0.03), bg = "white")
  # 
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_NMI", HeatmapValues, .GlobalEnv)
  
}  ## end of Func_MutualInfo function







