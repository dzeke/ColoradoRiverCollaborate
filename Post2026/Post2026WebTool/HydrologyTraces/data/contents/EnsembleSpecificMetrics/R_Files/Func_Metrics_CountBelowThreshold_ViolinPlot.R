##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#########################################################################################################
###### Function to calculate count below threshold for a specific duration
###### and then create the violin of the distribution of all traces of one scenario



### Function to calculate the Average Count Below Threshold =========================================

Func_CountBelowThreshold_ViolinPlot <- function(scenario=ISM_Full , Threshold=14.78 , Duration=c(5,10,15,20) , ScenarioName="ISM_Full"){
  

    data <- scenario
    
    n <- nrow(data)
    nTraces <- ncol(data)-1
    
    ### is data below the threshold or not (loop over all traces) -------
    
    IsBelowThreshold <- data
    IsBelowThreshold [,-1] <- NA
    
    for (j in 1:nTraces){
      for (i in 1:n){
        if (data[i,(1+j)]<Threshold) {
          IsBelowThreshold[i,(1+j)] <- 1
        }else{
          IsBelowThreshold[i,(1+j)] <- 0
        }
      }
    }
    
    
    ### Count below threshold over duration (all the traces) ---------------------
    
    Duration <- Duration[!is.na(Duration)]
    
    for (dur in 1:length(Duration)){
      
      nBlock <- (n-Duration[dur]+1)
      CountBelowThreshold <- data.frame(matrix( nrow=nBlock , ncol=(ncol(data)+1)  ))  ##data.frame(matrix( nrow=nBlock , ncol=(ncol(data)+1)  ))
      colnames(CountBelowThreshold)[1:2] <- c("StartYr", "EndYr")
      colnames(CountBelowThreshold)[3:(nTraces+2)] <- colnames(data)[-1]
      
      
      for (j in 1:nTraces){
        
        for (b in 1:nBlock){
          CountBelowThreshold$StartYr[b] <- data[b,1]
          CountBelowThreshold$EndYr[b] <- data[(b-1+Duration[dur]) , 1]
          CountBelowThreshold[b, (j+2)] <- sum(IsBelowThreshold[b:(b-1+Duration[dur]), (j+1)])
        }
      }
      

      # return(CountBelowThreshold)
      
      CountBelowThreshold_1col <- data.frame(matrix(NA, ncol=(2), nrow=(nBlock*nTraces)))
      colnames(CountBelowThreshold_1col) <- c("trace", "CountBT")
      for (t in 1: nTraces){
        CountBelowThreshold_1col[ ((t-1)*nBlock+1):(nBlock*t), 1] <- t
        CountBelowThreshold_1col[ ((t-1)*nBlock+1):(nBlock*t), 2] <- CountBelowThreshold[,(2+t)] 
      }
      
      if (Duration[dur]<10){
        CountBelowThreshold_1col$dur <- paste0("0", Duration[dur],"-yr")
      }else{
        CountBelowThreshold_1col$dur <- paste0(Duration[dur],"-yr")
      }
      
      
      if (dur==1){
        CountBelowThreshold_Durations <- CountBelowThreshold_1col
      }else {
        CountBelowThreshold_Durations <- rbind(CountBelowThreshold_Durations, CountBelowThreshold_1col)
      }
      
      
    } ## end of loop over durations
    

  
  
  ###### Plot ====================================================
  
    library("ggplot2")

    col_bxF <- rgb(r=1, g=0, b=0, alpha=0.1)
    col_bxB <- "red"
    col_density <- "royalblue1"  ## rgb(r=23/255, g=193/255, b=211/255, alpha=1)
    
    ggplot(CountBelowThreshold_Durations, aes(x=dur, y=CountBT))+
      
      # geom_violin(fill="lightblue", colour="lightblue3", adjust=2)+  ## adjust=2
      
      geom_dotplot( na.rm=T , binaxis="y", stackdir="center", 
                    method="histodot",
                    binwidth=1,
                    dotsize=0.007,
                    stackratio=0.5,
                    # dotsize=0.15,
                    # stackratio=0.02,
                    col=col_density,
                    fill=col_density) +
      
      geom_boxplot( width=0.07,
                    outlier.shape =1, outlier.colour=col_bxB, 
                    fill=col_bxF, col=col_bxB,
                    outlier.size=0.6,
                    lwd=0.3)+
      
      stat_summary( fun=mean, geom="point", shape=17, size=1.5, col=col_bxB)+
      
       
      annotate("text", x=5.51, y=16, label="Distribution", size=3)+
      annotate("rect", xmin=5.1, xmax=5.15, ymin=15.7, ymax=16.2, col=col_bxB, fill=col_bxF, lwd=0.3)+
      annotate("segment", x=5.08, xend=5.18, y=14.6, yend=14.6, col=col_density, lwd=1.1)+
      annotate("text", x=5.42, y=14.7, label="Density", size=3)+
      
      # annotate("text", x=-Inf, y=Inf, hjust=-0.2, vjust=2, label="Boxplot: Ranges of CBT", size=4)+
      # annotate("text", x=-Inf, y=Inf, hjust=-0.2, vjust=4, label="Blue line: Density of CBT", size=4)+

      
      coord_cartesian(xlim=c(1,4), clip="off")+
      theme_bw()+
      labs(title=paste0("Duration-count analysis (Threshold: ", Threshold, " maf/yr) \n ",ScenarioName ),
           x="Duration",
           y="Count below threshold (yrs)")+
      scale_y_continuous(limits=c(0, max(Duration)), breaks = (seq(0, 30, by=2))  )+
      scale_x_discrete(expand = expansion(add = 1))+ ## to add space between x and y axis
      theme(
        axis.text = element_text(size=13),  
        axis.title = element_text(size=13),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=8)),
        plot.title = element_text(hjust=0.5, size=13, face="bold" , margin=margin(b=8)),
        plot.margin = margin(t=40, r=110, b=10, l=70)
      )
    
    
  
}























