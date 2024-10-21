##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#### Function to add duration-severity of historical and tree ring to the plot (add dotty plots)
###################################################################################################


DurationSeverity_Dots <- function (data, MaxDuration=25, yr1=1906, yr2=2019, unit_factor=10^(-6), 
                                   post_yr=2000, gap=0, col1="lightskyblue2", col2="red", DotType=1, DotSize=0.5, DotBg="green", DotLwd=1,
                                   lable_min=T, ltm=T, line_minNF=T) {
  
  
  
  ##### DurSev calculations (dataframe) ====================================================
  
  ## nlowest function: find the nth lowest value (or index) in x --------------
  
  nlowest <- function(x,n,value=TRUE){
    s <- sort(x,index.return=TRUE)
    if(value==TRUE)  {s$x[n]}  else  {s$ix[n]}    ## TRUE: nlowest=value   FALSE: nlowest=index
  } 
  
  ## ---------------------
  
  years <- yr1:yr2
  n <- length(years)
  
  s=1
  ## take the flow data related to the s series ------------
  flow <- data[ which(data[,1]==yr1):which(data[,1]==yr2) ,(1+s)]
  
  ## empty matrixes -------------
  Mean<- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  lowest <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  lowest_index <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  lowest_year <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  
  ## Loop: over the defined sequences that their average is required ----------
  for (m_yr in 1:MaxDuration){  
    
    mean_m_yr <- rep(NA)
    sort <- rep(NA)
    
    for (m in 1:(n-(m_yr-1))){
      mean_m_yr[m] <- mean( flow[ m : (m+(m_yr-1)) ] )
      Mean[m ,m_yr] <- mean_m_yr[m]
    }
    
    for (m in 1:(n-(m_yr-1))){
      lowest[m ,m_yr] <- nlowest( mean_m_yr,m,value=TRUE)
      lowest_index[m ,m_yr] <- nlowest(mean_m_yr,m,value=FALSE)   
      lowest_year[m ,m_yr] <- years[lowest_index[m ,m_yr]]
    }
    
  }
  
  ## change unit ----------------------
  lowest_unit <- lowest*unit_factor  
  
  
  ## the final data frames that you can use for dotty plots
  DurSev <- lowest_unit
  DurSev_yr <- lowest_year
  

  
  if (is.na(post_yr)==TRUE){   ### color all dots blue ---------------------
    
    for (j in 1:MaxDuration){  
      for (i in 1:(n-(j-1))){  #1:n
        points(j+gap, DurSev[i,j], col= "lightskyblue2", bg=DotBg ,pch=DotType, cex=DotSize, lwd=DotLwd)
        # lowest_pre_yr[i,j] <- DurSev[i,j]   ## if you want to have the amount of blue dots
      }
    }
    
    
  } else {   ### seperate dots to blue and red --------------------
    
    for (j in 1:MaxDuration){  
      for (i in 1:(n-(j-1))){  #1:n
        
        if ( DurSev_yr[i,j]<post_yr) {
          points(j+gap, DurSev[i,j], col= col1 , bg=DotBg ,pch=DotType, cex=DotSize, lwd=DotLwd)
          # lowest_pre_yr[i,j] <- DurSev[i,j]   ## if you want to have the amount of blue dots
        }
      }
    }
    
    
    for (j in 1:MaxDuration){  
      for (i in 1:(n-(j-1))){  #1:n
        
        if ( DurSev_yr[i,j]>=post_yr) {
          points(j+gap, DurSev[i,j], col= "black" ,bg=col2 ,pch=21, cex=0.6, lwd=0.2)
          # text(DurSev[i,j]~j, labels=DurSev_yr[i,j], pos = 2, cex=0.5, col="black")   ## to add the year lables to dots
          # lowest_post_yr[i,j] <- DurSev[i,j]   ## if you want to take the amount of blue dots
        }
      }
    }
    
  }
  

  if(line_minNF==T){
    lines(c(1:MaxDuration), DurSev[1,], col="blue")
  }
  
  
  
  if (ltm==TRUE){
    
    ### add a line representing the long-term average of flow during the whole period -----------
    ave_all <- mean(flow)* unit_factor
    abline (ave_all, 0, col="steelblue2", lwd=1.2)
    
    ### add a line representing the long-term average of flow during the post-yr period 
    while(post_yr<=yr2){
      ave_post <- mean(flow[(which(years==post_yr) : which(years==yr2))] ) * unit_factor
      abline (ave_post, 0, col= col2, lwd=1.2)
      break}
    
    
    ### lable the two lines related to long-term averages -----------
    if(post_yr<=yr2){
      if(ave_all>ave_post){
        text((MaxDuration+0.2),(ave_all+0.3), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)  ##, font=2
        text((MaxDuration+0.2),(ave_post-0.4), labels= paste(round(ave_post, digits=2)), pos = 4, cex=1, col="red", xpd=TRUE)
      }
      if(ave_all<ave_post){
        text((MaxDuration+0.2),(ave_all-0.4), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)  ##, font=2
        text((MaxDuration+0.2),(ave_post+0.4), labels= paste(round(ave_post, digits=2)), pos = 4, cex=1, col="red", xpd=TRUE)
      }
    } else {
      text((MaxDuration+0.2),(ave_all+0.3), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)
    }
    
  }

  
  ### lable the first and second min
  
  xx <- c(1:MaxDuration)-0.07  ## c(1:MaxDuration)
  
  if (lable_min==TRUE){
    text((DurSev[1,]-0.08)~c(1:MaxDuration), labels=DurSev_yr[1,], pos = 1, cex=1.1, col="black", srt=0) ## the lowest     (vertical text: srt=90)  cex=0.6
    text(DurSev[2,]~xx, labels=DurSev_yr[2,], pos = 2, cex=1.1, col="gray47", srt=0)  ## the second lowest   cex=0.5
  }
  
  ## return the DurSev dataframe in hte Global Environment
  # assign("DurSev", data.frame(DurSev), .GlobalEnv)
  # assign("DurSev_yr", data.frame(DurSev_yr), .GlobalEnv)
  
} # function






