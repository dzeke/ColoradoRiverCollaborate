# FebruaryForecastError.r
#
# Plot Febuary Forecast Erros as Histogram
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# Data from http://inkstain.net (Feburary 7, 2023)
#
# David E. Rosenberg
# March 8, 2023
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="http://cran.r-project.org") 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

  
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}


if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

if (!require(ggplot2)) { 
  install.packages("colorspace", type = "source")
  library(colorspace)
  install.packages("yaml", type = "source")
  library(yaml)
  install.packages("ggplot2", type = "source")
  library(ggplot2) 
}



if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

#For rollapply - running sum
if (!require(zoo)) { 
  install.packages("zoo", type = "source")
  library(zoo) 
}





# Read Forecast Errors from Excel
sExcelFile <- 'FebruaryForecastErrors.xlsx'
dfForecastErrors <- read_excel(sExcelFile, sheet = 'Sheet1',  range = "A5:C17")

dfNames <- colnames(dfForecastErrors)

#Calculate Difference
dfForecastErrors$Error <- dfForecastErrors$final - dfForecastErrors$`Feb. 1 forecast`

#Histogram -- frequency of annual errors
ggplot(dfForecastErrors, aes(x=Error)) +
  geom_histogram(color="darkmagenta", fill="magenta", binwidth = 1000) +
  
  scale_x_continuous(limits = c(-5000,6000), breaks = seq(-5000,6000,by=1000)) +
  #scale_y_continuous(breaks = seq(0,11,by=1)) +
  
  labs(x="Feb. 1 Forecast Error\n(million acre feet per year)", y="Frequency\n(number of years)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

ggsave("ForecastErrorHist.png", width=9, height = 6.5, units="in")

print(paste0("Number of Years = ", nrow(dfForecastErrors)))
