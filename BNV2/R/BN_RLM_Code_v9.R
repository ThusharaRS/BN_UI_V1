#'Creating json object for BN

#'@return value
#'@export


# rm(list=ls())

display <- function(){
# install.packages('bnlearn', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("GGally")
# install.packages("network")
# install.packages("sna")
# install.packages("RColorBrewer")
# install.packages("intergraph")
# install.packages("qgraph")
# install.packages("bnviewer")
# install.packages("devtools")
# devtools::install_github("robson-fernandes/bnviewer")
# install.packages(c('devtools','bnviewer'))
# install.packages("lazyeval")
Packages <- c("plyr", "dplyr", "tidyr", "bnlearn", "reshape", "bnviewer","rjson","readr")
lapply(Packages, library, character.only = TRUE)

#RLM_Data <- read.csv("Data\\Respondent Level Data_Consumption Segments.csv")


data(RLM_Data)

RLM_Data_4vs5_0 <- filter(RLM_Data, RLM_Data$KO_Consumption >= 4)[,c(201,26:39,194:200,209:358)]
RLM_Data_4vs5_1 <- sapply(RLM_Data_4vs5_0,as.factor)
RLM_Data_4vs5_2 <- as.data.frame(RLM_Data_4vs5_1)
RLM_Data_4vs5_3 <- RLM_Data_4vs5_2[,c(107,23,12,16,121,151,163,47,21,78,74,1)]
RLM_Data_4vs5_4 <- rename(RLM_Data_4vs5_3, c("ST_MainSecOccasionNets17_KO_Prop_4vs5" = "KO during media consumption at leisure","ST_CompanionNets1_KO_Prop_4vs5" = "KO alone or by myself","Q28Q30Loop_11_Q28Q3001" = "Imagery KO is more refreshing than other soft drinks","QCNets_New" = "Age Nets","ST_Q26_10_TB_Prop_4vs5" = "Any Bev to renew my energy","ST_Q26_2_TB_Prop_4vs5" = "Any Bev to wake me up","ST_Q26_6_TB_Prop_4vs5" = "Any Bev to ensure i drink enough each day","ST_DaypartHighLevelNets_4_KO_Prop_4vs5" = "KO in evening","QF_New" = "Income Level","ST_MainSecOccasionNets07_SSD_Regular_Prop_4vs5" = "SSD eating dinner away","ST_MainSecOccasionNets06_KO_Prop_4vs5" = "KO eating lunch away","KO_Consumption_4_5" = "KO Consumption"))

### Ploychoric Correlation
# Daily_Matrix <- hetcor(RLM_Data_4vs5_4)
# Daily_Matrix1 <- as.data.frame(Daily_Matrix$correlations)
# write.csv(Daily_Matrix1, "D:/Respondent Level Modeling_Bev360 CBL/4. Analytics/Path Analysis/Bayesian Network/BN_Model_Correlation.csv")

#### score-based structure learning algorithms - HILL CLIMBING

BL <- matrix(c(## KO during media consumption at leisure
  # "KO alone or by myself", "KO during media consumption at leisure",
  #"Imagery KO is more refreshing than other soft drinks", "KO during media consumption at leisure",
  #"Age Nets", "KO during media consumption at leisure",
  #"Any Bev to renew my energy", "KO during media consumption at leisure",
  "Any Bev to wake me up", "KO during media consumption at leisure",
  #"Any Bev to ensure i drink enough each day", "KO during media consumption at leisure",
  "KO in evening", "KO during media consumption at leisure",
  #"Income Level", "KO during media consumption at leisure",
  "SSD eating dinner away", "KO during media consumption at leisure",
  "KO eating lunch away", "KO during media consumption at leisure",
  "KO Consumption", "KO during media consumption at leisure",

  ## KO alone or by myself
  "KO during media consumption at leisure", "KO alone or by myself",
  "Imagery KO is more refreshing than other soft drinks", "KO alone or by myself",
  "Age Nets", "KO alone or by myself",
  #"Any Bev to renew my energy", "KO alone or by myself",
  #"Any Bev to wake me up", "KO alone or by myself",
  "Any Bev to ensure i drink enough each day", "KO alone or by myself",
  "KO in evening", "KO alone or by myself",
  "Income Level", "KO alone or by myself",
  "SSD eating dinner away", "KO alone or by myself",
  "KO eating lunch away", "KO alone or by myself",
  "KO Consumption", "KO alone or by myself",

  ## Imagery KO is more refreshing than other soft drinks
  #"KO during media consumption at leisure", "Imagery KO is more refreshing than other soft drinks",
  "KO alone or by myself", "Imagery KO is more refreshing than other soft drinks",
  # "Age Nets", "Imagery KO is more refreshing than other soft drinks",
  #"Any Bev to renew my energy", "Imagery KO is more refreshing than other soft drinks",
  #"Any Bev to wake me up", "Imagery KO is more refreshing than other soft drinks",
  #"Any Bev to ensure i drink enough each day", "Imagery KO is more refreshing than other soft drinks",
  "KO in evening", "Imagery KO is more refreshing than other soft drinks",
  "Income Level", "Imagery KO is more refreshing than other soft drinks",
  #"SSD eating dinner away", "Imagery KO is more refreshing than other soft drinks",
  #"KO eating lunch away", "Imagery KO is more refreshing than other soft drinks",
  "KO Consumption", "Imagery KO is more refreshing than other soft drinks",

  ## Age Nets
  "KO during media consumption at leisure", "Age Nets",
  "KO alone or by myself", "Age Nets",
  "Imagery KO is more refreshing than other soft drinks", "Age Nets",
  "Any Bev to renew my energy", "Age Nets",
  "Any Bev to wake me up", "Age Nets",
  "Any Bev to ensure i drink enough each day", "Age Nets",
  "KO in evening", "Age Nets",
  "Income Level", "Age Nets",
  "SSD eating dinner away", "Age Nets",
  "KO eating lunch away", "Age Nets",
  "KO Consumption", "Age Nets",

  ## Any Bev to renew my energy
  # "KO during media consumption at leisure", "Any Bev to renew my energy",
  "KO alone or by myself", "Any Bev to renew my energy",
  "Imagery KO is more refreshing than other soft drinks", "Any Bev to renew my energy",
  "Age Nets", "Any Bev to renew my energy",
  "Any Bev to wake me up", "Any Bev to renew my energy",
  #"Any Bev to ensure i drink enough each day", "Any Bev to renew my energy",
  # "KO in evening", "Any Bev to renew my energy",
  # "Income Level", "Any Bev to renew my energy",
  #"SSD eating dinner away", "Any Bev to renew my energy",
  # "KO eating lunch away", "Any Bev to renew my energy",
  "KO Consumption", "Any Bev to renew my energy",

  ## Any Bev to wake me up
  "KO during media consumption at leisure", "Any Bev to wake me up",
  "KO alone or by myself", "Any Bev to wake me up",
  "Imagery KO is more refreshing than other soft drinks", "Any Bev to wake me up",
  "Age Nets", "Any Bev to wake me up",
  #"Any Bev to renew my energy", "Any Bev to wake me up",
  "Any Bev to ensure i drink enough each day", "Any Bev to wake me up",
  "KO in evening", "Any Bev to wake me up",
  # "Income Level", "Any Bev to wake me up",
  "SSD eating dinner away", "Any Bev to wake me up",
  "KO eating lunch away", "Any Bev to wake me up",
  "KO Consumption", "Any Bev to wake me up",

  ## Any Bev to ensure i drink enough each day
  "KO during media consumption at leisure", "Any Bev to ensure i drink enough each day",
  "KO alone or by myself", "Any Bev to ensure i drink enough each day",
  #"Imagery KO is more refreshing than other soft drinks", "Any Bev to ensure i drink enough each day",
  "Age Nets", "Any Bev to ensure i drink enough each day",
  #"Any Bev to renew my energy", "Any Bev to ensure i drink enough each day",
  "Any Bev to wake me up", "Any Bev to ensure i drink enough each day",
  #"Any Bev to ensure i drink enough each day", "Any Bev to ensure i drink enough each day",
  "KO in evening", "Any Bev to ensure i drink enough each day",
  "Income Level", "Any Bev to ensure i drink enough each day",
  "SSD eating dinner away", "Any Bev to ensure i drink enough each day",
  "KO eating lunch away", "Any Bev to ensure i drink enough each day",
  "KO Consumption", "Any Bev to ensure i drink enough each day",

  ## KO in evening
  "KO during media consumption at leisure", "KO in evening",
  "KO alone or by myself", "KO in evening",
  "Imagery KO is more refreshing than other soft drinks", "KO in evening",
  "Age Nets", "KO in evening",
  # "Any Bev to renew my energy", "KO in evening",
  # "Any Bev to wake me up", "KO in evening",
  # "Any Bev to ensure i drink enough each day", "KO in evening",
  "Income Level", "KO in evening",
  "SSD eating dinner away", "KO in evening",
  "KO eating lunch away", "KO in evening",
  "KO Consumption", "KO in evening",

  ## Income Level
  "KO during media consumption at leisure", "Income Level",
  "KO alone or by myself", "Income Level",
  "Imagery KO is more refreshing than other soft drinks", "Income Level",
  # "Age Nets", "Income Level",
  "Any Bev to renew my energy", "Income Level",
  "Any Bev to wake me up", "Income Level",
  "Any Bev to ensure i drink enough each day", "Income Level",
  "KO in evening", "Income Level",
  "SSD eating dinner away", "Income Level",
  "KO eating lunch away", "Income Level",
  "KO Consumption", "Income Level",

  ## SSD eating dinner away
  "KO during media consumption at leisure", "SSD eating dinner away",
  "KO alone or by myself", "SSD eating dinner away",
  #"Imagery KO is more refreshing than other soft drinks", "SSD eating dinner away",
  "Age Nets", "SSD eating dinner away",
  "Any Bev to renew my energy", "SSD eating dinner away",
  "Any Bev to wake me up", "SSD eating dinner away",
  # "Any Bev to ensure i drink enough each day", "SSD eating dinner away",
  "KO in evening", "SSD eating dinner away",
  "Income Level", "SSD eating dinner away",
  "KO eating lunch away", "SSD eating dinner away",
  "KO Consumption", "SSD eating dinner away",

  ## KO eating lunch away
  "KO during media consumption at leisure", "KO eating lunch away",
  "KO alone or by myself", "KO eating lunch away",
  #"Imagery KO is more refreshing than other soft drinks", "KO eating lunch away",
  "Age Nets", "KO eating lunch away",
  #"Any Bev to renew my energy", "KO eating lunch away",
  "Any Bev to wake me up", "KO eating lunch away",
  #"Any Bev to ensure i drink enough each day", "KO eating lunch away",
  "KO in evening", "KO eating lunch away",
  "Income Level", "KO eating lunch away",
  "SSD eating dinner away", "KO eating lunch away",
  "KO Consumption", "KO eating lunch away",

  ## KO Consumption
  # "KO during media consumption at leisure", "KO Consumption",
  "KO alone or by myself", "KO Consumption",
  "Imagery KO is more refreshing than other soft drinks", "KO Consumption",
  # "Age Nets", "KO Consumption",
  # "Any Bev to renew my energy", "KO Consumption",
  # "Any Bev to wake me up", "KO Consumption",
  # "Any Bev to ensure i drink enough each day", "KO Consumption",
  "KO in evening", "KO Consumption",
  # "Income Level", "KO Consumption",
  "SSD eating dinner away", "KO Consumption",
  "KO eating lunch away", "KO Consumption"


),
ncol = 2, byrow = TRUE)
# BL

WL <- matrix(c("KO during media consumption at leisure", "KO Consumption",
               "KO alone or by myself", "KO during media consumption at leisure"
),
ncol = 2, byrow = TRUE)
# WL

BN_RLM_HC_9 <- hc(RLM_Data_4vs5_4, score = "aic", whitelist = WL, blacklist = BL)
BN_RLM_HC_9

Score_BN <- score(BN_RLM_HC_9,RLM_Data_4vs5_4)
Score_BN

acyclic(BN_RLM_HC_9, directed = FALSE, debug = FALSE)
directed(BN_RLM_HC_9)
Arcs_DF <- as.data.frame(arcs(BN_RLM_HC_9))
Arcs_DF$Unique <- paste(Arcs_DF$from,Arcs_DF$to,sep = "-")

Boot_Strength_8 <- boot.strength(RLM_Data_4vs5_4, cluster = NULL, R = 100, m = 50, algorithm = "hc", algorithm.args = list(), cpdag = TRUE, debug = FALSE)


Boot_Strength_DF <- data.frame(From = Boot_Strength_8$from, To = Boot_Strength_8$to,
                               strength = Boot_Strength_8$strength,
                               direction = Boot_Strength_8$direction
)

Boot_Strength_DF$Unique <- paste(Boot_Strength_DF$From,Boot_Strength_DF$To,sep = "-")
Arcs_BN <- merge(Arcs_DF,Boot_Strength_DF,by = "Unique")
request.body <- toJSON(Arcs_BN[,-c(1:3,7)])


return(request.body )
}
display()
