#'Creating json object for BN
#'@param variableSelectedblListWList
#'@return value
#'@export



  rm(list=ls())
  Packages <- c("plyr", "dplyr", "tidyr", "bnlearn", "reshape","rjson")
  lapply(Packages, library, character.only = TRUE)
displayJson <- function(variableSelected,blList, wlList) {


  myVector_WL<-unlist(strsplit(wlList,","))
  myVector_BL<-unlist(strsplit(blList,","))

  Data_0 <- filter(Data, Data$KO_Consumption_4_5 != 99)
  names(Data_0) = Mapping$New_Variable[match(names(Data_0), Mapping$Old_Variable)]

  Data_1 <- sapply(Data_0,as.factor)
  Data_2 <- as.data.frame(Data_1)

  myVector_var<-unlist(strsplit(variableSelected,","))
  Data_3 <- Data_2[, c(myVector_var)]
  
  ## Black Listing
  BL <- matrix(c(myVector_BL),
               ncol = 2, byrow = TRUE)

  # White Listing
  WL <- matrix(c(myVector_WL),
               ncol = 2, byrow = TRUE)

  #### score-based structure learning algorithms - HILL CLIMBING
  BN_HC <- hc(Data_3, score = "aic", whitelist = WL, blacklist = BL)

  Score_BN <- score(BN_HC,Data_3)

  acyclic(BN_HC, directed = FALSE, debug = FALSE)
  directed(BN_HC)
  Arcs_DF <- as.data.frame(arcs(BN_HC))
  Arcs_DF$Unique <- paste(Arcs_DF$from,Arcs_DF$to,sep = "-")

  Boot_Strength <- boot.strength(Data_3, cluster = NULL, R = 100, m = nrow(Data_3), algorithm = "hc", algorithm.args = list(), cpdag = TRUE, debug = FALSE)

  Boot_Strength_DF <- data.frame(From = Boot_Strength$from, To = Boot_Strength$to,
                                 strength = Boot_Strength$strength,
                                 direction = Boot_Strength$direction
  )

  Boot_Strength_DF$Unique <- paste(Boot_Strength_DF$From,Boot_Strength_DF$To,sep = "-")
  Arcs_BN <- merge(Arcs_DF,Boot_Strength_DF,by = "Unique")
  request.body <- toJSON(Arcs_BN[,-c(1:3,7)])

  return (request.body)
  
}
#blList<-"KO during media consumption at leisure,KO Consumption,Any Bev to renew my energy,Income Level"
#wlList<-"Age Nets,KO Consumption"
#variableSelected<-"KO during media consumption at leisure,KO alone or by myself,Imagery - KO is more refreshing than other soft drinks,Age Nets,Any Bev to renew my energy,Any Bev to wake me up,Any Bev to ensure i drink enough each day,KO in evening,Income Level,Regular SSD while eating dinner away,KO while eating lunch away,KO Consumption"

#display(variableSelected,blList,wlList)
