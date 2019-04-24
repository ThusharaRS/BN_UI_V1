#'Creating json object for BN
#'@param variableSelectedblListWList
#'@return value
#'@export




displayJson <- function(variableSelected,blList, wlList) {
  
  rm(list=ls())
  Packages <- c("plyr", "dplyr", "tidyr", "bnlearn", "reshape","rjson")
  lapply(Packages, library, character.only = TRUE)

 
  return (variableSelected)
  #return (myVector)
}
#blList<-"KO during media consumption at leisure,KO Consumption,Any Bev to renew my energy,Income Level"
#wlList<-"Age Nets,KO Consumption"
#variableSelected<-"KO during media consumption at leisure,KO alone or by myself,Imagery - KO is more refreshing than other soft drinks,Age Nets,Any Bev to renew my energy,Any Bev to wake me up,Any Bev to ensure i drink enough each day,KO in evening,Income Level,Regular SSD while eating dinner away,KO while eating lunch away,KO Consumption"

#display(variableSelected,blList,wlList)
