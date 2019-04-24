# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(variableSelected,blList, wlList) {
   
variableSelected<-"Regular SSD while eating dinner away,KO with friend,Imagery - KO is unique and different from other soft drinks,KO while watching tv at home,Regular SSD alone or by myself,Age Nets,Any Bev while at work,Any Bev to celebrate with others"
wlList<-"Regular SSD while eating dinner away,KO with friend,KO with friend,Imagery - KO is unique and different from other soft drinks"
blList<-"KO while watching tv at home,Age Nets,Regular SSD alone or by myself,Age Nets,Any Bev while at work,Any Bev to celebrate with others"

  myVector_WL<-unlist(strsplit(wlList,","))
  myVector_BL<-unlist(strsplit(blList,","))
  myVector_var<-unlist(strsplit(variableSelected,","))
  
  return (myVector_var)
}
