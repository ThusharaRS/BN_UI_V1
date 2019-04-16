devtools::use_data_raw()


Mapping <-read.csv('SOB_Mapping_File.csv')

devtools::use_data(Mapping)



Data <-read.csv('RL.csv')

devtools::use_data(Data)

getwd()
setwd("data-raw")
setwd("..")
