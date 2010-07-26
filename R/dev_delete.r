###############################
###############################
##  Delete all graphical device
##  Input: NA
##  Output: NA
###############################
###############################

dev_delete<-function(){
devv<-dev.list();if(!is.null(devv)) for (i in 1:length(devv)){dev.off(devv[i])} }