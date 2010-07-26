###############################
###############################
##  Calculate the out of bag on final random forest
##  Input: NA
##  Output: NA
###############################
###############################
"OOB.RF.fitted"<-function(){
OOB<-c(1-c(sum(SRF_ini_dat_fitted$fact.temp==predict(SRF_ini_fitted))/nrow(SRF_ini_dat_fitted)))#% of correct prediction <=> Out-of-bag estimate of  error rate
SRF_ini_OOB_fitted<<-OOB
print(OOB)
}