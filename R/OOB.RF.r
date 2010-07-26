###############################
###############################
##  Calculate the out of bag on initial random forest
##  Input: NA
##  Output: NA
###############################
###############################
"OOB.RF"<-function(){
OOB<-c(1-c(sum(SRF_ini_dat$fact.temp==predict(SRF_ini))/nrow(SRF_ini_dat)))#% of correct prediction <=> Out-of-bag estimate of  error rate
SRF_ini_OOB<<-OOB
print(OOB)
}