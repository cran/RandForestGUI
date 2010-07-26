###############################
###############################
##  Choose the marginal method for unsupervised random forest
##  Input: NA
##  Output: NA
###############################
###############################
"unsup.mode.RF"<-function(){
choiceGUI("Type of unsupervised mode","Choose your unsupervised mode","1:addc1 (advised), 2:addcl2")
if (nf==1) URF_meth <- "addcl1"
if (nf==2) URF_meth <- "addcl2"
URF_meth<<-URF_meth
tkdestroy(tt)
rf()
}