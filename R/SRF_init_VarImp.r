###############################
###############################
##  Input: NA
##  Output: NA
###############################
###############################
"SRF_init_VarImp"<-function(){
af<<-1
go<-function(){
choiceGUI("Supervised Random Forest","threshold of significance"," ")
pval.threshold<<-nf
inf<<-names(which(pval.init<=pval.threshold))
print(paste("number of taxonomic unit :",length(inf),"out of",length(pval.init)))
choiceGUI("Supervised Random Forest","Redone,validation","1=redone, 2=validation, 3=cancel")
af<<-nf
print(af)
}
while(af==1) go()
if(af==2)
{ 
ass<-vector(length=length(inf))
for (i in 1:length(inf)){
ass[i]<-which(colnames(mat.analyse.RF)==inf[i]) }
mat.analyse.RF<<- mat.analyse.RF[,ass]
print("Sucessfully done")}
if(af==3)stop()
}