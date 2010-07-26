###############################
###############################
##  Calculate the mean decrease in accuracy with conditionnal or unconditionnal method on the initial random forest
##  Input: NA
##  Output: NA
###############################
###############################
"MDA.RF"<-function(){
dev_delete()
temp<-function(){
choiceGUI("Taxonomic units importance","Should mean decrease accuracy be calculated in conditionnal mode ?","1:false, 2:true (advised but long), 3:cancel")
cond<<-nf
if(nf==3) {stop()}
if (nf==1) MDA_conditional<<-"Not conditional"
if (nf==2) MDA_conditional<<-"Conditional"
if (cond==1) cond<<-FALSE else(cond<<-TRUE)
print("Please wait while working")
load(file=paste(strsplit(filename.RF,".Rdata")[[1]],".iniSRF",sep=""))
SRF_ini_myvarimp<- varimp(SRF_ini,conditional=cond)
rm(SRF_ini)
gc(reset=TRUE)
names(SRF_ini_myvarimp)<-colnames(mat.analyse.RF)
SRF_ini_myvarimp<<-SRF_ini_myvarimp
SRF_ini_myvarimp<<-sort(SRF_ini_myvarimp,decreasing=TRUE)
colo=NULL
barplot(sort(SRF_ini_myvarimp),xlab=paste(MDA_conditional,"mean decrease accuracy"),main="Taxonomic units importance", col=colo,space=0.75,names.arg=rownames(sort(SRF_ini_myvarimp)), horiz=TRUE, cex.names=0.45,cex=0.45, las=1)
SRF_ini_myvarimp_selected<<-SRF_ini_myvarimp[1:which(SRF_ini_myvarimp<=abs(min(SRF_ini_myvarimp)))[1]]
print(SRF_ini_myvarimp)
print("successfully")
}
if(exists("SRF_ini_myvarimp")){
choiceGUI("Taxonomic units importance","What do you want to do ?","1:print taxonomic units importance, 2:recalculate, 3:cancel")
if(nf==3) {stop()}
if(nf==1){barplot(sort(SRF_ini_myvarimp),xlab=paste(MDA_conditional,"mean decrease accuracy"),main="Taxonomic units importance", space=0.75,names.arg=rownames(sort(SRF_ini_myvarimp)), horiz=TRUE, cex.names=0.45,cex=0.45, las=1)
  presentation<-function(vec){
  lengt<-ceiling(length(vec)/4)
  a1<-a2<-a3<-a4<-a5<-a6<-a7<-a8<-vector(length=lengt)
  a1[]<-a2[]<-a3[]<-a4[]<-a5[]<-a6[]<-a7[]<-a8[]<-""
  a1<-names(vec)[1:lengt]
  a2<-vec[1:lengt]
  a3<-names(vec)[c(lengt+1):c(lengt*2)]
  a4<-vec[c(lengt+1):c(lengt*2)]
  a5<-names(vec)[c(2*lengt+1):c(lengt*3)]
  a6<-vec[c(2*lengt+1):c(lengt*3)]
  a7[1:c(lengt-c(lengt*4-length(vec)))]<-names(vec)[c(lengt*3+1):length(vec)]
  a8[1:c(lengt-c(lengt*4-length(vec)))]<-vec[c(lengt*3+1):length(vec)]
  a<-data.frame(a1,a2,a3,a4,a5,a6,a7,a8,row.names=NULL)
  print(a)}
presentation(SRF_ini_myvarimp)}
if(nf==2){temp()}
}
if(!exists("SRF_ini_myvarimp")){temp()   }
}