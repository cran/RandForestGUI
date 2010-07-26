###############################
###############################
##  Procedure to tune mtry in the unsupervised random forest
##  Input: NA
##  Output: NA
###############################
###############################
"URF_tune_mtry"<-function(){
dev_delete()

URF_tune_mtry_temp<-function(){
bestforest<-function(meth,datRF,it,mtry,nbtree){
valtemp<-1
OBB<-vector(length=it)
for ( k in 1:it){
za<-za+1
#waitGUI(za,c(it*length(val)))
index1 <- sample(c(1:(2*dim(datRF)[1])))##donne val au hasard 1 à 2* nb observation
rep1 <-  order(index1) ###donne ordre
yy<-vector(length=2*dim(datRF)[1]);yy[]<-2;yy[1:dim(datRF)[1]]<-1;yy<-factor(yy)##crée le facteur
if (meth=="addcl1") sample1 <- function(X) {sample(X, replace=TRUE) }## addcl1
if (meth=="addcl2") sample1 <- function(X)   { runif(length(X), min=min(X), max =max(X)) }## addcl2
dd<-apply(datRF,2,sample1)###permute les lignes
val<-data.frame(yy,rbind(datRF,dd))
datRFsyn<-val[index1,]
if (exists("mycforesttemp")) {rm(mycforesttemp);gc(reset=TRUE)}
mycontrols <- cforest_unbiased(ntree=nbtree, mtry=mtry, minsplit=5)
set.seed(2908)
mycforesttemp <- cforest(yy ~ ., data=datRFsyn, controls=mycontrols)
val<-c(1-c(sum(yy==predict(mycforesttemp))/length(yy)))
OBB[k]<-val
if (val<=valtemp) valtemp<-val##OOB de la meilleur foret
if (val<=valtemp) mycforest<-mycforesttemp  ##meileur foret
if (val<=valtemp) best<-k ##numero d'iteration de la meilleur foret
}
errormoy<-mean(OBB)###val OOB moyen
errorsd<-sd(OBB)
d<-list(OBB,errormoy,errorsd,valtemp,best,mycforest)
return(d)
}
#choiceGUI("Tune mtry of unsupervised random forest","Select mtry incrementation arount sqrt(nb(probe))",paste("advised",ceiling(sqrt(dim(mat.analyse.RF)[2]))))
valmoymtry<-ceiling(sqrt(dim(mat.analyse.RF)[2]))
choiceGUI("Best mtry determination","What is your mtry incrementation ?","advised 5")
val<-c(valmoymtry-nf):c(valmoymtry+nf)
URF_tune_mtry_incr<<-c(val[1],val[length(val)])
choiceGUI("Best mtry determination","How many forests ?","advised 100")
iterationnb=nf
URF_tune_mtry_forest<<-nf
choiceGUI("Best mtry determination","How many trees per forest ?","advised 1000")
nbtree<-nf
URF_tune_mtry_nbtree<<-nf
ee<-matrix(nr=length(val),nc=5);colnames(ee)<-c("OOB_mean","00B_sd","mtry","OOB_mean+SD","OOB_mean-SD");rownames(ee)<-paste("mtry",val,sep="");ee[,3]<-val
za<-0
for (i in 1:length(val)){
waitGUI(i,length(val))
a<-bestforest(meth=URF_meth,datRF=mat.analyse.RF,it=iterationnb,mtry=val[i],nbtree=nbtree)
ee[i,1]<-a[[2]]
ee[i,2]<-a[[3]]
ciw <- qt(0.975, iterationnb) * ee[,2]/ sqrt(iterationnb)
ee[,4]<-c(ee[,1]+ciw)
ee[,5]<-c(ee[,1]-ciw)
URF_mtry<-ee
URF_mtry<<-URF_mtry
saveGUI.RF(typesave=2)
print("Autosave successfully done in the current direstory")
}
dev.off()
URF_mtry<<-URF_mtry
fig_par(typee=1)
plot(URF_mtry[,3],URF_mtry[,1],type="l",ylim=c(0,1),xlab="mtry value",ylab="Out-of-bag error",main="Unsupervised random forest",sub=paste(iterationnb,"forests,", nbtree,"trees/forest", URF_meth))
errbar(URF_mtry[,3],URF_mtry[,1], yplus=URF_mtry[,5], yminus=URF_mtry[,4],add=TRUE)
tkdestroy(tt)
RandForestGUI()
tk2notetab.select(nb, "URF")
tkfocus(tt)
}
"plot_tune_mtry"<-function(){
fig_par(typee=1)
plot(URF_mtry[,3],URF_mtry[,1],type="l",ylim=c(0,1),xlab="mtry value",ylab="OOB error",main="Unsupervised conditionnal inference random forest")
errbar(URF_mtry[,3],URF_mtry[,1], yplus=URF_mtry[,5], yminus=URF_mtry[,4],add=TRUE)
}

if (exists("URF_mtry")){
choiceGUI("Best mtry determination","What do you want to do ?","1:retune the mtry,  2:plot the result")
if(nf==1)  URF_tune_mtry_temp()
if(nf==2) plot_tune_mtry()
}
if (!exists("URF_mtry")){URF_tune_mtry_temp()}
}