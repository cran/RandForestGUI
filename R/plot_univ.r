###############################
###############################
##  plot different univariate test on the selected probes for the final random forest
##  Input: NA
##  Output: NA
###############################
###############################
"plot_univ"<-function(){
###############"""
##calcul du tableau des moyennes
#################
dev_delete()
tablee<-function(){
probee<<-sort(SRF_ini_myvarimp_selected)
finn<-matrix(nc=length(levels(factt))*2,nr=length(probee));
rownames(finn)<-names(probee)
colnames(finn)<-c(paste("mean",levels(factt)),paste("sd",levels(factt)))
for (i in 1:length(probee)){
val<-which(colnames(mat.analyse.RF.fitted)==names(probee)[i])
s=split(mat.analyse.RF.fitted[,val],factt)
z<-0
for (k in 1: length(s)){
z<-z+1
finn[i,z]<-mean(s[[k]])
}
for (k in 1: length(s)){
z<-z+1
finn[i,z]<-sd(s[[k]])
}
}
SRF_ini_mean_probe<<-finn
print(paste("Mean and standard deviation of the",names(fact.RF)[nf7],"categorical variable"))
print(SRF_ini_mean_probe)
}
###################"
###fais le barplot et boxplot
######################
barplott<-function(){
a<-function(i,mean){
x11()
size<-ncol(mean)
z=apply(t(mean),2,function(x){x/sum(x)})
z<-z[c(i,1:size)[-c(i+1)],]
fig_par(typee=1)
barplot(z[,order(z[1,],decreasing=TRUE)],ylab="Relative intensity (from 0 to 1)",legend = rownames(z),col=c(1:size)[c(i,1:size)[-c(i+1)]],las=2,cex.axis=0.7,border="white")
#require(RColorBrewer)
# myp <- brewer.pal(9,"Reds" )
 #myp<-c("white",myp)
 #x11();heatmap(z[,order(z[1,],decreasing=T)],Colv=NA,col = myp)
}
fig_par(typee=1)
bartab<-SRF_ini_mean_probe[,c(1:c(length(levels(factt))))]
for(z in 1:dim(bartab)[2]){
a(i=z,mean=bartab)}
##lboxplot
x11()
val<-c(mat.analyse.RF.fitted)
##les facteurs
aa<-vector("list",ncol(mat.analyse.RF.fitted))
for (i in 1:ncol(mat.analyse.RF.fitted)){
aa[[i]]<-paste(colnames(mat.analyse.RF.fitted)[i],factt,sep="")
}
aa<-unlist(aa);aa<-factor(aa)
##le boxplot
fig_par(typee=1)
boxplot(val~aa,cex.axis=1,las=2,ylab="Intensity (from 0 to 1)",col=rep(1:length(levels(factt)),ncol(mat.analyse.RF.fitted)))
}
################
###plot circle
####################""
circleplot<-function(){
k=0
x11()
for (j in 1: ceiling(length(probee)/4)){
x11();par(mfrow=c(2,2))
for (i in 1:4){
k=k+1
if(k<= length(probee)) {
n=names(probee)[k]
a=c(round(decostand(mat.analyse.RF[,which(colnames(mat.analyse.RF)==n)],method="range")*5,digit=1)+0.5)
fig_par(typee=1)
plot(SRF_fitted_mds$points[,1],SRF_fitted_mds$points[,2],xlab="Dimension 1",ylab="Dimension 2",cex=a,col=factt,main=paste("MDS plot with",n,sep="   "))
text(SRF_fitted_mds$points[,1],SRF_fitted_mds$points[,2],factt,cex=0.7,pos=1)
}
}}
 }
################"""
##mean + sd plot
#################
meansd<-function(){
SRF_ini_myvarimp_selected<-sort(SRF_ini_myvarimp_selected)
##identification des colonnes de mat.analyse
a<-vector(length=length(SRF_ini_myvarimp_selected))
for (i in 1:length(SRF_ini_myvarimp_selected)){
a[i]<- which(colnames(mat.analyse.RF)==names(SRF_ini_myvarimp_selected)[i])}
##calcul des moy, ciw et lim (du graph) de chaque sonde
finn<-vector("list", length(SRF_ini_myvarimp_selected))
names(finn)<-colnames(mat.analyse.RF)[a]
for (i in 1:length(SRF_ini_myvarimp_selected)){
ss=split(mat.analyse.RF[,a[i]],factt)
meann=unlist(lapply(ss,mean))
lengthh=unlist(lapply(ss,length))
sdd=unlist(lapply(ss,sd))
ciw<-vector(length=length(ss))
for (j in 1:length(ss)){ ciw[j] <- qt(0.975, lengthh[j]) * sdd[j]/ sqrt(lengthh[j])}
limy<-meann;limy[]<-NA
limy[1:2]=c(min(c(meann+ciw,meann-ciw)),max(c(meann+ciw,meann-ciw)))
if(sum(mat.analyse.RF[,a[i]])!=0) ppv<-kruskal_test(mat.analyse.RF[,a[i]]~factt,distribution = approximate(B = 9999))
if(sum(mat.analyse.RF[,a[i]])!=0) ppv<-pvalue(ppv)
if(sum(mat.analyse.RF[,a[i]])==0) ppv<-NA
pv<-meann;pv[]<-NA
pv[1]<-ppv
finn[[i]]<-data.frame(meann,ciw,limy,pv)
}
##plot
x11()
i=0
for (j in 1: ceiling(length(finn)/4)){
v=rep(4,ceiling(length(finn)/4))
v[length(v)]<-c(ceiling(length(finn)/4)*4)-length(finn)
x11();par(mfrow=c(2,2))
for (z in 1:v){
i=i+1
if(i<= length(probee)) {
fig_par(typee=1)
plot(1:dim(finn[[i]])[1],finn[[i]][,1],ylim=finn[[i]][1:2,3],type="p",col=1:dim(finn[[i]])[1],xlab=names(fact.RF)[nf7],ylab="Intensity",main=paste(names(finn)[[i]],"P-value",finn[[i]][1,4]))
errbar(1:dim(finn[[i]])[1],finn[[i]][,1],col=1:dim(finn[[i]])[1], yplus=c(finn[[i]][,1]+finn[[i]][,2]), yminus=c(finn[[i]][,1]-finn[[i]][,2]),add=TRUE)
}}}}
############"
##dominance
#########""
domm<-function(){
probee<<-sort(SRF_ini_myvarimp_selected)
finn<-matrix(nc=length(levels(factt)),nr=length(probee));
rownames(finn)<-names(probee)
colnames(finn)<-c(paste("mean",levels(factt)))
for (i in 1:length(probee)){
val<-which(colnames(mat.analyse.RF.fitted)==names(probee)[i])
s=split(mat.analyse.RF.fitted[,val],factt)
z<-0
for (k in 1: length(s)){
z<-z+1
finn[i,z]<-mean(s[[k]])
}
}
val<-finn
##
neww<-val;neww[]<-NA
for (i in 1:ncol(val)){
for (j in 1:nrow(val)){
neww[j,i]<-round(val[j,i]/max(val[j,-i]),digit=2)}}
dominant_index<-data.frame(apply(neww,1,which.max),neww)
names(dominant_index)[1]<-"dominant class"
names(dominant_index)[2:length(dominant_index)]<-levels(factt)
##ajoute les nom des levels au lieu de level 1,2,3 etc
for (i in 1:length(levels(factt))){
 dominant_index[dominant_index==i]<-levels(factt)[i]}
##ordonne le tableau domonant index par levels
val<-vector("list",length=length(levels(factt)))
names(val)<-levels(factt)
for (i in 1:length(levels(factt))){
val[[i]]<-which(dominant_index[,1]==levels(factt)[i])}
dominant_index<-dominant_index[unlist(val),]
dominant_index<<-dominant_index
print(dominant_index) }
#####
choiceGUI("Information about top sufficient taxonomic units","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
nf7<<-nf
factt<<-fact.RF[,nf]
choiceGUI("Information about top sufficient taxonomic units","Which analysis do you want to perform ?","1=:all, 2:mean+SD table,3:mean+SD plot, 4:barplot, 5:circle plot, 6:dominance index, 7:cancel")
if(nf==1){tablee()
domm()
meansd()
circleplot()
barplott()}
if(nf==2){tablee()}
if(nf==4){barplott()}
if(nf==5){circleplot()}
if(nf==3){ meansd()}
if(nf==6){domm()}
if(nf>=7){stop()}
}