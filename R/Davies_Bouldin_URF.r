###############################
###############################
##  Calculates the Davies Bouldin index on Kmeans or PAM
##  Input: NA
##  Output: NA
###############################
###############################
"Davies_Bouldin_URF"<-function(){
dev_delete()

calc<-function(){
fin<-matrix(nc=3,nr=c(dim(mat.analyse.RF)[1]-1));colnames(fin)<-c("nb clusters","DBkmeans","DBpam")
fin[,1]<-2:dim(mat.analyse.RF)[1]
print("Please wait while working.")
print("The k-means algorithm is calculated with 10000 iterations and 10000 random starts")
for (i in 2:c(dim(mat.analyse.RF)[1]-1)){
waitGUI(i,dim(mat.analyse.RF)[1])
kmeans_t=kmeans(URF_mean,i,iter.max=10000,nstart=10000)$cluster
pam_t=pam(URF_mean,i,diss=TRUE)$clustering
fin[i-1,2]<-index.DB(x=URF_mean, cl=kmeans_t)$DB
fin[i-1,3]<-index.DB(x=URF_mean, cl=pam_t)$DB
}
dev.off()
DB_index<<-fin
}
plott<-function(){
fig_par(typee=1)
x11();plot(DB_index[,1],DB_index[,2],col="red",ylim=c(0,ceiling(max(na.omit(DB_index[,c(2,3)])))),main="k-means in red, PAM in blue",type="b",sub="10000 iterations and 10000 random starts for k-means",cex=0.5,ylab="Davies-Bouldin's index",xlab="Number of microbial clusters")
par(new=TRUE)
plot(DB_index[,1],DB_index[,3],ylim=c(0,ceiling(max(na.omit(DB_index[,c(2,3)])))),col="blue",type="b",cex=0.5,ylab=NA,xlab=NA)
grid(20, 5, lwd = 2)
}


if(exists("DB_index")){
choiceGUI("Microbial clusters determination","What do you want to do ?","1:recalculate, 2:plot, 3:attribute cluster as a categorical variable, 4:cancel")
nff1<<-nf
if (nff1==1) {calc();plott();cluster_det_URF()}
if (nff1==2) {plott();cluster_det_URF()}
if (nff1==3) {cluster_det_URF()}
if(nff1==4)  stop()
}
if(!exists("DB_index")){calc();plott();cluster_det_URF()}

}