###############################
###############################
##  Choice the number of cluster and the algorithm (kmeans or gam) to perform in unsupervised random forest
##  Input: NA
##  Output: NA
###############################
###############################
"cluster_det_URF"<-function(){
fig_par(typee=1)
x11();plot(DB_index[,1],DB_index[,2],col="red",ylim=c(0,ceiling(max(na.omit(DB_index[,c(2,3)])))),main="k-means in red, PAM in blue",type="b",sub="10000 iterations, 10000 random starts for k-means",cex=0.5,ylab="Davies-Bouldin's index",xlab="Number of microbial cluster")
par(new=TRUE)
plot(DB_index[,1],DB_index[,3],ylim=c(0,ceiling(max(na.omit(DB_index[,c(2,3)])))),col="blue",type="b",cex=0.5,ylab=NA,xlab=NA)
grid(20, 5, lwd = 2)
choiceGUI("Microbial clusters determination","What algorithm do you want to use ?","1:k-means, 2:PAM (Partitioning Around Medoids), 3:cancel")
cluster_algo<<-nf
if(cluster_algo==1) cluster_algo<<-"k-means"
if(cluster_algo==2) cluster_algo<<-"PAM"
if(cluster_algo==3) stop()
k<- cluster_algo
choiceGUI("Microbial clusters determination","How many microbial clusters ?","")
nb_clust_URF<<-nf
choiceGUI("Microbial clusters determination","Which categorical variable will be serve to print label ?",paste(colnames(fact.RF),collapse=","))
factt<<-fact.RF[,nf]
if (cluster_algo=="k-means") clust_URF<<-kmeans(URF_mean,nb_clust_URF,iter.max=10000,nstart=10000)$cluster
if (cluster_algo=="PAM")  clust_URF<<-pam(URF_mean,nb_clust_URF,diss=TRUE)$clustering
fig_par(typee=1)
x11();plot(URF_mds$points[,1],URF_mds$points[,2],col=clust_URF,main=paste(nb_clust_URF,"clusters using", k, "on MDS based unsupervised random forest dissimilarities"))
text(URF_mds$points[,1],URF_mds$points[,2],factt,cex=0.7,pos=1)##
choiceGUI("Microbial clusters determination","Validation ?","1:redefine, 2:yes, 3:cancel")
if(nf==1) cluster_det_URF()
if(nf==3)  stop()
if(nf==2) {
choicefactGUI("Microbial clusters determination","What is the name of the categorical variable which corresponds to these microbial clusters ?","exemple: k-means_5_clusters")
relevels <- function(ff) {
		temp<-ff
		for (i in 1:length(ff)){temp[,i]<-factor(as.character(ff[,i])) }
		return(temp)
		}
fact.RFf<-fact.RF
fact.RF<-data.frame(fact.RFf,clust_URF)} 
names(fact.RF)<-c(names(fact.RFf),nf)
##premiere fois def cluster
#if(length(fact.RF$clust_URF)!=0){choiceGUI("Microbial clusters determination","Do you want to add this microbial clusters in the categorical variables table ?","1=no, redefine,  2=yes")##déjà def cluster
#if (nf==2) {fact.RF<-data.frame(fact.RF,clust_URF)} ##ajoute clusyer
#if (nf==1){fact.RF<-fact.RF[,-which(names(fact.RF)=="clust_URF")]
#fact.RF<-data.frame(fact.RF,clust_URF)
#} 
#}
fact.RF<-relevels(fact.RF)
fact.RF<<-fact.RF
tkdestroy(tt)
RandForestGUI()
tk2notetab.select(nb, "URF")
tkfocus(tt)
}
