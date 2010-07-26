###############################
###############################
##  Compute mds (isotonic or classical) on initial random forest distances
##  Input: NA
##  Output: NA
###############################
###############################
"mds.RF"<-function(){
temp<-function(){
if (exists("SRF_ini_mds")) {choiceGUI("mds plot","Are you sure you want to change mds","no=1, yes=2")
if(nf==1) stop()}
choiceGUI("mds plot","choose your mds method","1:isotonic advised, 2:classical, 3:sammon")
SRF_init_mds.type<<-nf
if(SRF_init_mds.type!=2) {
  choiceGUI("mds plot","Number of random start","10 000 advised")
  SRF_init_nbperm<<-nf}
"best.nmds" <- function (dis,k,itr,maxit=100)
{
  require(MASS)
  dis<-as.dist(dis)
  best <- 0
  strss <- rep(0,itr)
  minstr <- 99999
  for (i in 1:itr)
  {
  waitGUI(i,itr)
  dis<-as.dist(dis)
    if(SRF_init_mds.type==1) tmp <- isoMDS(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), maxit = maxit, trace=FALSE)
    if(SRF_init_mds.type==3) tmp <- sammon(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), trace=FALSE)
    strss[i] <- tmp$stress
    if (tmp$stress < minstr)
    {
      minstr <- tmp$stress
      best <- i
      out <- tmp
    }
  }
  print(paste("best result = ",best))
  out
}
prox_calc<-function(prox){
prox<-sqrt(1-prox)
prox[prox<=0] <- 0.0000000000000000000000000000000001
prox=as.dist(prox)
return(prox)}
print("please wait")
if (SRF_init_mds.type!=2) SRF_ini_mds<<-best.nmds(dis=prox_calc(prox=SRF.init.prox.raw),k=2,itr=SRF_init_nbperm,maxit=100)
if (SRF_init_mds.type==2){
points<-cmdscale(d=prox_calc(prox=SRF.init.prox.raw),k=2)
stress<-"Not available for classical MDS"
SRF_ini_mds<-list(points,stress)
names(SRF_ini_mds)<-c("points","stress")
SRF_ini_mds<<-SRF_ini_mds
}


print("successfully done")
if (URF_mds.type==1)  dev.off()
if(SRF_init_mds.type==1) SRF_init_mds.type<<-"Isotonic"
if(SRF_init_mds.type==2) SRF_init_mds.type<<-"Classical"
if(SRF_init_mds.type==3) SRF_init_mds.type<<-"Sammon"
}



heatmap.temp<-function(){
choiceGUI("Heatmap","Which categorical variable will be serve to plot color groups ?",paste(colnames(fact.RF),collapse=","))
nf3<<-nf
factt<<-fact.RF[,nf]
x11()
prox_calc<-function(prox){
prox<-sqrt(1-prox)
prox[prox<=0] <- 0.0000000000000000000000000000000001
prox=as.dist(prox)
return(prox)}
dis=prox_calc(prox=SRF.init.prox.raw)
##normalise
zz<-mat.analyse.RF
som<-apply(zz,2,sum)
zerro<-which(som==0)
aa<-zz
for (i in 1:ncol(zz)){
aa[,i]<-zz[,i]/som[i]}
aa[,zerro]<-rep(0,nrow(aa))
####RowSideColors
colo<-factt
colo<-as.numeric(colo)
colo<-palette()[colo] 
###det des colours
a<-heat.colors(100)
b<-a;b[]<-NA#ici on inverse du blanc au rouge 
for (i in 1:length(a)){b[i]<-a[length(a)-i+1]}#ici
###heatmap
dd<-c("ward","single","complete","average","mcquitty","median","centroid")
choiceGUI("Heatmap","What is your tree algorithm ?","1:ward, 2:single,3:complete,4:average (UPGMA), 5:mcquitty, 6:median, 7:centroid")
tree_algo<<-dd[nf]
zere<-hclust(dist(t(aa)), method=tree_algo)$order
heatmap(aa, Colv= zere,Rowv=as.dendrogram(hclust(dis, method=tree_algo)),col= b,RowSideColors=colo)
legend_color_scale(b,subb=paste(tree_algo,"trees based on supervised random forest dissimilarities \nfor microbial profiles and Euclidian distances for taxonomic units.\n Row side colors corresponds to",colnames(fact.RF)[nf3],"microbial clusters"))
}


plot_mds<-function(){
URF_temp_mds<-SRF_ini_mds
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to print label ?",paste(colnames(fact.RF),collapse=","))
factt<<-fact.RF[,nf]
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to plot symbol color ?",paste(colnames(fact.RF),collapse=","))
colo<-as.numeric(fact.RF[,nf])
x11()
fig_par(typee=1)

if(URF_temp_mds$stress=="Not available for classical MDS") {plot(URF_temp_mds$points[,1],URF_temp_mds$points[,2],xlab="Dimension 1", ylab="Dimension 2",col=colo,main=paste(SRF_init_mds.type,"MDS based on random forest constrained by",fact.supRF.init))}
if(URF_temp_mds$stress!="Not available for classical MDS") {plot(URF_temp_mds$points[,1],URF_temp_mds$points[,2],xlab="Dimension 1", ylab="Dimension 2",col=colo,main=paste(SRF_init_mds.type,"MDS based on random forest constrained by",fact.supRF.init),sub=paste(SRF_init_nbperm,"random starts ; stress=",round(URF_temp_mds$stress,digit=4)))}
text(URF_temp_mds$points[,1],URF_temp_mds$points[,2],factt,cex=0.7,pos=1)
if(URF_temp_mds$stress!="Not available for classical MDS") print(paste("stress",round(URF_temp_mds$stress,digit=4)))
}



choiceGUI("Plot supervised random forest","What kind of graph ?","1:MDS, 2:Heatmap, 3:Cancel")
nf1<<-nf
if(nf1==1){


if (exists("SRF_ini_mds")) {
choiceGUI("mds plot","What to do","1 redone mds, 2 plot mds")
if(nf==1) {temp();plot_mds()  }
if(nf==2)  {plot_mds() }
}
if (!exists("SRF_ini_mds")) {temp();plot_mds()}



}
if(nf1==2){heatmap.temp()}
if(nf1==3){stop()}

}