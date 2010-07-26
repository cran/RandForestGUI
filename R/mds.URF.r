###############################
###############################
##  Compute mds (isotonic or classical) on unsupervised random forest distances
##  Input: NA
##  Output: NA
###############################
###############################
"mds.URF"<-function(){
temp<-function(){
if (exists("URF_mds")) { choiceGUI("mds plot","Are you sure you want to change mds","no=1, yes=2")
                        if(nf==1) stop()}
choiceGUI("mds plot","choose your mds method","1:isotonic advised, 2:classical, 3:sammon")
URF_mds.type<<-nf
if(URF_mds.type!=2) {
  choiceGUI("mds plot","Number of random start","10 000 advised")
  URF_mds_nbperm<<-nf}
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
    if(URF_mds.type==1) tmp <- isoMDS(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), maxit = maxit, trace=FALSE)
    if(URF_mds.type==3) tmp <- sammon(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k),  trace=FALSE)
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
prox[prox<=0] <- 0.0000000000000000000000000000000001
prox=as.dist(prox)
return(prox)}
print("please wait")
if (URF_mds.type!=2){ URF_mds<<-best.nmds(dis=prox_calc(prox=URF_mean),k=2,itr=URF_mds_nbperm,maxit=100)}
if (URF_mds.type==2){
points<-cmdscale(d=prox_calc(prox=URF_mean),k=2)
stress<-"Not available for classical MDS"
URF_mds<-list(points,stress)
names(URF_mds)<-c("points","stress")
URF_mds<<-URF_mds
}
print("successfully done")
if (URF_mds.type==1) dev.off()
if(URF_mds.type==1) URF_mds.type<<-"Isotonic"
if(URF_mds.type==2) URF_mds.type<<-"Classical"
if(URF_mds.type==3) URF_mds.type<<-"Sammon"
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
dis=1-prox_calc(prox=URF_mean)
##normalise
zz<-mat.analyse.RF
if(exists("SRF_ini_myvarimp")){
zz<-vector(length=ncol(mat.analyse.RF))
for (i in 1:ncol(mat.analyse.RF)) {
zz[i]<-which(colnames(mat.analyse.RF)==names(SRF_ini_myvarimp)[i])}
zz<-mat.analyse.RF[,zz]}

som<-apply(zz,2,sum)
zerro<-which(som==0)
aa<-zz
for (i in 1:ncol(zz)){
aa[,i]<-zz[,i]/som[i]}
aa[,zerro]<-rep(0,nrow(aa))
####detremine ColSideColors
if(exists("SRF_ini_myvarimp")){
myp<-SRF_ini_myvarimp
for (i in 1:9){myp[as.vector(SRF_ini_myvarimp)>=quantile(as.vector(SRF_ini_myvarimp),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Greys" )[i]}
myp[as.vector(SRF_ini_myvarimp)<=quantile(as.vector(SRF_ini_myvarimp),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Greys" )[1]}
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
legend_color_scale(b,subb=paste(tree_algo,"trees based on unsupervised random forest dissimilarities \nfor microbial profiles and Euclidian distances for taxonomic units.\n Row side colors corresponds to",colnames(fact.RF)[nf3],"microbial clusters"))
}

plot_mds.temp<-function(){
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to print label ?",paste(colnames(fact.RF),collapse=","))
factt<<-fact.RF[,nf]
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to plot symbol color ?",paste(colnames(fact.RF),collapse=","))
colo<-as.numeric(fact.RF[,nf])
x11()
fig_par(typee=1)
if(URF_mds$stress=="Not available for classical MDS") {plot(URF_mds$points[,1],URF_mds$points[,2],col=colo,xlab="Dimension 1", ylab="Dimension 2",main=paste(URF_mds.type,"MultiDimensionnal Scaling based on unsupervised random forest"),sub=paste(URF_meth,"sampling method")) }
if(URF_mds$stress!="Not available for classical MDS") {plot(URF_mds$points[,1],URF_mds$points[,2],col=colo,xlab="Dimension 1", ylab="Dimension 2",main=paste(URF_mds.type,"MultiDimensionnal Scaling based on unsupervised random forest"),sub=paste(URF_meth,"sampling method;",URF_mds_nbperm,"random start ; stress=",round(URF_mds$stress,digit=4))) }
text(URF_mds$points[,1],URF_mds$points[,2],factt,cex=0.7,pos=1)
print(paste("stress",URF_mds$stress))
}

choiceGUI("Plot unsupervised random forest","What kind of graph ?","1:MDS, 2:Heatmap, 3:Cancel")
nff2<<-nf
if (nff2==3){stop()}
if (nff2==1){ 
if (exists("URF_mds")) {choiceGUI("mds plot","What to do","1 redone mds, 2 plot mds")
if(nf==1) {temp();plot_mds.temp()  }
if(nf==2)  {plot_mds.temp() }
}
if (!exists("URF_mds")) {temp();plot_mds.temp()}
}

if (nff2==2){heatmap.temp() }

} 