###############################
###############################
##  Compute mds (isotonic or classical) on final random forest distances
##  Input: NA
##  Output: NA
###############################
###############################
"mds.fitted.RF"<-function(){
temp<-function(){
if (exists("SRF_fitted_mds")) {choiceGUI("mds plot","Are you sure you want to change mds","no=1, yes=2")
if(nf==1) stop()  }
choiceGUI("mds plot","choose your mds method","1:isotonic advised, 2:classical, 3:sammon")
SRF_fitted_mds.type<<-nf
if(SRF_fitted_mds.type!=2) {
  choiceGUI("mds plot","Number of random start","10 000 advised")
  SRF_fitted_nbperm<<-nf}
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
  if(SRF_fitted_mds.type==1)  tmp <- isoMDS(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), maxit = maxit, trace=FALSE)
      if(SRF_fitted_mds.type==3)  tmp <- sammon(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k),  trace=FALSE)
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
if (SRF_fitted_mds.type!=2) SRF_fitted_mds<<-best.nmds(dis=prox_calc(prox=SRF.fitted.prox.raw),k=2,itr=SRF_fitted_nbperm,maxit=100)
if (SRF_fitted_mds.type==2){
points<-cmdscale(d=prox_calc(prox=SRF.fitted.prox.raw),k=2)
stress<-"Not available for classical MDS"
SRF_fitted_mds<-list(points,stress)
names(SRF_fitted_mds)<-c("points","stress")
SRF_fitted_mds<<-SRF_fitted_mds
}


print("successfully done")
if (URF_mds.type==1) dev.off()
if(SRF_fitted_mds.type==1) SRF_fitted_mds.type<<-"Isotonic"
if(SRF_fitted_mds.type==2) SRF_fitted_mds.type<<-"Classical"
if(SRF_fitted_mds.type==3) SRF_fitted_mds.typee<<-"Sammon"
}


heatmap.temp<-function(){
x11()
prox_calc<-function(prox){
prox<-sqrt(1-prox)
prox[prox<=0] <- 0.0000000000000000000000000000000001
prox=as.dist(prox)
return(prox)}
dis=prox_calc(prox=SRF.fitted.prox.raw)
##normalise
som<-apply(mat.analyse.RF.fitted,2,sum)
aa<-mat.analyse.RF.fitted
for (i in 1:ncol(mat.analyse.RF.fitted)){
aa[,i]<-mat.analyse.RF.fitted[,i]/som[i]}
####detremine ColSideColors
myp<-SRF_ini_myvarimp_selected
for (i in 1:9){myp[as.vector(SRF_ini_myvarimp_selected)>=quantile(as.vector(SRF_ini_myvarimp_selected),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Greys" )[i]}
myp[as.vector(SRF_ini_myvarimp_selected)<=quantile(as.vector(SRF_ini_myvarimp_selected),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Greys" )[1]
####RowSideColors
colo<-fact.RF[,which(names(fact.RF)==fact.supRF.fitted)]
colo<-as.numeric(colo)
colo<-palette()[colo]
###heatmap
dd<-c("ward","single","complete","average","mcquitty","median","centroid")
choiceGUI("Heatmap","What is your tree algorithm ?","1:ward, 2:single,3:complete,4:average (UPGMA), 5:mcquitty, 6:median, 7:centroid")
tree_algo_fitted<<-dd[nf]
###det des colours
a<-heat.colors(100)
b<-a;b[]<-NA#ici on inverse du blanc au rouge 
for (i in 1:length(a)){b[i]<-a[length(a)-i+1]}#ici on inverse du blanc au rouge
zere<-hclust(dist(t(aa)), method=tree_algo_fitted)$order
heatmap(aa, Colv= zere,Rowv=as.dendrogram(hclust(dis, method=tree_algo_fitted)),col= b,ColSideColors=myp,RowSideColors=colo)
legend_color_scale(b,subb=paste(tree_algo_fitted,"trees based on supervised random forest dissimilarities \nfor microbial profiles and Euclidian distances for taxonomic units.\n Row side colors corresponds to",names(fact.RF)[which(names(fact.RF)==fact.supRF.fitted)],"microbial clusters. \n Column side colors corresponds to the taxonomic units importances"))
}


plot_mds.RF.tempp<-function(){
URF_temp_mds<<-SRF_fitted_mds
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to print label ?",paste(colnames(fact.RF),collapse=","))
factt<<-fact.RF[,nf]
choiceGUI("Metric multidimensional scaling","Which categorical variable will be serve to plot symbol color ?",paste(colnames(fact.RF),collapse=","))
colo<-as.numeric(fact.RF[,nf])
x11()
fig_par(typee=1)
if(URF_temp_mds$stress!="Not available for classical MDS") plot(URF_temp_mds$points[,1],URF_temp_mds$points[,2],col=colo,xlab="Dimension 1", ylab="Dimension 2",main=paste(SRF_fitted_mds.type,"MDS based on random forest constrained by",fact.supRF.init,"with",length(SRF_ini_myvarimp_selected),"taxonomic units"),sub=paste(SRF_fitted_nbperm,"random starts ; stress=",round(URF_temp_mds$stress,digit=4)))
if(URF_temp_mds$stress=="Not available for classical MDS") plot(URF_temp_mds$points[,1],URF_temp_mds$points[,2],col=colo,xlab="Dimension 1", ylab="Dimension 2",main=paste(SRF_fitted_mds.type,"MDS based on random forest constrained by",fact.supRF.init,"with",length(SRF_ini_myvarimp_selected),"taxonomic units"))
text(URF_temp_mds$points[,1],URF_temp_mds$points[,2],factt,cex=0.7,pos=1)
if(URF_temp_mds$stress!="Not available for classical MDS") print(paste("stress",round(URF_temp_mds$stress,digit=4)))
}


choiceGUI("Plot supervised random forest with the top sufficient taxonmic units","What kind of graph ?","1:MDS, 2:Heatmap, 3:Cancel")
nf6<<-nf
if (nf6==1){
if (exists("SRF_fitted_mds")) {choiceGUI("mds plot","What to do","1 redone mds, 2 plot mds")
if(nf==1) {temp();plot_mds.RF.tempp()  }
if(nf==2)  {plot_mds.RF.tempp() }
}
if (!exists("SRF_fitted_mds")) {temp();plot_mds.RF.tempp()}
}

if(nf6==2) {heatmap.temp()}
if(nf6==3) { stop()}
}