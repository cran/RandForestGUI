###############################
###############################
##  correlation of the initial random forest distances and the final random forest distances
##  Input: NA
##  Output: NA
###############################
###############################
"correlation.RF.init.vs.fitted"<-function(){
prox_calc1<-function(prox){
prox<-sqrt(1-prox)
prox[prox<=0] <- 0.0000000000000000000000000000000001
prox=as.dist(prox)
return(prox)}
dis_init=prox_calc1(prox=SRF.init.prox.raw)
dis_fitted=prox_calc1(prox=SRF.fitted.prox.raw)
z<-cor.test(dis_init,dis_fitted)
fig_par(typee=1)
plot(dis_init,dis_fitted,xlab=paste("Initial random forest with",ncol(mat.analyse.RF),"taxonomic units"),ylab=paste("Random forest after selection of the",ncol(mat.analyse.RF.fitted),"best taxonomic units"),main=paste("Dissimilarities of the initial random forest vs the random forest with the",ncol(mat.analyse.RF.fitted), "best probes"))
print(paste("Determination coefficient R2",(z$estimate)^2))
print(paste("p value", z$p.value))
correlation<<-c(c((z$estimate)^2),z$p.value)
}