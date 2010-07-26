###############################
###############################
##  Procedure to produce color scale in heatmap
##  Input: NA
##  Output: NA
###############################
###############################
"legend_color_scale"<-function(pal,subb){
##voir plot mds rf fitted
x11()
plot(0,0,type="n",xlim=c(0,length(pal)+1),ylim=c(1,11),bty="n",xaxt="n",yaxt="n",ylab = NA, xlab = NA,axes=FALSE,sub=subb,main="Heatmap legend")
for (i in 1:length(pal)){
rect(i,5,i+1,6,col=pal[i],border=NA)
}
rect(1,5,length(pal)+1,6,col="transparent")
}