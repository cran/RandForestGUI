###############################
###############################
##  Produce heatmap
##  Input: NA
##  Output: NA
###############################
###############################
"heat.mapGUI.RF"<-function(b)
{
  tt<-tktoplevel()
	tkwm.title(tt,"Plot a heatmap")
	tkgrid(tklabel(tt,text="                                                                                         "))
	tkgrid(tklabel(tt,text="Choose the proximity index to compare microbial profiles ?"))
	index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Pearson","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dice-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
	index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=13)
	tkgrid(index1)
	tkgrid(tklabel(tt,text="Choose the algorithm to compute the hierarchical clustering of the heatmap"))
	alg<-c("Ward","Single","Complete","Average","McQuitty","Median","Centroid")
	alg1<-tkwidget(tt,"ComboBox",editable=FALSE,values=alg,width=40,height=7)
	tkgrid(alg1)
	tkgrid(tklabel(tt,text="Choose the color of the heatmap"))  
	colo<-c("Blues","Greens","Greys","Oranges","Purples","Reds","Orange/Red","Blue/Green")
	colo1<-tkwidget(tt,"ComboBox",editable=FALSE,values=colo,width=40,height=8)
	tkgrid(colo1)
	tkgrid(tklabel(tt,text="     "))
	compute.heatmap<-function()
	{
		colo <- unlist(as.numeric(tcl(colo1, "getvalue")) + 1)
		if (colo == 1) myp <- brewer.pal(9,"Blues" )
		if (colo == 2) myp <- brewer.pal(9,"Greens")
		if (colo == 3) myp <- brewer.pal(9,"Greys")
		if (colo == 4) myp <- brewer.pal(9,"Oranges")
		if (colo == 5) myp <- brewer.pal(9,"Purples")
		if (colo == 6) myp <- brewer.pal(9,"Reds")
		if (colo == 7) myp <- brewer.pal(9,"OrRd")
		if (colo == 8) myp <- brewer.pal(9,"BuGn") 
		diste <- unlist(as.numeric(tcl(index1, "getvalue")) + 1)
		if (diste == 1) index <- "euclidean"
		if (diste == 2) index <- "maximum"
		if (diste == 3) index <- "manhattan"
		if (diste == 4) index <- "canberra"
		if (diste == 5) index <- "minkowski"
		if (diste == 6) index <- "Pearson"
		if (diste == 7) index <- "bray/curtis"
		if (diste == 8) index <- "chisq"
		if (diste == 9) index <- "ruzicka"
		if (diste == 10) index <- "roberts"
		if (diste == 11) index <-"jaccard"
		if (diste == 12) index <-"sorensen"
		if (diste == 13) index <-"ochiai"
		if (diste == 14) index <-"steinhaus"
		meth <- unlist(as.numeric(tcl(alg1, "getvalue")) + 1)
		if (meth == 1) methx <- "ward"
		if (meth == 2) methx <- "single"
		if (meth == 3) methx <- "complete"
		if (meth == 4) methx <- "average"
		if (meth == 5) methx <- "mcquitty"
		if (meth == 6) methx <- "median"
		if (meth == 7) methx <- "centroid"
"newdist"<-function(mat,index)                                            
{                                                                       
	ndist=0                                                               
	if (index=="euclidean")	ndist<-dist(mat,index)                        
	if (index=="maximum")	ndist<-dist(mat,index)                          
	if (index=="manhattan") 	ndist<-dist(mat,index)                      
	if (index=="canberra")    	ndist<-dist(mat,index)                    
	if (index=="minkowski")  	ndist<-dist(mat,index)                      
	if (index=="Pearson")		ndist<-as.dist(1-abs(cor(t(mat))))            
	if (index=="bray/curtis") 	ndist<-dsvdis(mat,index)                  
	if (index=="chisq")       	ndist<-dsvdis(mat,index)                  
	if (index=="ruzicka")     	ndist<-dsvdis(mat,index)                  
	if (index=="roberts")     	ndist<-dsvdis(mat,index)                  
	if (index=="jaccard")     	ndist<-vegdist(mat,index)                 
	if (index=="sorensen")    	ndist<-dsvdis(mat,index)                  
	if (index=="ochiai") 	    	ndist<-dsvdis(mat,index)                  
	if (index=="steinhaus")   	ndist<-dsvdis(mat,index)                  
	return(ndist)                                                         
}     
if(nf==0)	heatmap(b,scale="none", Colv=NA,distfun=function(b1) as.dist(newdist(b1,index)),hclustfun=function (b1) hclust(b1, method=methx),labCol=c(""), cexRow=1,cexCol=0.1,col=myp,main=paste('method = ',index,' algorithm = ',methx))
if(nf!=0)	{
colo<-factt
colo<-as.numeric(colo)
colo<-palette()[colo]
heatmap(b, Colv=NA,distfun=function(b1) as.dist(newdist(b1,index)),hclustfun=function (b1) hclust(b1, method=methx),labCol=c(""), cexRow=1,scale="none",cexCol=0.1,col=myp,main=paste('method = ',index,' algorithm = ',methx),RowSideColors=colo)
}
}
	tt11<-tkframe(tt)
	b1<-tkbutton(tt11,text="Plot",command=compute.heatmap)
	b2<-tkbutton(tt11,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(tt11)
	tkgrid(tklabel(tt,text="   "))
	tkfocus(tt)
} 
                                                                  
                                                                        