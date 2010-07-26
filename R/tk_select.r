###############################
###############################
##  Add information on main tcl tk windows generated with the RandForestGUI function
##  Input: NA
##  Output: NA
###############################
###############################
"tk_select"<-function(){
###creation de la matrice des facteurs
select.fact<-vector("list", dim(summary(fact.RF.init))[2])
names(select.fact)<-colnames(summary(fact.RF.init))
for (i in 1:dim(summary(fact.RF.init))[2]){
temp1<-levels(fact.RF.init[,i])
temp2<-vector("list",length(levels(fact.RF.init[,i])))
for (j in 1:length(levels(fact.RF.init[,i])))
{
temp2[[j]]<-which(levels(fact.RF[,i])==levels(fact.RF.init[,i])[j])
}
for (k in 1:length(temp2)) {temp2[[k]]<-length(temp2[[k]])}
temp2<-unlist(temp2)
temp2[temp2==0]<-"no"
temp2[temp2==1]<-"yes"
f<-data.frame(temp1,temp2)
names(f)<-c(colnames(summary(fact.RF.init))[i],"Available")
select.fact[[i]]<-f
}
###Font
fontHeading <- tkfont.create(family="times",size=9,weight="bold",underline=TRUE)
fontHeading2 <- tkfont.create(family="times",size=9,weight="bold")
###partie 1
tkgrid(tklabel(tb1,text=""))
if(exists("mat.analyse.RF")) tkgrid(tklabel(tb1,text=paste(dim(mat.analyse.RF)[1],"microbial profiles of",dim(mat.analyse.RF.init)[1],"and",dim(mat.analyse.RF)[2],"taxonomic units"),font=fontHeading))
if(!exists("mat.analyse.RF")) tkgrid(tklabel(tb1,text=paste(dim(mat.analyse.RF.init)[1],"microbial profiles and",dim(mat.analyse.RF)[2],"taxonomic units"),font=fontHeading))
for (j in 1:length(select.fact)){
tkgrid(tklabel(tb1,text=""))
tkgrid(tklabel(tb1,text=paste(names(select.fact)[[j]],"available",sep="     "),font=fontHeading2))
for (i in 1:dim(select.fact[[j]])[1]){
z=paste(factor(unlist(select.fact[[j]][i,])),collapse="      ")
tkgrid(tklabel(tb1,text=z))}}
###partie 2
if(exists("mat.analyse.RF")) tkgrid(tklabel(tb2,text=paste(dim(mat.analyse.RF)[2],"taxonomic units"),font=fontHeading2))
if(!exists("mat.analyse.RF")) tkgrid(tklabel(tb2,text=paste(dim(mat.analyse.RF.init)[2],"taxonomic units"),font=fontHeading2))
if(exists("URF_meth")) tkgrid(tklabel(tb2,text="Unsupervised random forest method",font=fontHeading))
if(exists("URF_meth")) tkgrid(tklabel(tb2,text=URF_meth))
if(exists("URF_tune_mtry_incr")) tkgrid(tklabel(tb2,text="mtry determination",font=fontHeading))
tkgrid(tklabel(tb2,text=paste("Heuristic mtry : ",round(sqrt(ncol(mat.analyse.RF))))))
if(exists("URF_tune_mtry_incr")) tkgrid(tklabel(tb2,text=paste("mtry tested from",URF_tune_mtry_incr[1],"to",URF_tune_mtry_incr[2])))
if(exists("URF_tune_mtry_forest")) tkgrid(tklabel(tb2,text=paste("Number of forest :",URF_tune_mtry_forest)))
if(exists("URF_tune_mtry_nbtree")) tkgrid(tklabel(tb2,text=paste("Number of tree :",URF_tune_mtry_nbtree)))
if(exists("URF_nbforest")) tkgrid(tklabel(tb2,text="Unsupervised random forest",font=fontHeading))
if(exists("URF_nbforest")) tkgrid(tklabel(tb2,text=paste("Number of forest :",URF_nbforest)))
if(exists("URF_nbtree")) tkgrid(tklabel(tb2,text=paste("Number of tree :",URF_nbtree)))
if(exists("URF_mtryy")) tkgrid(tklabel(tb2,text=paste("mtry used :",URF_mtryy)))
if(exists("URF_mds.type")) tkgrid(tklabel(tb2,text="Multidimensional scaling (MDS)",font=fontHeading))
if(exists("URF_mds.type")) tkgrid(tklabel(tb2,text=paste("Type : ",URF_mds.type)))
if(exists("URF_mds_nbperm")) {if(URF_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb2,text=paste("Number of random starts: ",URF_mds_nbperm))) }
if(exists("URF_mds")) {if(URF_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb2,text=paste("Stress : ",round(URF_mds$stress,digit=4))))} #####################
if(exists("cluster_algo")) tkgrid(tklabel(tb2,text="Microbial clusters determination",font=fontHeading))
if(exists("cluster_algo")) tkgrid(tklabel(tb2,text=paste("Algorithm used :",cluster_algo)))
if(exists("nb_clust_URF")) tkgrid(tklabel(tb2,text=paste("Number of microbial clusters :",nb_clust_URF)))   
###partie 3
tkgrid(tklabel(tb3,text="  "))
if(exists("mat.analyse.RF")) tkgrid(tklabel(tb3,text=paste(dim(mat.analyse.RF)[2],"taxonomic units"),font=fontHeading2))
if(!exists("mat.analyse.RF")) tkgrid(tklabel(tb3,text=paste(dim(mat.analyse.RF.init)[2],"taxonomic units"),font=fontHeading2))
if(exists("SRF_ini_mtry")) tkgrid(tklabel(tb3,text="mtry determination",font=fontHeading))
tkgrid(tklabel(tb3,text=paste("Heuristic mtry : ",round(sqrt(ncol(mat.analyse.RF))))))
if(exists("SRF_ini_mtry")) tkgrid(tklabel(tb3,text=paste("mtry tested from",SRF_ini_mtry[1,2],"to",SRF_ini_mtry[nrow(SRF_ini_mtry),2])))
if(exists("SRF_ini_tunemtry_nbforest")) tkgrid(tklabel(tb3,text=paste("Number of forest :",SRF_ini_tunemtry_nbforest)))
if(exists("SRF_ini_tunemtry_nbtree")) tkgrid(tklabel(tb3,text=paste("Number of tree :",SRF_ini_tunemtry_nbtree)))
if(exists("SRF_ini_OOB")) tkgrid(tklabel(tb3,text="Supervised random forest",font=fontHeading))
if(exists("fact.supRF.init")) tkgrid(tklabel(tb3,text=paste("Categorical variable tested:",fact.supRF.init)))
if(exists("SRF.init.mtry")) tkgrid(tklabel(tb3,text=paste("mtry used :",SRF.init.mtry)))
if(exists("SRF_ini_OOB")) tkgrid(tklabel(tb3,text=paste("Out-of-bag :",round(SRF_ini_OOB,digit=4))))
if(exists("SRF_ini_nbforest")) tkgrid(tklabel(tb3,text=paste("Number of forest :",SRF_ini_nbforest)))
if(exists("SRF.init.nbtree")) tkgrid(tklabel(tb3,text=paste("Number of tree :",SRF.init.nbtree)))
if(exists("SRF_init_mds.type")) tkgrid(tklabel(tb3,text="Multidimensional scaling (MDS)",font=fontHeading))
if(exists("SRF_init_mds.type")) tkgrid(tklabel(tb3,text=paste("Type : ",SRF_init_mds.type)))
if(exists("SRF_init_nbperm")) {if(SRF_ini_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb3,text=paste("Number of random starts : ",SRF_init_nbperm)))}
if(exists("SRF_ini_mds")){ if(SRF_ini_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb3,text=paste("Stress : ",round(SRF_ini_mds$stress,digit=4))))}
tkgrid(tklabel(tb3,text="Taxonomic units importance",font=fontHeading))
if(exists("SRF_ini_myvarimp")) tkgrid(tklabel(tb3,text=MDA_conditional))
if(!exists("SRF_ini_myvarimp")) tkgrid(tklabel(tb3,text="Not performed"))
if(exists("decrease_nb_var_nbtree")) tkgrid(tklabel(tb3,text="Backward taxonomic units selection",font=fontHeading))
if(exists("decrease_nb_var_nbtree")) tkgrid(tklabel(tb3,text=paste("Number of tree: ",decrease_nb_var_nbtree)))
if(exists("decrease_nb_var_mtryincr")) tkgrid(tklabel(tb3,text=paste("mtry incrementation around heuristic mtry:",decrease_nb_var_mtryincr)))
if(exists("decrease_nb_var_nbfor_aff")) tkgrid(tklabel(tb3,text="Backward taxonomic units selection refine",font=fontHeading))
if(exists("decrease_nb_var_nbfor_aff")) tkgrid(tklabel(tb3,text=paste("Number of forest : ",decrease_nb_var_nbfor_aff)))
if(exists("decrease_nb_var_nbtree_aff")) tkgrid(tklabel(tb3,text=paste("Numer of tree : ",decrease_nb_var_nbtree_aff)))
if(exists("SRF_ini_tune_nb_var_aff_maxmin")) tkgrid(tklabel(tb3,text=paste("Range taxonomic units selection from",c(ncol(mat.analyse.RF)-SRF_ini_tune_nb_var_aff_maxmin)[2],"to",c(ncol(mat.analyse.RF)-SRF_ini_tune_nb_var_aff_maxmin)[1])))
###partie 4
tkgrid(tklabel(tb4,text="  "))
if(exists("SRF_ini_myvarimp_selected")) tkgrid(tklabel(tb4,text=paste(length(SRF_ini_myvarimp_selected),"taxonomic units selected of",length(SRF_ini_myvarimp)),font=fontHeading2))
if(exists("SRF_ini_mtry")) tkgrid(tklabel(tb4,text="mtry determination",font=fontHeading))
if(exists("mat.analyse.RF.fitted")) tkgrid(tklabel(tb4,text=paste("Heuristic mtry : ",round(sqrt(ncol(mat.analyse.RF.fitted))))))
if(exists("SRF_ini_mtry_fitted")) tkgrid(tklabel(tb4,text=paste("mtry tested from",SRF_ini_mtry_fitted[1,2],"to",SRF_ini_mtry_fitted[nrow(SRF_ini_mtry_fitted),2])))
if(exists("SRF_fit_tunemtry_nbforest")) tkgrid(tklabel(tb4,text=paste("Number of forest :",SRF_fit_tunemtry_nbforest)))
if(exists("SRF_fin_tunemtry_nbtree")) tkgrid(tklabel(tb4,text=paste("Number of tree :",SRF_fin_tunemtry_nbtree)))
if(exists("SRF_ini_OOB_fitted")) tkgrid(tklabel(tb4,text="Supervised random forest with top sufficient probes",font=fontHeading))
if(exists("fact.supRF.fitted")) tkgrid(tklabel(tb4,text=paste("Categorical variable tested:",fact.supRF.fitted)))
if(exists("SRF.fit.mtry")) tkgrid(tklabel(tb4,text=paste("mtry used :",SRF.fit.mtry)))
if(exists("SRF_ini_OOB_fitted")) tkgrid(tklabel(tb4,text=paste("Out-of-bag :",round(SRF_ini_OOB_fitted,digit=4))))
if(exists("correlation")){
if(correlation[2]<0.001) pval<-"P<0.001"
if(correlation[2]<0.01) pval<-"P<0.01"
if(correlation[2]<0.05) pval<-"P<0.05"
if(correlation[2]>=0.05) pval<-"non significant"}
if(exists("correlation")) tkgrid(tklabel(tb4,text=paste("Correlation sup. rdm forest vs sup. rdm forest with top sufficient probes :",round(correlation[1],digit=4),pval)))####
if(exists("SRF_ini_nbforest_fitted")) tkgrid(tklabel(tb4,text=paste("Number of forest :",SRF_ini_nbforest_fitted)))
if(exists("SRF.fit.nbtree")) tkgrid(tklabel(tb4,text=paste("Number of tree :",SRF.fit.nbtree)))
if(exists("SRF_fitted_mds.type")) tkgrid(tklabel(tb4,text="Multidimensional scaling (MDS)",font=fontHeading))
if(exists("SRF_fitted_mds.type")) tkgrid(tklabel(tb4,text=paste("Type : ",SRF_fitted_mds.type)))
if(exists("SRF_fitted_nbperm")){if(SRF_fitted_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb4,text=paste("Number of random starts : ",SRF_fitted_nbperm)))}
if(exists("SRF_fitted_mds")) {if(SRF_fitted_mds$stress!="Not available for classical MDS") tkgrid(tklabel(tb4,text=paste("Stress : ",round(SRF_fitted_mds$stress,digit=4)))) }
if(exists("ctree_fitted_pval")) tkgrid(tklabel(tb5,text="Decision tree",font=fontHeading))
if(exists("ctree_fitted_pval")) tkgrid(tklabel(tb5,text=paste("p value : ",ctree_fitted_pval)))

tkgrid(tklabel(tb6,text=paste("Language : ",getLanguage())))
tkgrid(tklabel(tb6,text=paste("RAM memory (Mb) allocated : ",memory.limit())))
if(is.na(colo_fig_par[1])) tkgrid(tklabel(tb6,text=paste("Background color of plot : ","white")))
if(!is.na(colo_fig_par[1])) tkgrid(tklabel(tb6,text=paste("Background color of plot : ",colo_fig_par[1])))
tk2notetab.select(nb, "Management")
}